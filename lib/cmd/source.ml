open Cmdliner

let ( / ) = Filename.concat

(* Best-effort handle name from a packages_dir path:
   [.../v1/avsm/packages] → "avsm"; [.../packages] → "default". *)
let handle_of_packages_dir d =
  let parent = Filename.dirname d in
  let name = Filename.basename parent in
  if name = "" || name = "/" then "default" else name

(* Locate the [<pkgs_dir>/<name>/<name.version>] dir that contributed
   [pkg]'s opam file. Same lookup as [Plan.find_pkg_source_dir], but
   we need it without a resolved [Plan.t] in scope. *)
let find_pkg_source_dir ~packages_dirs (pkg : OpamPackage.t) =
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let full = OpamPackage.to_string pkg in
  List.find_map
    (fun d ->
      if Sys.file_exists (d / name / full) then Some (d, name, full) else None)
    packages_dirs

(* Read [pkg]'s [url:] block — the package's "main" source. Multi-archive
   packages (with [extra-sources:]) deliberately ignored: per the
   bundle's "pick the first one" contract, we only carry the main url
   for each package. Returns [None] for sourceless / virtual packages. *)
let main_source ~packages_dirs (pkg : OpamPackage.t) =
  match find_pkg_source_dir ~packages_dirs pkg with
  | None -> None
  | Some (d, name, full) -> (
      let path = d / name / full / "opam" in
      try
        let opam =
          OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string path))
        in
        match OpamFile.OPAM.url opam with
        | None -> None
        | Some u -> Some (OpamFile.URL.url u, OpamFile.URL.checksum u)
      with _ -> None)

(* Identity key for dedup. Tarballs key on their first declared
   checksum (content-addressed); git URLs key on the URL string
   (which carries the [#commit] pin). Two packages whose main source
   maps to the same key share one extracted directory — the second
   one's [*.opam] file lives inside the first's tree, dune walks
   both. *)
let archive_key (url : OpamUrl.t) (checksums : OpamHash.t list) =
  match checksums with
  | ck :: _ -> "h:" ^ OpamHash.to_string ck
  | [] -> "u:" ^ OpamUrl.to_string url

(* Toolchain packages aren't bundled as source, but they still need to
   land in [root.opam]'s [depends:] block so a downstream [dune pkg
   lock] / [opam install] can resolve a real OCaml. Split the solver's
   pkg list into [(consumer, toolchain)]. *)
let partition_toolchain ~(toolchain : Oi.Toolchain.info option) pkgs =
  match toolchain with
  | None -> (pkgs, [])
  | Some info ->
      List.partition (fun p -> not (OpamPackage.Set.mem p info.packages)) pkgs

(* -- Reporepo subset ---------------------------------------------------- *)

let mkdir_p d =
  let rec go d =
    if Sys.file_exists d then ()
    else begin
      go (Filename.dirname d);
      try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  go d

(* Recursive copy of [src] into [dst], hardlinking regular files when
   possible (cross-fs gracefully falls back to byte copy). Used to
   snapshot a reporepo [<pkg>/<pkg.ver>/] directory — that tree
   carries the [opam] file plus any [files/] subdir holding
   [extra-files:] patches. *)
let rec copy_tree src dst =
  Sys.readdir src
  |> Array.iter (fun name ->
      let s = src / name and d = dst / name in
      match (Unix.lstat s).Unix.st_kind with
      | Unix.S_DIR ->
          (try Unix.mkdir d 0o755
           with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
          copy_tree s d
      | Unix.S_REG when not (Sys.file_exists d) -> (
          try Unix.link s d
          with Unix.Unix_error _ ->
            let i = open_in_bin s and o = open_out_bin d in
            Fun.protect
              ~finally:(fun () ->
                close_in_noerr i;
                close_out_noerr o)
              (fun () ->
                let buf = Bytes.create 65536 in
                let rec loop () =
                  let n = input i buf 0 (Bytes.length buf) in
                  if n > 0 then begin
                    output_bytes o buf;
                    loop ()
                  end
                in
                loop ()))
      | Unix.S_LNK ->
          let target = Unix.readlink s in
          (try Unix.symlink target d
           with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
      | _ -> ())

let copy_reporepo_subset ~packages_dirs ~bundle_repo
    (pkgs : OpamPackage.t list) =
  let n = ref 0 in
  List.iter
    (fun pkg ->
      match find_pkg_source_dir ~packages_dirs pkg with
      | None -> ()
      | Some (d, name, full) ->
          let src = d / name / full in
          let dst =
            bundle_repo / "v1" / handle_of_packages_dir d / "packages" / name
            / full
          in
          if Sys.file_exists src && not (Sys.file_exists dst) then begin
            mkdir_p dst;
            copy_tree src dst;
            incr n
          end)
    pkgs;
  !n

(* -- Per-package extraction --------------------------------------------- *)

(* Extract [pkg]'s main source into [dst] via the same fetch chain
   [oi build] uses. [cache_urls] points opam at the local mirror
   (which we just warmed up), so tarballs we already fetched are
   read from disk; git URLs are cloned + checked out at the pinned
   commit. opam's [pull_tree] handles tarball strip-1, git
   clone-and-checkout, and patch application uniformly. *)
let extract_main_source ~cache_urls ~dst (pkg : OpamPackage.t)
    (url : OpamUrl.t) (checksums : OpamHash.t list) =
  let cache_dir =
    OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
  in
  let result =
    OpamRepository.pull_tree
      (OpamPackage.to_string pkg)
      ~cache_dir ~cache_urls
      (OpamFilename.Dir.of_string dst)
      checksums [ url ]
    |> OpamProcess.Job.run
  in
  match result with
  | OpamTypes.Result _ | OpamTypes.Up_to_date _ -> Ok ()
  | OpamTypes.Not_available (_, msg) -> Error msg

type install = {
  extracted : OpamPackage.t list;
      (** Got their own [<output>/<pkg>/]. *)
  duplicates : (OpamPackage.t * string) list;
      (** Share an archive with an [extracted] package; second arg is
          the basename of the directory hosting both opam files. *)
  virtuals : OpamPackage.t list;
      (** No main [url:] in the opam file — toolchain or virtual
          packages. Become [root.opam] dependencies. *)
  failures : (string * string) list;
}

(* Map every extracted / duplicate pkg name to the bundle subdirectory
   that holds its opam file. Extracted pkgs map to their own
   basename; duplicates map to the [extracted] pkg they share an
   archive with. *)
let dir_of_pkg install =
  let m = Hashtbl.create 64 in
  List.iter
    (fun pkg ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      Hashtbl.add m name name)
    install.extracted;
  List.iter
    (fun (pkg, dir) ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      Hashtbl.add m name dir)
    install.duplicates;
  m

let install_sources ~cache_urls ~output ~packages_dirs pkgs =
  (* [by_key] maps an archive's identity to the basename of the
     directory that hosts its extracted source. The first package to
     claim an archive owns the directory; later ones merely note the
     name they should pin against. *)
  let by_key : (string, string) Hashtbl.t = Hashtbl.create 64 in
  let extracted = ref [] in
  let duplicates = ref [] in
  let virtuals = ref [] in
  let failures = ref [] in
  List.iter
    (fun pkg ->
      match main_source ~packages_dirs pkg with
      | None -> virtuals := pkg :: !virtuals
      | Some (url, checksums) -> (
          let key = archive_key url checksums in
          match Hashtbl.find_opt by_key key with
          | Some dir -> duplicates := (pkg, dir) :: !duplicates
          | None ->
              let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
              let dst = output / pkg_name in
              let record_extracted () =
                Hashtbl.add by_key key pkg_name;
                extracted := pkg :: !extracted
              in
              if Sys.file_exists dst then record_extracted ()
              else
                match extract_main_source ~cache_urls ~dst pkg url checksums
                with
                | Ok () -> record_extracted ()
                | Error msg ->
                    failures := (pkg_name, msg) :: !failures))
    pkgs;
  {
    extracted = List.rev !extracted;
    duplicates = List.rev !duplicates;
    virtuals = List.rev !virtuals;
    failures = List.rev !failures;
  }

(* -- dune-project + root.opam ------------------------------------------

   The bundle is a multi-package source tree: each extracted
   subdirectory carries its own [*.opam] files, and a bundle-level
   [root.opam] declares every package the solve needed that isn't
   carried as source — toolchain packages plus virtual / sourceless
   packages. A downstream [dune pkg lock] / [opam install --deps-only]
   resolves those externally; the in-tree pins cover the rest. *)

let dune_project_contents = "(lang dune 3.20)\n"

(* A directory is dune-buildable if dune sees a project marker at its
   root. Packages whose source landed in a non-dune dir need explicit
   opam pin-depends entries — dune workspace machinery doesn't pick
   them up. *)
let dir_is_dune_buildable dir =
  Sys.file_exists (dir / "dune-project") || Sys.file_exists (dir / "dune")

let dep_line buf pkg =
  Buffer.add_string buf
    (Fmt.str "  %S {= %S}\n"
       (OpamPackage.Name.to_string (OpamPackage.name pkg))
       (OpamPackage.Version.to_string (OpamPackage.version pkg)))

(* Emit pin URLs in opam's bare-relative-path form ([./foo]) rather
   than the [file:./foo] form. The latter parses as
   [ssh://file/./foo] under [OpamUrl.of_string] (the [:] gets
   treated as a host separator); plain [./foo] is opam's
   conventional path-pin and resolves to [file:///<cwd>/foo] at
   parse time, which is what we want when the user runs
   [oi build] / [opam install] from inside the bundle dir. *)
let pin_depend_line buf pkg dir =
  Buffer.add_string buf
    (Fmt.str "  [ %S %S ]\n" (OpamPackage.to_string pkg) ("./" ^ dir))

let render_root_opam ~target_label ~externals ~non_dune_pins =
  let buf = Buffer.create 1024 in
  let pf fmt = Fmt.kstr (Buffer.add_string buf) fmt in
  pf "opam-version: \"2.0\"\n";
  pf "synopsis: \"oi source bundle for %s\"\n" target_label;
  pf "description: \"\"\"\n";
  pf "Auto-generated by [oi source]. Each subdirectory holds the\n";
  pf "extracted source of one in-tree package. Dune-buildable packages\n";
  pf "are picked up by [dune] via the workspace [dune-project]; the\n";
  pf "[pin-depends:] block below redirects opam at non-dune packages\n";
  pf "(those whose source has no [dune-project]/[dune]) to their local\n";
  pf "directory. The [depends:] block names everything the bundle\n";
  pf "needs that isn't carried as source — toolchain compilers,\n";
  pf "virtual base packages — plus the non-dune pins for completeness.\n";
  pf "Resolve with [dune pkg lock] or [opam install --deps-only].\"\"\"\n";
  pf "depends: [\n";
  List.iter (dep_line buf) externals;
  List.iter (fun (pkg, _dir) -> dep_line buf pkg) non_dune_pins;
  pf "]\n";
  if non_dune_pins <> [] then begin
    pf "pin-depends: [\n";
    List.iter (fun (pkg, dir) -> pin_depend_line buf pkg dir) non_dune_pins;
    pf "]\n"
  end;
  Buffer.contents buf

let write_workspace_files ~output ~target_label ~externals ~non_dune_pins =
  let write path content =
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc content)
  in
  write (output / "dune-project") dune_project_contents;
  write (output / "root.opam")
    (render_root_opam ~target_label ~externals ~non_dune_pins)

(* -- Local mirror warm-up ------------------------------------------------

   The actual [pull_tree] extraction reads from opam's [cache_dir] /
   the local mirror; warm both up-front via the same
   [Source.Mirror.fetch_archives] [oi build --archives-only] uses, so
   extractions afterwards are pure CPU + disk. *)
let warm_local_mirror ~fs ~cache ~packages_dirs pkgs =
  let archives = Oi.Source.Mirror.collect_archives ~packages_dirs pkgs in
  if archives = [] then None
  else begin
    Oi.Say.step "Fetching %d source archive(s)" (List.length archives);
    let last = ref "" in
    let on_progress ~fetched ~total ~current =
      let msg =
        match current with
        | None -> Fmt.str "fetched %d/%d" fetched total
        | Some c -> Fmt.str "fetched %d/%d  %s" fetched total c
      in
      if msg <> !last then begin
        last := msg;
        Oi.Say.progress msg
      end
    in
    let summary =
      Oi.Source.Mirror.fetch_archives ~fs ~cache ~on_progress archives
    in
    Oi.Say.progress_clear ();
    Some summary
  end

(* -- @handle/pkg shorthand ---------------------------------------------- *)

(* Strip [@handle/pkg] prefixes the same way [oi build] / [oi run] do:
   the handle joins [with_repos] (overlay shows up in the solve), the
   bare [pkg] takes its place in [targets], and the pkg-with-version
   spec joins [with_deps] for constraint propagation. *)
let split_handle_targets ~with_repos ~with_deps targets =
  let targets, with_repos, with_deps =
    List.fold_left
      (fun (ts, repos, deps) t ->
        match Target.split_handle_prefix t with
        | None -> (t :: ts, repos, deps)
        | Some (h, pkg_spec) ->
            let pkg, _ = OpamFormula.atom_of_string pkg_spec in
            ( OpamPackage.Name.to_string pkg :: ts,
              repos @ [ h ],
              deps @ [ pkg_spec ] ))
      ([], with_repos, with_deps) targets
  in
  (List.rev targets, with_repos, with_deps)

(* -- Main flow ---------------------------------------------------------- *)

let cmd =
  let run () data_dir cache_dir refresh registry use_registry with_repos
      with_deps _jobs toolchain_override targets output =
    Harness.run @@ fun ~sw env ->
    let { Harness.fs; sys; platform; cache; _ } =
      Harness.bootstrap ~sw env cache_dir
    in
    if output = "" then Oi.Error.config_error "oi source: -o DIR is required";
    if targets = [] then
      Oi.Error.config_error "oi source: at least one TARGET is required";
    (* [Eio.Path.mkdirs] mishandles relative paths whose split-parent
       is the empty string (it ends up calling [mkdirat] with [""]).
       Resolve to absolute against cwd before any filesystem op. *)
    let output =
      if Filename.is_relative output then
        Filename.concat (Sys.getcwd ()) output
      else output
    in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / output);
    let bundle_repo = output / "reporepo" in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / bundle_repo);
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let _ = Terms.remotes_of ~url:registry ~mode:use_registry in
    let conf =
      Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    let targets, with_repos, with_deps =
      split_handle_targets ~with_repos ~with_deps targets
    in
    let extra_deps, url_project =
      Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    let extra_constraints = Oi.Project.Script.constraints extra_deps in
    let with_repos =
      with_repos
      @ Oi.Pipeline.filter_compatible_overlays
          ~reporepo_path:(Terms.reporepo_path ()) ~toolchain:None
          url_project.overlays
    in
    let cli_extras =
      Target.cli_extra_repos ~fs ~sys ?toolchain:None with_repos
    in
    let all_extras =
      Target.merge_extras ~cli:cli_extras ~project:url_project.extra_repos
    in
    let toolchain =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:false
        ~override:toolchain_override
        ~handles:(List.sort_uniq String.compare with_repos)
        ()
    in
    let conf, tc_ctx = Oi.Pipeline.toolchain_views toolchain conf in
    let packages_dirs =
      Stdlib.Option.to_list
        (Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh url_project.pins)
      @ Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras
      @ (match toolchain with
        | Some (info : Oi.Toolchain.info) -> info.packages_dirs
        | None -> Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ())
    in
    let cache_root = Oi.Cache.root_s cache in
    let ctx =
      Oi.Solver.Ctx.create
        ~prefix:(cache_root / "build" / "prefix")
        ~packages_dirs ~conf ?toolchain:tc_ctx ()
    in
    let names =
      List.map OpamPackage.Name.of_string targets
      @ List.map OpamPackage.Name.of_string url_project.roots
      |> Oi.Pipeline.drop_override_compiler_roots ~override:toolchain_override
           ~toolchain
    in
    Oi.Say.step "Solving %d target(s) (+test, +doc)" (List.length names);
    (* Test / doc deps for the solve roots (not transitive) so the
       bundle is complete enough to run a target's tests + docs.
       [opam install --with-test --with-doc foo] semantics. The pair
       enters the solve cache key so the bundle solve doesn't
       overwrite a parallel base-only [oi build] solve. *)
    let test_doc_roots = OpamPackage.Name.Set.of_list names in
    let pkgs =
      match
        Oi.Solver.solve ~test:test_doc_roots ~doc:test_doc_roots ~fs
          ~cache_root ctx ~packages_dirs ~constraints:extra_constraints names
      with
      | Ok pkgs -> pkgs
      | Error msg -> Oi.Error.no_solution msg
    in
    let consumer_pkgs, toolchain_pkgs =
      partition_toolchain ~toolchain pkgs
    in
    Oi.Say.info "%d package(s) in solve closure (%d toolchain)"
      (List.length consumer_pkgs) (List.length toolchain_pkgs);
    (match warm_local_mirror ~fs ~cache ~packages_dirs consumer_pkgs with
    | None -> ()
    | Some s ->
        List.iter
          (fun (url, msg) -> Oi.Say.warn "fetch failed %s: %s" url msg)
          s.failed);
    let cache_urls = [ Oi.Source.Mirror.url ~cache ] in
    let install =
      install_sources ~cache_urls ~output ~packages_dirs consumer_pkgs
    in
    Oi.Say.field "sources" "%d extracted, %d shared, %d virtual → %s"
      (List.length install.extracted)
      (List.length install.duplicates)
      (List.length install.virtuals)
      output;
    List.iter
      (fun (pkg, msg) -> Oi.Say.warn "extract %s: %s" pkg msg)
      install.failures;
    let n_repo =
      copy_reporepo_subset ~packages_dirs ~bundle_repo consumer_pkgs
    in
    Oi.Say.field "reporepo" "%d package dir(s) → %s" n_repo bundle_repo;
    let externals = toolchain_pkgs @ install.virtuals in
    (* Classify every extracted / duplicate package by whether its
       source dir is dune-buildable. Non-dune packages need explicit
       opam pin-depends entries — dune's workspace walk doesn't
       cover them. *)
    let dirs = dir_of_pkg install in
    let non_dune_pins =
      let in_pkgs = install.extracted @ List.map fst install.duplicates in
      List.filter_map
        (fun pkg ->
          let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
          match Hashtbl.find_opt dirs name with
          | None -> None
          | Some dir ->
              if dir_is_dune_buildable (output / dir) then None
              else Some (pkg, dir))
        in_pkgs
    in
    write_workspace_files ~output
      ~target_label:(String.concat ", " targets)
      ~externals ~non_dune_pins;
    Oi.Say.field "workspace"
      "dune-project + root.opam (%d external dep(s), %d non-dune pin(s))"
      (List.length externals) (List.length non_dune_pins);
    Oi.Say.ok "wrote source bundle to %s" output;
    if install.failures <> [] then exit 1
  in
  let output =
    Arg.(
      value & opt string ""
      & info ~docv:"DIR"
          ~doc:
            "Output directory. Receives one subdirectory per package (the \
             package's main source extracted), a $(b,reporepo/) subtree \
             with the opam files the solve referenced, and \
             $(b,dune-project) + $(b,root.opam) declaring external \
             dependencies. Created if missing; existing entries are \
             preserved."
          [ "o"; "output" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Opam package name or $(b,@HANDLE/PKG) shortcut to fetch sources \
             for. Repeatable."
          [])
  in
  let info =
    Cmd.info "source" ~doc:"Fetch sources for a target into a flat bundle"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve $(b,TARGET)'s dep closure (the same way $(b,oi build) \
             would), then extract each package's main source into \
             $(b,DIR/<pkg>/). Tarballs are unpacked, $(b,git+...) URLs are \
             cloned and checked out at the pinned commit. Packages whose \
             main source resolves to the same archive are deduplicated: \
             only the first is extracted; the rest aren't given their own \
             directory because their $(b,*.opam) file lives inside the \
             first one's tree (dune walks both).";
          `P
            "Each package's $(b,*.opam) file (and any $(b,extra-files/) \
             patches alongside) is also copied into $(b,DIR/reporepo/) so a \
             downstream offline $(b,oi build) / $(b,oi sync) has the \
             reporepo metadata.";
          `P
            "$(b,DIR/dune-project) + $(b,DIR/root.opam) make the bundle a \
             dune workspace. $(b,root.opam)'s $(b,depends:) lists every \
             package the solve wanted that isn't carried as source — \
             toolchain compilers and virtual base packages — pinned to \
             exact versions, so a downstream $(b,dune pkg lock) / \
             $(b,opam install --deps-only) reproduces the build environment.";
          `P
            "$(b,{with-test}) and $(b,{with-doc}) dependencies of the solve \
             roots are always pulled in so the bundle is complete enough to \
             $(b,dune runtest) and $(b,dune build @doc) offline.";
          `P
            "$(b,extra-sources:) blocks are skipped — only each package's \
             main $(b,url:) block is bundled.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ Terms.registry $ Terms.use_registry $ Terms.with_repos
      $ Terms.with_deps $ Terms.jobs $ Terms.toolchain $ targets $ output)
