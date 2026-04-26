open Cmdliner

let ( / ) = Filename.concat
(* -- plan ---------------------------------------------------------------- *)

(* Rendering helpers for [oi show]'s default succinct page. *)

(* Format the top-block "Target:" line. For a CLI-supplied target we
   print it verbatim (e.g. "utop", "@avsm/tangled"); for the
   local-project case we show the first declared package name plus a
   count when there is more than one. *)
let show_target_label ~targets ~project_deps =
  match targets with
  | [] -> (
      match project_deps with
      | [] -> "local project"
      | [ p ] -> Fmt.str "local project (%s)" p
      | many -> Fmt.str "local project (%d packages)" (List.length many))
  | _ -> String.concat " " targets

(* The overlay line is only printed when the solve actually pulled from
   an overlay. CLI-supplied [@handle/pkg] shortcuts and project
   [x-repos:] both feed into [with_repos], so we take the first handle
   we see. *)
let show_overlay_label ~with_repos =
  match with_repos with
  | [] -> None
  | h :: _ when not (Target.is_url_like h) -> Some ("@" ^ h)
  | _ -> None

(* Split the action plan's nodes into (cached, source) counts. *)
let show_counts action_plan =
  List.fold_left
    (fun (c, s) (n : Oi.Plan.node) ->
      match n.method_ with
      | Oi.Plan.Binary -> (c + 1, s)
      | Oi.Plan.Source -> (c, s + 1))
    (0, 0)
    (Oi.Plan.nodes action_plan)

(* Compute the depexts declared by every package in the plan (both
   cached and source), along with the host installation status. The
   full closure is what the old [oi depexts] reported and is the right
   answer for scripting use ("what would this need from apt if I were
   building from scratch?"). When [--os] is set the host check isn't
   meaningful and we return [None] for the status. *)
let show_depexts ~ctx ~packages_dirs ~action_plan ~os_override =
  let all_pkgs =
    List.map (fun (n : Oi.Plan.node) -> n.pkg) (Oi.Plan.nodes action_plan)
  in
  let entries =
    match os_override with
    | None -> Oi.Depexts.compute ctx ~packages_dirs all_pkgs
    | Some _ ->
        let conf = Oi.Solver.Ctx.conf ctx in
        Oi.Depexts.compute_for_conf ~conf ~packages_dirs all_pkgs
  in
  let all =
    List.fold_left
      (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
      OpamSysPkg.Set.empty entries
  in
  let status =
    if os_override <> None then None else Some (Oi.Depexts.status all)
  in
  (all, status)

(* Read the first *.opam file in [cwd] directly, for the no-target
   case where we want to surface the project's own metadata rather
   than a dependency's. Returns [(pkg, opam)] where the package name
   is taken from the filename (minus the [.opam] suffix) and the
   version is a placeholder since a project's own opam file is
   typically versionless. *)
let read_first_local_opam ~cwd =
  let entries = try Sys.readdir cwd |> Array.to_list with _ -> [] in
  let opams =
    entries
    |> List.filter (fun n -> Filename.check_suffix n ".opam")
    |> List.sort String.compare
  in
  match opams with
  | [] -> None
  | first :: _ -> (
      let path = Filename.concat cwd first in
      let name = Filename.chop_suffix first ".opam" in
      try
        let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) in
        let pkg =
          OpamPackage.create
            (OpamPackage.Name.of_string name)
            (OpamPackage.Version.of_string "dev")
        in
        Some (pkg, opam)
      with _ -> None)

(* Pick the package whose metadata we'll surface on the default info
   page. A CLI target resolves to its action-plan node. For the
   local-project case we read the project's own first *.opam file
   directly (otherwise we'd show metadata for the first
   dependency, which is misleading). Anything else falls through to
   the first plan node as a last-ditch option. *)
type show_meta_source =
  | From_node of Oi.Plan.node
  | From_project_opam of OpamPackage.t * OpamFile.OPAM.t

let show_primary_meta ~action_plan ~targets ~project_deps ~cwd =
  let find_name name =
    try Some (Oi.Plan.find action_plan (OpamPackage.Name.of_string name))
    with _ -> None
  in
  match targets with
  | first :: _ -> (
      match find_name first with Some n -> Some (From_node n) | None -> None)
  | [] -> (
      match read_first_local_opam ~cwd with
      | Some (pkg, opam) -> Some (From_project_opam (pkg, opam))
      | None -> (
          match project_deps with
          | first :: _ ->
              Stdlib.Option.map (fun n -> From_node n) (find_name first)
          | [] -> None))

(* Collapse a multi-line synopsis to its first line so the info page
   stays tidy. *)
let first_line s =
  match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i

(* Print a single optional metadata field. Skipped silently when the
   value is absent or empty. The label column is fixed at 11
   characters so all rows on the info page line up. *)
let show_meta_line label value =
  match value with
  | "" -> ()
  | v ->
      Fmt.pr "%a %s@,"
        Fmt.(styled `Bold string)
        (Fmt.str "%-11s" (label ^ ":"))
        v

(* Extract a compact, user-facing snapshot of an opam file's
   descriptive metadata fields for the info page. *)
let show_package_meta (_pkg : OpamPackage.t) (opam : OpamFile.OPAM.t) =
  let synopsis =
    Stdlib.Option.value (OpamFile.OPAM.synopsis opam) ~default:""
    |> String.trim |> first_line
  in
  let license = String.concat ", " (OpamFile.OPAM.license opam) in
  let homepage = String.concat ", " (OpamFile.OPAM.homepage opam) in
  let dev_repo =
    match OpamFile.OPAM.dev_repo opam with
    | None -> ""
    | Some u -> OpamUrl.to_string u
  in
  let maintainer = String.concat ", " (OpamFile.OPAM.maintainer opam) in
  let tags = String.concat ", " (OpamFile.OPAM.tags opam) in
  let description =
    Stdlib.Option.value (OpamFile.OPAM.descr_body opam) ~default:""
    |> String.trim
  in
  (synopsis, license, homepage, dev_repo, maintainer, tags, description)

(* List the binaries that would end up on [$PATH] when this target's
   layer is assembled into a prefix. When the layer is cached locally
   we scan [layers/<os_key>/<hash>/fs/bin] and [fs/sbin] directly;
   that's cheaper than a sqlite query and also works for layers the
   index doesn't cover (fresh builds that haven't been re-indexed
   yet). Returns [[]] for a layer that hasn't been built, for a
   purely library package, or when the fs/ tree is missing. *)
let show_package_binaries ~cache_root ~os_key ~layer_hash =
  let layer_dir = cache_root / "layers" / os_key / layer_hash / "fs" in
  let scan sub =
    let dir = layer_dir / sub in
    if not (Sys.file_exists dir) then []
    else try Sys.readdir dir |> Array.to_list with _ -> []
  in
  let bins = scan "bin" @ scan "sbin" in
  List.sort_uniq String.compare bins

(* Collect the (handle, version, url) tuples the user would want to
   see on the info page: when a toolchain is active, its overlay
   chain (e.g. [oxcaml + default]); otherwise the default base
   chain (relocatable / default). Plus any overlays named
   explicitly in [with_repos], in that order, deduplicated by
   handle. *)
let show_repositories ?toolchain ~with_repos () =
  let entries =
    try Oi.Source.Reporepo.load ~path:(Terms.reporepo_path ())
    with Oi.Error.E _ -> []
  in
  let base_handles =
    match toolchain with
    | Some (info : Oi.Toolchain.info) -> info.handle :: info.dep_handles
    | None ->
        Oi.Source.Reporepo.base_entries ()
        |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
  in
  let extra_handles = List.filter (fun h -> not (Target.is_url_like h)) with_repos in
  let all = base_handles @ extra_handles |> List.sort_uniq String.compare in
  let ordered =
    let seen = Hashtbl.create 4 in
    let push acc h =
      if List.mem h all && not (Hashtbl.mem seen h) then begin
        Hashtbl.add seen h ();
        h :: acc
      end
      else acc
    in
    let acc = List.fold_left push [] base_handles in
    let acc = List.fold_left push acc extra_handles in
    List.rev acc
  in
  List.filter_map
    (fun h ->
      match Oi.Source.Reporepo.latest entries ~handle:h with
      | Some (e : Oi.Source.Reporepo.entry) ->
          let url = if e.commit = "" then e.url else e.url ^ "#" ^ e.commit in
          Some (h, e.version, url)
      | None -> (
          (* Toolchain overlay: not in reporepo, but we know its URL. *)
          match Oi.Toolchain.url_of ~handle:h with
          | Some url -> Some (h, "builtin", url)
          | None -> None))
    ordered

(* Render the default succinct info page. *)
let show_render_info ~target_label ~target_version ~target_opam ~overlay ~os_key
    ~ocaml_version ~n_cached ~n_source ~all_depexts ~dep_status ~repositories
    ~binaries =
  let n_total = n_cached + n_source in
  Fmt.pr "@[<v>";
  let target_line =
    match target_version with
    | "" -> target_label
    | v -> Fmt.str "%s %s" target_label v
  in
  show_meta_line "Target" target_line;
  let description =
    match target_opam with
    | None -> ""
    | Some (pkg, opam) ->
        let synopsis, license, homepage, dev_repo, maintainer, tags, description
            =
          show_package_meta pkg opam
        in
        show_meta_line "Synopsis" synopsis;
        show_meta_line "License" license;
        show_meta_line "Homepage" homepage;
        (* Only surface dev-repo when it adds information beyond the
           homepage. Many opam files repeat the same github URL for
           both, which just makes the info page noisier. *)
        if dev_repo <> homepage then show_meta_line "Source" dev_repo;
        show_meta_line "Maintainer" maintainer;
        show_meta_line "Tags" tags;
        description
  in
  (match binaries with
  | [] -> ()
  | bs -> show_meta_line "Binaries" (String.concat ", " bs));
  (match overlay with
  | None -> ()
  | Some tag ->
      show_meta_line "Overlay" (Fmt.str "%a" Fmt.(styled `Cyan string) tag));
  show_meta_line "Platform" os_key;
  show_meta_line "OCaml" ocaml_version;
  Fmt.pr "@,";
  if n_source = 0 then
    show_meta_line "Packages" (Fmt.str "%d total, all cached locally." n_total)
  else begin
    show_meta_line "Packages" (Fmt.str "%d total" n_total);
    Fmt.pr "              cached: %d@," n_cached;
    Fmt.pr "              build:  %d  (from source)@," n_source
  end;
  Fmt.pr "@,";
  (match (dep_status : Oi.Depexts.status option) with
  | _ when OpamSysPkg.Set.is_empty all_depexts ->
      show_meta_line "Depexts" "(no depexts declared)"
  | None ->
      (* [--os] set: can't tell what's installed on this host, so
         just list them all plain. *)
      let names =
        OpamSysPkg.Set.elements all_depexts |> List.map OpamSysPkg.to_string
      in
      show_meta_line "Depexts" (String.concat ", " names);
      Fmt.pr "            %a@,"
        Fmt.(styled `Faint string)
        "(host check skipped because --os is set)"
  | Some st ->
      (* Every depext declared, with the uninstalled ones marked.
         Missing tokens are styled in yellow so they stand out even
         when "(missing)" is the only textual marker. *)
      let render p =
        let name = OpamSysPkg.to_string p in
        if OpamSysPkg.Set.mem p st.missing then
          Fmt.str "%a" Fmt.(styled `Yellow string) (name ^ " (missing)")
        else name
      in
      let rendered =
        OpamSysPkg.Set.elements all_depexts
        |> List.map render |> String.concat ", "
      in
      show_meta_line "Depexts" rendered;
      if not (OpamSysPkg.Set.is_empty st.missing) then
        let missing_names =
          OpamSysPkg.Set.elements st.missing |> List.map OpamSysPkg.to_string
        in
        Fmt.pr "            %a@,"
          Fmt.(styled `Faint string)
          (Fmt.str "Run: sudo apt install %s" (String.concat " " missing_names)));
  (match repositories with
  | [] -> ()
  | rows ->
      Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Repositories:";
      (* Two columns: [@handle (version)] left-padded to the longest
         token so URLs line up. *)
      let left = List.map (fun (h, v, _) -> Fmt.str "@%s (%s)" h v) rows in
      let col = List.fold_left (fun m s -> max m (String.length s)) 0 left in
      List.iter2
        (fun (_, _, url) l ->
          Fmt.pr "  %a  %s@,"
            Fmt.(styled `Cyan string)
            (Fmt.str "%-*s" col l) url)
        rows left);
  (match description with
  | "" -> ()
  | body ->
      Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Description:";
      String.split_on_char '\n' body
      |> List.iter (fun line -> Fmt.pr "  %s@," line));
  Fmt.pr "@]@."
[@@@warning "-32"]

let cmd =
  let run () data_dir cache_dir refresh registry toolchain targets with_repos
      with_deps tree only_depexts os_override =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr = _proc_mgr; fs = fs; clock = clock; sys = sys; platform = platform; os_key = os_key; cache = cache } =
      Harness.bootstrap env cache_dir
    in
    let _ = registry in
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf_host = Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version in
    let conf =
      match os_override with
      | None -> conf_host
      | Some os -> Os_override.resolve conf_host os
    in
    let toolchain =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:false
        toolchain
    in
    let conf, tc_ctx = Oi.Pipeline.toolchain_views toolchain conf in
    (* Toolchain overlay's packages_dirs drive the consumer solve too:
       when set, they REPLACE [get_packages_dirs] rather than stack
       on top, otherwise the default flow would add [relocatable]
       whose [ocaml-base-compiler.5.5.0] conflicts with the toolchain
       pin. *)
    let tc_pkg_dirs =
      match toolchain with
      | None -> None
      | Some (info : Oi.Toolchain.info) -> Some info.packages_dirs
    in
    let cwd_s, _ = Workspace.resolved_cwd fs in
    (* No pre-rewrite of the targets: solve them as-is. The layer
       index is consulted later only when the solve doesn't yield a
       matching package — same policy as [oi run]. Pre-rewriting
       would inject a [@default/X] handle pin that overrides any
       user [--with=X.VERSION] constraint. *)
    (* One [Target.extract_handle_pins] pass handles both user-typed
       [@handle/pkg] and the rewrites we just introduced: the
       handle is routed into [with_repos], the stripped package spec
       replaces the original token, and a [handle_pin] is recorded
       so the overlay version gets pinned later. *)
    let targets, with_repos, target_pins =
      Target.extract_handle_pins ~with_repos targets
    in
    let with_deps, with_repos, with_pins =
      Target.extract_handle_pins ~with_repos with_deps
    in
    let handle_pins = target_pins @ with_pins in
    let extra_deps, url_project =
      Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    (* Only consult the local project's declarations when the user did
       not name an explicit target; otherwise [oi show pkg] inside a
       project would silently pull the project's own deps into the
       solve and produce misleading output. *)
    let project_extras, project_pins, project_overlays, project_deps =
      if targets <> [] then ([], [], [], [])
      else
        match Oi.Project.load ~fs cwd_s with
        | exception Sys_error _ -> ([], [], [], [])
        | exception Eio.Exn.Io _ -> ([], [], [], [])
        | p -> (p.extra_repos, p.pins, p.overlays, p.deps)
    in
    let project_extras = project_extras @ url_project.extra_repos in
    let project_pins = project_pins @ url_project.pins in
    let project_overlays = project_overlays @ url_project.overlays in
    let project_overlays =
      Oi.Pipeline.filter_compatible_overlays ~reporepo_path:(Terms.reporepo_path ())
        ~toolchain project_overlays
    in
    let with_repos = project_overlays @ with_repos in
    let cli_extras = Target.cli_extra_repos ~fs ~sys with_repos in
    let all_extras = Target.merge_extras ~cli:cli_extras ~project:project_extras in
    let extra_pkg_dirs =
      Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras
    in
    let pin_dir =
      Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh project_pins
    in
    let base_pkg_dirs =
      match tc_pkg_dirs with
      | Some dirs -> dirs
      | None -> Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ()
    in
    let packages_dirs =
      Stdlib.Option.to_list pin_dir @ extra_pkg_dirs @ base_pkg_dirs
    in
    let extra_constraints = Oi.Project.Script.constraints extra_deps in
    let handle_constraints =
      Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
    in
    let extra_constraints =
      OpamPackage.Name.Map.union
        (fun a _ -> a)
        handle_constraints extra_constraints
    in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Project.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_deps
    in
    let url_names = List.map OpamPackage.Name.of_string url_project.roots in
    let project_dep_names = List.map OpamPackage.Name.of_string project_deps in
    let names =
      List.map OpamPackage.Name.of_string targets
      @ project_dep_names @ extra_names @ url_names
    in
    if names = [] then
      Oi.Error.config_error
        "oi show: nothing to show (no TARGET, no --with, and no *.opam files \
         in %s)"
        cwd_s;
    let cache_root = Oi.Cache.root_s cache in
    let build_prefix = cache_root / "build" / "prefix" in
    let ctx =
      Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs ~conf
        ?toolchain:tc_ctx ()
    in
    let pkgs =
      match
        Oi.Solver.solve ~fs ~cache_root ctx ~packages_dirs
          ~constraints:extra_constraints names
      with
      | Ok pkgs -> pkgs
      | Error msg ->
          (* "No known implementations at all" usually means the user
             typed a name that isn't actually an opam package - a
             common confusion when a project's display name differs
             from its package name (e.g. "ocurrent" vs [current]).
             Walk the packages_dirs for substring matches and include
             them in the error so the fix is obvious. *)
          let contains ~needle s =
            let nl = String.length needle and sl = String.length s in
            if nl = 0 || nl > sl then false
            else
              let rec loop i =
                if i + nl > sl then false
                else if String.sub s i nl = needle then true
                else loop (i + 1)
              in
              loop 0
          in
          (* Bidirectional substring match: a package is a candidate if
             either the typed target contains the package's name (e.g.
             target="ocurrent" matches package "current") or the
             package's name contains the typed target (e.g.
             target="curr" matches "current"). Case-insensitive. Both
             sides need at least four letters: shorter names (like
             [re]) otherwise match as spurious fragments of unrelated
             targets. *)
          let suggest_for target =
            let lower = String.lowercase_ascii target in
            if String.length lower < 4 then []
            else
              List.concat_map
                (fun dir -> try Sys.readdir dir |> Array.to_list with _ -> [])
                packages_dirs
              |> List.sort_uniq String.compare
              |> List.filter (fun name ->
                  let ln = String.lowercase_ascii name in
                  String.length ln >= 4
                  && ln <> lower
                  && (contains ~needle:lower ln || contains ~needle:ln lower))
          in
          let extras =
            targets
            |> List.concat_map suggest_for
            |> List.sort_uniq String.compare
          in
          let hint =
            match extras with
            | [] -> ""
            | xs ->
                let shown, rest =
                  if List.length xs > 8 then
                    (List.filteri (fun i _ -> i < 8) xs, List.length xs - 8)
                  else (xs, 0)
                in
                Fmt.str "\n\nDid you mean one of these packages?\n  %s%s"
                  (String.concat " " shown)
                  (if rest > 0 then Fmt.str " (+%d more)" rest else "")
          in
          Oi.Error.no_solution (msg ^ hint)
    in
    let d10 =
      Oi.Pipeline.make_d10 ~sys ~fs
        ~clock:(clock :> D10.Config.clk)
        ~cache ~os_key
    in
    let action_plan = Oi.Plan.build ctx ~d10 ~packages_dirs pkgs in
    if tree then begin
      let plan =
        Oi.Plan.resolve ctx ~packages_dirs ~cache_root ~os_key
          ~ocaml_version:conf.ocaml_version action_plan
      in
      Fmt.pr "%a@." Oi.Plan.pp plan
    end
    else
      let all_depexts, dep_status =
        show_depexts ~ctx ~packages_dirs ~action_plan ~os_override
      in
      if only_depexts then
        (* Always print every depext, one per line, with no status
           marking. Intended for piping into a package manager; the
           caller handles which ones are already installed. *)
        let _ = dep_status in
        OpamSysPkg.Set.iter
          (fun p -> Fmt.pr "%s@." (OpamSysPkg.to_string p))
          all_depexts
      else
        let target_label = show_target_label ~targets ~project_deps in
        let overlay = show_overlay_label ~with_repos in
        let n_cached, n_source = show_counts action_plan in
        let primary =
          show_primary_meta ~action_plan ~targets ~project_deps ~cwd:cwd_s
        in
        let target_version, target_opam, target_layer_hash =
          match primary with
          | None -> ("", None, None)
          | Some (From_node n) ->
              ( OpamPackage.Version.to_string (OpamPackage.version n.pkg),
                Some (n.pkg, n.opam),
                Some n.layer_hash )
          | Some (From_project_opam (pkg, opam)) ->
              (* Project *.opam files rarely pin a real version;
                 "dev" isn't useful on a user-facing line, so we
                 suppress the version column here. *)
              ("", Some (pkg, opam), None)
        in
        let repositories = show_repositories ?toolchain ~with_repos () in
        let binaries =
          match target_layer_hash with
          | None -> []
          | Some h -> show_package_binaries ~cache_root ~os_key ~layer_hash:h
        in
        show_render_info ~target_label ~target_version ~target_opam ~overlay
          ~os_key ~ocaml_version:conf.ocaml_version ~n_cached ~n_source
          ~all_depexts ~dep_status ~repositories ~binaries
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Opam package, binary name, or $(b,@HANDLE/PKG). Omitted: read \
             $(b,*.opam) in the current directory."
          [])
  in
  let tree =
    Arg.(
      value & flag
      & info ~doc:"Print the full per-package build plan." [ "tree" ])
  in
  let only_depexts =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print system packages, one per line, suitable for piping to \
             $(b,apt), $(b,apk), or $(b,dnf)."
          [ "only-depexts" ])
  in
  let os_override =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"OS"
          ~doc:
            "Evaluate depexts for $(b,OS) instead of the host. Accepts any tag \
             $(b,dockerfile-opam) recognises ($(b,alpine-3.23), \
             $(b,ubuntu-22.04), $(b,fedora-43), $(b,alpine), $(b,ubuntu), \
             ...). The host-installed check is skipped."
          [ "os" ])
  in
  let info =
    Cmd.info "show" ~doc:"Summarise a package's build plan and depexts"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve for $(b,TARGET) and print its metadata, package count, \
             reporepo pins, and declared system dependencies. No sources are \
             fetched and no builds run.";
          `P "With no $(b,TARGET), reads $(b,*.opam) in the current directory.";
          `S "MODES";
          `I
            ( "(default)",
              "Summary page: opam metadata, overlay tag, package counts, \
               binaries, depexts with uninstalled ones marked, and the pinned \
               reporepo overlays." );
          `I
            ( "$(b,--tree)",
              "Full per-package build plan: layer hashes, source URLs, \
               resolved build and install commands." );
          `I
            ( "$(b,--only-depexts)",
              "Every declared depext, one per line, no formatting. For piping \
               into a system package manager." );
          `S Manpage.s_examples;
          `Pre
            "  oi show utop\n\
            \  oi show --tree utop\n\
            \  sudo apt install \\$(oi show --only-depexts @avsm/tangled)\n\
            \  oi show --only-depexts --os=fedora-43";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ Terms.registry $ Terms.toolchain $ targets $ Terms.with_repos
      $ Terms.with_deps $ tree $ only_depexts $ os_override)

(* -- env ----------------------------------------------------------------- *)


(* -- tool installation --------------------------------------------------- *)


(* -- add ----------------------------------------------------------------- *)

