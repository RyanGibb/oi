open Cmdliner

let ( / ) = Filename.concat
[@@@warning "-32"]

let cmd =
  let run () data_dir cache_dir refresh dry_run registry toolchain target
      with_deps with_repos jobs args =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache } =
      Harness.bootstrap env cache_dir
    in
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf = Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version in
    let toolchain =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:true
        toolchain
    in
    let remote = Terms.remote_of_registry registry in
    let dune_cache_root = Oi.Cache.dune_root cache in
    (* Preserve [binary_name] (the original token) for the final
       [bin/<name>] exec lookup. Resolution proceeds solve-first,
       search-after: we first try [target] verbatim as a package
       name (with any [--with] deps included). If that solve
       produces [bin/<binary_name>] in the prefix, we're done. Only
       on miss do we consult the layer index for binary→package
       mapping. The old approach of pre-rewriting [target] via the
       index baked in a [@default/X] handle pin before any solve,
       which then overrode user constraints like
       [--with=utop.2.16.0+ox1]. *)
    let binary_name = target in
    (* [TARGET] and every [--with] token accept the
       [@handle/pkg[constraint]] shortcut. The handle is routed into
       [with_repos] so the overlay joins the solve; the stripped
       package spec takes its place; each handle_pin is then pinned
       to the overlay's version below. For [TARGET] the package name
       feeds the solve, but [binary_name] (captured above) still
       drives the final [bin/<name>] lookup. *)
    let target, with_repos, with_deps, target_pin =
      match Target.split_handle_prefix target with
      | None -> (target, with_repos, with_deps, None)
      | Some (h, pkg_spec) ->
          let pkg, user_constr = OpamFormula.atom_of_string pkg_spec in
          let pin = { Target.handle = h; pkg; user_constr } in
          ( OpamPackage.Name.to_string pkg,
            with_repos @ [ h ],
            with_deps @ [ pkg_spec ],
            Some pin )
    in
    let with_deps, with_repos, with_pins =
      Target.extract_handle_pins ~with_repos with_deps
    in
    (* URL-projects in [--with=…]: clone each URL into the pin cache,
       scan its *.opam files, and merge the contribution as pins +
       solver roots + overlays + extra_repos. *)
    let extra_deps, url_project =
      Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    let extra_constraints = Oi.Project.Script.constraints extra_deps in
    (* Resolve the cwd once; reused for project-extras loading and the
       script-file existence check below. *)
    let cwd_s, cwd = Workspace.resolved_cwd fs in
    (* Load project extras (if any *.opam in cwd). A missing/unreadable
       directory degrades to "no extras"; malformed metadata still raises
       [Error.E] so the user sees the problem. *)
    let project_extras, project_pins, project_overlays =
      match Oi.Project.load ~fs cwd_s with
      | exception Sys_error _ -> ([], [], [])
      | exception Eio.Exn.Io _ -> ([], [], [])
      | p -> (p.extra_repos, p.pins, p.overlays)
    in
    let project_extras = project_extras @ url_project.extra_repos in
    let project_pins = project_pins @ url_project.pins in
    let project_overlays = project_overlays @ url_project.overlays in
    (* Treat [@HANDLE] entries from the project's [x-repos:] as if
       they had been passed via [--with-repo]. Project-declared
       overlays go earlier in the list so CLI-supplied ones take
       priority (first-wins at repos level; later arguments stack
       atop). When [--toolchain] is set, drop any project overlays
       tagged for a different toolchain — the explicit flag wins. *)
    let project_overlays =
      Oi.Pipeline.filter_compatible_overlays ~reporepo_path:(Terms.reporepo_path ())
        ~toolchain project_overlays
    in
    let with_repos = project_overlays @ with_repos in
    let cli_extras = Target.cli_extra_repos ~fs ~sys with_repos in
    let all_extras = Target.merge_extras ~cli:cli_extras ~project:project_extras in
    (* Pin each [@handle/pkg] (from TARGET or [--with]) to whatever
       version the overlay ships, so a dev-tagged version (e.g.
       [2.0.0~dev]) that would otherwise sort below a stable repo's
       version still wins when the user explicitly asked for it. The
       overlay is cloned upfront so we can scan its [packages/] tree;
       the subsequent solve reuses the same clone. *)
    let handle_pins = Stdlib.Option.to_list target_pin @ with_pins in
    let handle_constraints =
      Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
    in
    let extra_constraints =
      OpamPackage.Name.Map.union
        (fun a _ -> a)
        handle_constraints extra_constraints
    in
    let solve_assemble_run pkg_names =
      Logs.info (fun m ->
          m "Solving for packages: %s" (String.concat ", " pkg_names));
      let names = List.map OpamPackage.Name.of_string pkg_names in
      let layer_hashes =
        Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
          ~os_key ~dry_run ~extra_repos:all_extras ~pins:project_pins ~refresh
          ?remote ?jobs ?toolchain ~constraints:extra_constraints names
      in
      Logs.info (fun m -> m "Got %d layer hashes" (List.length layer_hashes));
      let prefix =
        Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      in
      Logs.info (fun m -> m "Assembled prefix at %s" prefix);

      let bin = prefix / "bin" / binary_name in
      Logs.info (fun m -> m "Looking for binary: %s" bin);
      let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info toolchain in
      let env_vars () =
        Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
      in
      if Workspace.path_exists fs bin then begin
        Logs.info (fun m -> m "Found binary, executing");
        exit (Subprocess.run proc_mgr ~env:(env_vars ()) (bin :: args))
      end
      else
        (* Non-relocatable toolchains keep their compiler binaries
           (ocamlc, ocamlfind, ocamlbuild, ...) at the fixed
           toolchain prefix rather than in the consumer prefix —
           [Opam_ctx.create] marks those packages as already
           installed so they're not rebuilt into the consumer side.
           Look for the binary there too before declaring it
           missing. The consumer prefix's env still applies (PATH /
           OCAMLPATH / CAML_LD_LIBRARY_PATH layer the toolchain's
           [bin]/[lib] in already). *)
        let tc_bin =
          match toolchain with
          | Some (info : Oi.Toolchain.info) when not info.relocatable ->
              let p = info.install_prefix / "bin" / binary_name in
              if Workspace.path_exists fs p then Some p else None
          | _ -> None
        in
        match tc_bin with
        | Some p ->
            Logs.info (fun m ->
                m "Found %s in toolchain prefix: %s" binary_name p);
            exit (Subprocess.run proc_mgr ~env:(env_vars ()) (p :: args))
        | None ->
            (* List what binaries are available in the prefix *)
            let bin_dir = prefix / "bin" in
            (try
               let bins = Eio.Path.read_dir Eio.Path.(fs / bin_dir) in
               Logs.info (fun m ->
                   m "Available binaries in prefix: %s"
                     (String.concat ", " (List.sort String.compare bins)))
             with Eio.Exn.Io _ ->
               Logs.info (fun m -> m "No bin/ directory in prefix"));
            false
    in
    (* HTTP(S) script URLs: fetch to a fresh tmp file keeping the [.ml]
       suffix, then treat the local copy as the script path for the
       rest of the run. Run caching keys on the script's content hash,
       so the nondeterministic path doesn't defeat the build cache. *)
    let target =
      let is_url s =
        String.starts_with ~prefix:"http://" s
        || String.starts_with ~prefix:"https://" s
      in
      if is_url target then begin
        let local = Filename.temp_file "oi-script-" ".ml" in
        Logs.info (fun m -> m "Fetching %s to %s" target local);
        if
          not (D10.Sysops.Curl.fetch sys ~url:target ~dst:Eio.Path.(fs / local))
        then Oi.Error.not_found target "failed to fetch %s" target;
        local
      end
      else target
    in
    (* Only .ml files are treated as scripts *)
    if Filename.check_suffix target ".ml" then begin
      if not (Workspace.path_exists cwd target) then
        Oi.Error.not_found target "file not found: %s" target;
      (* For scripts, solve deps first to get a prefix with the compiler *)
      let all_script_deps =
        Oi.Project.Script.parse_deps_from_file ~fs target @ extra_deps
      in
      let ocaml_name = OpamPackage.Name.of_string "ocaml" in
      let dep_opam_names =
        List.filter_map
          (fun (d : Oi.Project.Script.dep) ->
            if OpamPackage.Name.equal d.name ocaml_name then None
            else Some d.name)
          all_script_deps
      in
      let constraints = Oi.Project.Script.constraints all_script_deps in
      let layer_hashes =
        if dep_opam_names = [] then []
        else
          Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
            ~os_key ~dry_run ~extra_repos:all_extras ~pins:project_pins ~refresh
            ?remote ?jobs ?toolchain ~constraints dep_opam_names
      in
      if dry_run && dep_opam_names = [] then
        (* No deps to solve, but still in dry-run mode — just exit *)
        exit 0;
      let prefix =
        Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      in
      Script_runner.run ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache
        ~data_dir ?remote target extra_deps args
    end
    else begin
      (* Include --with deps in every solve *)
      let ocaml_name = OpamPackage.Name.of_string "ocaml" in
      let extra_names =
        List.filter_map
          (fun (d : Oi.Project.Script.dep) ->
            if OpamPackage.Name.equal d.name ocaml_name then None
            else Some (Oi.Project.Script.name_s d))
          extra_deps
        @ url_project.roots
      in
      let solve_assemble_run_with pkg_names =
        solve_assemble_run (pkg_names @ extra_names)
      in
      (* Step 0a: solve [target] as a package name alongside [--with]
         deps. Catches the common case where binary and package name
         match (utop, dune, odoc, ...). The catch logs in verbose
         mode — when no later step succeeds either the user gets a
         generic "no package provides" error, and the [-v] log is
         where they find the real solver diagnostic. *)
      let try_step label f =
        try f ()
        with Oi.Error.E e ->
          Logs.info (fun m -> m "%s failed: %a" label Oi.Error.pp e);
          false
      in
      let from_target =
        try_step (Fmt.str "solve %s" target) (fun () ->
            solve_assemble_run_with [ target ])
      in
      (* Step 0b: best-effort fallback when [target] isn't an opam
         package. Solve whatever else we have (extras, toolchain
         roots, pins), assemble, and look for [bin/<target>] there.
         Catches toolchain-supplied binaries like [ocamlc]. *)
      let from_with =
        if not from_target then
          try_step "fallback solve (extras + toolchain roots)" (fun () ->
              solve_assemble_run extra_names)
        else from_target
      in
      (* Explicit [@handle/pkg] target: never silently fall through
         to the layer-index lookup. The user named the source of
         truth; substituting a different package (irmin-cli for
         irmin, say) would be wrong. *)
      if Stdlib.Option.is_some target_pin && not from_with then
        Oi.Error.not_found binary_name
          "overlay-pinned package does not provide bin/%s. Check 'oi config' \
           or the overlay's opam file."
          binary_name;
      if not from_with then begin
        (* Dash-split prefixes: "a-b-c" → ["a-b-c"; "a-b"; "a"] *)
        let dash_prefixes name =
          let parts = String.split_on_char '-' name in
          let rec aux acc prefix = function
            | [] -> List.rev acc
            | p :: rest ->
                let prefix = match prefix with "" -> p | s -> s ^ "-" ^ p in
                aux (prefix :: acc) prefix rest
          in
          List.rev (aux [] "" parts)
        in
        (* Step 1: layer-index lookup. Solving for [target] verbatim
           didn't yield [bin/<target>] — either [target] isn't a
           package name, or the package that owns the binary is
           named differently (e.g. [ocluster-admin] is shipped by
           [ocluster]). Ask the index for the provider, route the
           overlay (when present) through [@handle/pkg] so the
           overlay is added to [with_repos], then re-solve. *)
        let clk = (Eio.Stdenv.clock env :> D10.Config.clk) in
        let from_index =
          match
            Layer_index.binary_to_package ~sys ~fs ~clock:clk ~cache ~os_key ~registry
              binary_name
          with
          | Some (pkg_name, _) when pkg_name <> binary_name ->
              (* Layer index says [bin/<binary_name>] is shipped by
                 [pkg_name] (and optionally an overlay handle).
                 [solve_assemble_run_with] takes raw package names —
                 [@handle/pkg] tokens were never parsed here. The base
                 [@default] is always in scope already, and any
                 user-relevant overlay handle is in scope via
                 [--with-repo] / [x-repos] anyway, so passing just
                 [pkg_name] works for the common cases without
                 lying through an unparsed [@handle/...] string. *)
              Logs.info (fun m ->
                  m "Index: bin/%s provided by package %s" binary_name pkg_name);
              try_step (Fmt.str "solve %s" pkg_name) (fun () ->
                  solve_assemble_run_with [ pkg_name ])
          | _ -> false
        in
        if not from_index then begin
          (* Step 2: Try target name and dash-split prefixes. Skip any
             prefix already in [extra_names] (Step 0 solved that already)
             and skip any prefix that doesn't exist as a package in any
             configured repo — a missing package name cannot possibly
             provide the binary, and attempting to solve for it wastes
             a full solver run. *)
          let pin_dir =
            Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh project_pins
          in
          let packages_dirs =
            Stdlib.Option.to_list pin_dir
            @ Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras
            @ Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ()
          in
          let package_exists name =
            List.exists (fun dir -> Sys.file_exists (dir / name)) packages_dirs
          in
          let prefixes =
            dash_prefixes target
            |> List.filter (fun p -> not (List.mem p extra_names))
            |> List.filter package_exists
          in
          if prefixes = [] then
            Oi.Error.not_found target "no package provides bin/%s" target
          else begin
            Logs.info (fun m ->
                m "Trying packages: %s" (String.concat ", " prefixes));
            let found =
              List.exists
                (fun name ->
                  try_step (Fmt.str "solve %s (dash-prefix)" name) (fun () ->
                      solve_assemble_run_with [ name ]))
                prefixes
            in
            if not found then
              Oi.Error.not_found target "no package provides bin/%s" target
          end
        end
      end
    end
  in
  let target =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"TARGET"
          ~doc:
            "Either the name of a binary to run, the path to an OCaml $(b,.ml) \
             script, or an $(b,http) or $(b,https) URL pointing at a remote \
             $(b,.ml) script."
          [])
  in
  let dry_run =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print the packages that would be built, but do not fetch, \
             compile, or run anything."
          [ "n"; "dry-run" ])
  in
  let args =
    Arg.(
      value & pos_right 0 string []
      & info ~docv:"ARG"
          ~doc:
            "Arguments passed through to $(b,TARGET). Use $(b,--) to separate \
             them from $(b,oi)'s own flags."
          [])
  in
  let info =
    Cmd.info "run" ~doc:"Run an OCaml script or any opam-packaged binary"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi run) resolves the dependencies of $(b,TARGET), installs \
             them into a shared local cache, and executes $(b,TARGET) with the \
             right environment. The first invocation of a given target does \
             the work; every subsequent invocation with the same dependency \
             set reuses the cache and starts in a few milliseconds.";
          `P
            "$(b,TARGET) is one of three things: the name of a binary \
             installed by some opam package, the path to a local OCaml \
             $(b,.ml) script, or an $(b,http) or $(b,https) URL pointing at a \
             remote OCaml script.";
          `S "BINARY TARGETS";
          `P
            "When $(b,TARGET) names a binary, $(b,oi) looks up the opam \
             package that ships it and installs that package on demand. If the \
             binary name is not found directly, dash-separated prefixes are \
             tried in turn, so $(b,ocluster-admin) falls back to looking up \
             $(b,ocluster). Any packages you pass with $(b,--with) are \
             searched first, which lets you pin the provider explicitly when \
             two packages ship the same binary name.";
          `Pre
            "  oi run utop\n\
            \  oi run ocamlformat -- --help\n\
            \  oi run --with=crockford roguedoi";
          `S "SCRIPT TARGETS";
          `P
            "When $(b,TARGET) is a $(b,.ml) file, $(b,oi) reads its first line \
             to discover the dependencies, builds them together with the \
             script, and caches the result. Re-running the same script without \
             edits is almost instantaneous; editing the script invalidates the \
             cache entry and triggers a rebuild. Remote $(b,http) or \
             $(b,https) URLs are fetched on every invocation and rebuilt only \
             when the contents differ from the cached copy.";
          `P
            "Declare dependencies with an $(b,@@@opam) attribute at the top of \
             the file:";
          `Pre "  [@@@opam fmt cmdliner lwt>=5.0]";
          `P
            "Each token names an opam package. An optional version constraint \
             uses the usual relational operators ($(b,>=), $(b,>), $(b,<=), \
             $(b,<), $(b,=)). A dotted suffix picks a findlib sub-library, for \
             example $(b,ppx_deriving.show). Any package whose name starts \
             with $(b,ppx_) is wired in as a PPX preprocessor.";
          `Pre
            "  oi run my_script.ml\n\
            \  oi run my_script.ml --with=tls -- arg1 arg2\n\
            \  oi run https://gist.example.com/hello.ml";
          `S "OVERLAYS";
          `P
            "An overlay is a curated collection of opam packages pinned to \
             specific git commits (see $(b,oi repo)). Prefix a target or a \
             $(b,--with) value with $(i,@HANDLE/) to take that package from \
             the named overlay, or use bare $(i,@HANDLE) to pull the entire \
             overlay into the solve. Overlays stack, which is how you compose \
             two users' collections into one solve.";
          `Pre
            "  oi run @avsm/owntracks\n\
            \  oi run @samoht/irmin\n\
            \  oi run --with=@avsm/crockford roguedoi";
          `P
            "Add $(b,x-repos: [\"@HANDLE\"]) inside an opam file to make the \
             overlay apply automatically to every $(b,oi) command run in that \
             project. The same field also accepts plain repository URLs as an \
             unpinned escape hatch; a leading $(b,@) marks reporepo handles.";
          `S "GIT URLS";
          `P
            "Passing $(b,--with=URL) clones the repository and treats every \
             $(b,*.opam) file at its root as a pinned solver root. The \
             recognised URL schemes are $(b,http://), $(b,https://), \
             $(b,git+), $(b,git@), $(b,git://), and $(b,ssh://). Append \
             $(b,#REF) to pin a specific branch, tag, or commit.";
          `Pre
            "  oi run --with=https://github.com/owner/project.git target\n\
            \  oi run --with=git+https://example.org/foo.git#branch foo";
          `S "VERSION CONSTRAINTS";
          `P
            "Pin a $(b,--with) dependency to a specific version with \
             $(b,pkg.VERSION) or $(b,pkg=VERSION). The same relational \
             operators as the script format are accepted.";
          `Pre
            "  oi run --with=dune.3.20.0 -- dune --version\n\
            \  oi run --with=fmt>=0.9 my_script.ml";
          `S "DRY RUN";
          `P
            "The $(b,-n) and $(b,--dry-run) flags print the plan $(b,oi) would \
             execute and exit. Each package in the tree carries a tag that \
             tells you where the bytes would come from:";
          `I ("$(b,binary)", "Already present in the local cache.");
          `I ("$(b,remote)", "Available from the configured registry.");
          `I ("$(b,source)", "Would be compiled from source.");
          `I
            ( "$(b,virtual)",
              "A placeholder such as $(b,conf-pkg-config) with nothing to \
               build." );
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ dry_run $ Terms.registry $ Terms.toolchain $ target $ Terms.with_deps
      $ Terms.with_repos $ Terms.jobs $ args)
