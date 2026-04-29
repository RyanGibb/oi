open Cmdliner

let ( / ) = Filename.concat

let cmd =
  let run () data_dir cache_dir refresh dry_run registry toolchain_override
      target with_deps with_repos jobs args =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache } =
      Harness.bootstrap env cache_dir
    in
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf =
      Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    let remote = Terms.remote_of_registry registry in
    let dune_cache_root = Oi.Cache.dune_root cache in
    (* [TARGET] and every [--with] token accept the
       [@handle/pkg[constraint]] shortcut. The handle is routed into
       [with_repos] so the overlay joins the solve; the stripped
       package spec takes its place; each handle_pin is then pinned
       to the overlay's version below. *)
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
    (* After the [@handle/pkg] strip above, [target] is the bare package
       name (or the user's verbatim input if no [@] prefix was given);
       use that as the binary name we'll look for in [prefix/bin/]. *)
    let binary_name = target in
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
    (* Resolve the toolchain now that we know every [@handle] in scope:
       any [@h/pkg] target/with-dep, every [--with-repo=@h] handle, and
       every project [x-repos: @h]. The unified resolver scans these for
       implicit [x-oi-toolchain] declarations, falls back to the reporepo
       default, and hard-errors on conflict — same rulebook every command
       uses. *)
    let tc_handles =
      Target.pin_handles (Stdlib.Option.to_list target_pin @ with_pins)
      @ Target.handles_of_tokens with_repos
      @ project_overlays
      |> List.sort_uniq String.compare
    in
    let toolchain =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:true
        ~override:toolchain_override ~handles:tc_handles ()
    in
    (* Treat [@HANDLE] entries from the project's [x-repos:] as if
       they had been passed via [--with-repo]. Project-declared
       overlays go earlier in the list so CLI-supplied ones take
       priority (first-wins at repos level; later arguments stack
       atop). When [--toolchain] is set, drop any project overlays
       tagged for a different toolchain — the explicit flag wins. *)
    let project_overlays =
      Oi.Pipeline.filter_compatible_overlays
        ~reporepo_path:(Terms.reporepo_path ()) ~toolchain project_overlays
    in
    let with_repos = project_overlays @ with_repos in
    let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
    let all_extras =
      Target.merge_extras ~cli:cli_extras ~project:project_extras
    in
    (* Pin each [@handle/pkg] (from TARGET or [--with]) to whatever
       version the overlay ships, so a dev-tagged version (e.g.
       [2.0.0~dev]) that would otherwise sort below a stable repo's
       version still wins when the user explicitly asked for it. The
       overlay is cloned upfront so we can scan its [packages/] tree;
       the subsequent solve reuses the same clone. *)
    let handle_pins = Stdlib.Option.to_list target_pin @ with_pins in
    let handle_constraints =
      Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras
        handle_pins
    in
    let extra_constraints =
      OpamPackage.Name.Map.union
        (fun a _ -> a)
        handle_constraints extra_constraints
    in
    (* [bin/] contents of the layer whose package matches [want_name].
       Walks [hashes], reads each [layer.json], and lists the matching
       layer's [fs/bin/]. Empty when no such layer exists or it ships
       nothing in [bin/]. *)
    let layer_binaries ~hashes ~want_name =
      let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
      let owns_target hash =
        match
          D10.Layer.load_meta Eio.Path.(fs / layers_dir / hash / "layer.json")
        with
        | Some m -> (
            match OpamPackage.of_string_opt m.package with
            | Some p
              when OpamPackage.Name.to_string (OpamPackage.name p) = want_name
              ->
                Some hash
            | _ -> None)
        | None -> None
      in
      match List.find_map owns_target hashes with
      | None -> []
      | Some hash -> (
          try
            Eio.Path.read_dir Eio.Path.(fs / layers_dir / hash / "fs" / "bin")
            |> List.sort String.compare
          with Eio.Exn.Io _ -> [])
    in
    (* When a solve succeeds but [bin/<binary_name>] is missing, stash
       the binaries the target package's layer ships so the error site
       can include them and suggest the right [oi run --with=…]
       invocation. *)
    let unfound_bins = ref [] in
    (* Solve [pkg_names], assemble the consumer prefix, exec
       [bin/<binary_name>] if it exists; falls back to looking under a
       non-relocatable toolchain's fixed prefix before giving up.
       Returns [false] when no matching binary was found, after
       populating [unfound_bins] for the error path. Calls [exit] on
       successful exec, so a [true] return is unreachable in practice. *)
    let solve_and_exec pkg_names =
      Logs.info (fun m ->
          m "Solving for packages: %s" (String.concat ", " pkg_names));
      let names =
        List.map OpamPackage.Name.of_string pkg_names
        |> Oi.Pipeline.drop_override_compiler_roots ~override:toolchain_override
             ~toolchain
      in
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
      let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info toolchain in
      let env_vars () =
        Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
      in
      let bin = prefix / "bin" / binary_name in
      Logs.info (fun m -> m "Looking for binary: %s" bin);
      if Workspace.path_exists fs bin then begin
        Logs.info (fun m -> m "Found binary, executing");
        exit (Subprocess.run proc_mgr ~env:(env_vars ()) (bin :: args))
      end;
      (* Non-relocatable toolchains keep their compiler binaries
         (ocamlc, ocamlfind, ocamlbuild, ...) at the fixed toolchain
         prefix rather than in the consumer prefix — [Opam_ctx.create]
         marks those packages as already installed so they're not
         rebuilt into the consumer side. Look there before declaring
         the binary missing. The consumer prefix's env still applies
         (PATH / OCAMLPATH / CAML_LD_LIBRARY_PATH layer the toolchain's
         [bin]/[lib] in already). *)
      let toolchain_bin =
        match toolchain with
        | Some (info : Oi.Toolchain.info) when not info.relocatable ->
            let p = info.install_prefix / "bin" / binary_name in
            if Workspace.path_exists fs p then Some p else None
        | _ -> None
      in
      match toolchain_bin with
      | Some p ->
          Logs.info (fun m ->
              m "Found %s in toolchain prefix: %s" binary_name p);
          exit (Subprocess.run proc_mgr ~env:(env_vars ()) (p :: args))
      | None ->
          (* For [@handle/pkg], list the binaries that pkg's own layer
             ships (not the whole prefix). For plain targets we don't
             yet know which package owns the binary, so fall back to
             the whole prefix [bin/] — still a useful hint even if
             noisy. *)
          let bins =
            match target_pin with
            | Some (pin : Target.handle_pin) ->
                let want_name = OpamPackage.Name.to_string pin.pkg in
                layer_binaries ~hashes:layer_hashes ~want_name
            | None -> (
                try
                  Eio.Path.read_dir Eio.Path.(fs / prefix / "bin")
                  |> List.sort String.compare
                with Eio.Exn.Io _ -> [])
          in
          unfound_bins := bins;
          Logs.info (fun m ->
              m "Available binaries: %s"
                (if bins = [] then "(none)" else String.concat ", " bins));
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
      let dep_opam_names =
        Oi.Pipeline.drop_override_compiler_roots ~override:toolchain_override
          ~toolchain dep_opam_names
      in
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
        ~data_dir ?toolchain ?remote target extra_deps args
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
      (* Composes [solve_and_exec] with the [extra_names] every solve
         needs to include ([--with] deps + URL-supplied roots). *)
      let solve_with_extras pkg_names =
        solve_and_exec (pkg_names @ extra_names)
      in
      (* Materialise [packages_dirs] once, lazily — both step 0a's
         "is [target] a package?" precheck and step 2's dash-prefix
         search consult it, so we want to share one Pin.materialize +
         Repo.ensure_extra + Reporepo.ensure_base pass. The toolchain's
         own [info.packages_dirs] takes the place of the default base
         when set (matches what Pipeline.build does), so the precheck
         sees the same package universe the actual solve will. *)
      let packages_dirs =
        lazy
          (let pin_dir =
             Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh project_pins
           in
           Stdlib.Option.to_list pin_dir
           @ Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras
           @
           match toolchain with
           | Some (info : Oi.Toolchain.info) -> info.packages_dirs
           | None ->
               Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ())
      in
      let package_exists name =
        List.exists
          (fun dir -> Sys.file_exists (dir / name))
          (Lazy.force packages_dirs)
      in
      (* Step 0a: solve [target] as a package name alongside [--with]
         deps. Catches the common case where binary and package name
         match (utop, dune, odoc, ...). Skipped when [target] isn't a
         known package name in any configured repo — solving for it
         would just waste a solver pass and produce a confusing "no
         known implementations" diagnostic. The [@handle/pkg] case from
         a [--with=@h/pkg] token already routed [pkg] into [extra_names],
         so step 0b handles it. *)
      let try_step label f =
        try f ()
        with Oi.Error.E e ->
          Logs.info (fun m -> m "%s failed: %a" label Oi.Error.pp e);
          false
      in
      let from_target =
        if not (package_exists target) then begin
          Logs.info (fun m ->
              m "Skipping solve %s — not a package name in any configured repo"
                target);
          false
        end
        else
          try_step (Fmt.str "solve %s" target) (fun () ->
              solve_with_extras [ target ])
      in
      (* Step 0b: best-effort fallback when [target] isn't an opam
         package. Solve whatever else we have (extras, toolchain
         roots, pins), assemble, and look for [bin/<target>] there.
         Catches toolchain-supplied binaries like [ocamlc]. *)
      let from_with =
        if not from_target then
          try_step "fallback solve (extras + toolchain roots)" (fun () ->
              solve_and_exec extra_names)
        else from_target
      in
      (* Explicit [@handle/pkg] target: never silently fall through to
         the layer-index lookup. The user named the source of truth;
         substituting a different package (irmin-cli for irmin, say)
         would be wrong. The error tells them which executables the
         package does install and how to run one. *)
      let fail_overlay_pin_no_binary (pin : Target.handle_pin) =
        let qualified =
          "@" ^ pin.handle ^ "/" ^ OpamPackage.Name.to_string pin.pkg
        in
        let suggestion =
          match !unfound_bins with
          | [] ->
              " The package solved but installed no executables — check the \
               overlay's opam file."
          | bins ->
              Fmt.str
                " The package installs: %s.@,\
                 To run one of them: oi run --with=%s %s"
                (String.concat ", " bins) qualified (List.hd bins)
        in
        Oi.Error.not_found binary_name
          "%s solved but does not install bin/%s.%s" qualified binary_name
          suggestion
      in
      (match target_pin with
      | Some pin when not from_with -> fail_overlay_pin_no_binary pin
      | _ -> ());
      if not from_with then begin
        (* Dash-split prefixes, longest-first: "a-b-c" →
           ["a-b-c"; "a-b"; "a"]. Each accumulator step appends the next
           segment to the previous longest prefix. *)
        let dash_prefixes name =
          String.split_on_char '-' name
          |> List.fold_left
               (fun acc part ->
                 let prev = match acc with [] -> "" | p :: _ -> p ^ "-" in
                 (prev ^ part) :: acc)
               []
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
            Layer_index.binary_to_package ~sys ~fs ~clock:clk ~cache ~os_key
              ~registry binary_name
          with
          | Some (pkg_name, _) when pkg_name <> binary_name ->
              (* Layer index says [bin/<binary_name>] is shipped by
                 [pkg_name] (and optionally an overlay handle).
                 [solve_with_extras] takes raw package names —
                 [@handle/pkg] tokens were never parsed here. The base
                 [@default] is always in scope already, and any
                 user-relevant overlay handle is in scope via
                 [--with-repo] / [x-repos] anyway, so passing just
                 [pkg_name] works for the common cases without
                 lying through an unparsed [@handle/...] string. *)
              Logs.info (fun m ->
                  m "Index: bin/%s provided by package %s" binary_name pkg_name);
              try_step (Fmt.str "solve %s" pkg_name) (fun () ->
                  solve_with_extras [ pkg_name ])
          | _ -> false
        in
        if not from_index then begin
          (* Step 2: Try target name and dash-split prefixes. Skip any
             prefix already in [extra_names] (Step 0 solved that already)
             and skip any prefix that doesn't exist as a package in any
             configured repo — a missing package name cannot possibly
             provide the binary, and attempting to solve for it wastes
             a full solver run. Reuses the same materialisation as
             step 0a's precheck. *)
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
                      solve_with_extras [ name ]))
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
            "Resolve $(b,TARGET)'s dependencies, install them into the shared \
             cache, and run $(b,TARGET). Subsequent runs with the same dep set \
             reuse the cache.";
          `P
            "$(b,TARGET) is a binary name, a local $(b,.ml) script, or an \
             $(b,http)/$(b,https) URL pointing at a remote script.";
          `S "BINARY TARGETS";
          `P
            "$(b,oi) looks up the opam package shipping the named binary and \
             installs it. Dash-prefixes fall back: $(b,ocluster-admin) tries \
             $(b,ocluster). $(b,--with) packages are searched first.";
          `Pre
            "  oi run utop\n\
            \  oi run ocamlformat -- --help\n\
            \  oi run --with=crockford roguedoi";
          `S "SCRIPT TARGETS";
          `P
            "For $(b,.ml) scripts, $(b,oi) parses the first line for deps, \
             builds them with the script, and caches by content hash. Editing \
             the script triggers a rebuild. Remote URLs are refetched on every \
             invocation and rebuilt only when contents change.";
          `P "Declare deps on the first line:";
          `Pre "  [@@@opam fmt cmdliner lwt>=5.0]";
          `P
            "Each token is an opam package, with optional version constraint \
             ($(b,>=), $(b,>), $(b,<=), $(b,<), $(b,=)) and optional findlib \
             sub-library ($(b,ppx_deriving.show)). $(b,ppx_*) packages are \
             auto-wired as PPX preprocessors.";
          `Pre
            "  oi run my_script.ml\n\
            \  oi run my_script.ml --with=tls -- arg1 arg2\n\
            \  oi run https://gist.example.com/hello.ml";
          `S "OVERLAYS";
          `P
            "Prefix $(i,@HANDLE/) on a target or $(b,--with) value to pull \
             from the named overlay; bare $(i,@HANDLE) stacks the whole \
             overlay onto the solve. See $(b,oi repo).";
          `Pre
            "  oi run @avsm/owntracks\n  oi run --with=@avsm/crockford roguedoi";
          `P
            "Add $(b,x-repos: [\"@HANDLE\"]) to a project's $(b,*.opam) to \
             apply the overlay automatically. Plain URLs are also accepted as \
             unpinned escape hatches.";
          `S "TOOLCHAIN";
          `P "Picked in order:";
          `I ("1.", "$(b,--toolchain=NAME).");
          `I
            ( "2.",
              "$(b,x-oi-toolchain) on an in-scope $(b,@HANDLE) (target, \
               $(b,--with-repo=@h), or project $(b,x-repos:)). Conflicts \
               error." );
          `I ("3.", "Reporepo's $(b,x-oi-default-toolchain).");
          `S "GIT URLS";
          `P
            "$(b,--with=URL) clones the repo and pins each root $(b,*.opam) as \
             a solver root. Schemes: $(b,http://), $(b,https://), $(b,git+), \
             $(b,git@), $(b,git://), $(b,ssh://). Append $(b,#REF) for a \
             specific commit/tag/branch.";
          `Pre
            "  oi run --with=https://github.com/owner/project.git target\n\
            \  oi run --with=git+https://example.org/foo.git#branch foo";
          `S "VERSION CONSTRAINTS";
          `P "Pin a $(b,--with) dep with $(b,pkg.VERSION) or $(b,pkg=VERSION).";
          `Pre
            "  oi run --with=dune.3.20.0 -- dune --version\n\
            \  oi run --with=fmt>=0.9 my_script.ml";
          `S "DRY RUN";
          `P "$(b,-n) prints the plan and exits. Per-package tags:";
          `I ("$(b,binary)", "Already cached.");
          `I ("$(b,remote)", "Fetchable from the configured registry.");
          `I ("$(b,source)", "Would compile from source.");
          `I
            ( "$(b,virtual)",
              "Placeholder ($(b,conf-pkg-config) etc.); nothing to build." );
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ dry_run $ Terms.registry $ Terms.toolchain $ target $ Terms.with_deps
      $ Terms.with_repos $ Terms.jobs $ args)
