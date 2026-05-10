open Cmdliner

let ( / ) = Filename.concat

let run_impl (c : Terms.common) refresh locked skip_local dry_run registry
    use_registry toolchain_override target with_deps with_repos jobs save_d10ir
    args =
  Harness.run @@ fun ~sw env ->
  let {
    Harness.proc_mgr;
    fs;
    clock;
    sys;
    platform;
    os_key;
    cache;
    http_session;
    _;
  } =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let data_dir = c.data_dir in
  (* [--locked] forces offline-friendly defaults so an agent that
     pre-warmed the cache fails fast on any cache miss. *)
  let refresh = refresh && not locked in
  let use_registry = if locked then Oi.Use_registry.Never else use_registry in
  let dune_cache_root = Oi.Cache.dune_root cache in
  let cache_root = Oi.Cache.root_s cache in
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
  (* Fast-path: if we've run these exact args before and the binary
       still exists, skip all expensive work (opam init, reporepo,
       toolchain resolution, pin materialization, solving). *)
  let is_script =
    Filename.check_suffix target ".ml"
    || String.starts_with ~prefix:"http://" target
    || String.starts_with ~prefix:"https://" target
  in
  let fast_key =
    if dry_run || refresh || is_script then None
    else
      Some
        (Digest.to_hex
           (Digest.string
              (String.concat "\x00"
                 ([
                    os_key;
                    binary_name;
                    Stdlib.Option.value ~default:"" toolchain_override;
                    registry;
                    (if skip_local then "skip-local" else "with-local");
                  ]
                 @ List.sort String.compare with_deps
                 @ [ "\x01" ]
                 @ List.sort String.compare with_repos))))
  in
  let fast_cache_path key =
    cache_root / "run-cache" / String.sub key 0 2 / (key ^ ".marshal")
  in
  let store_fast_cache ~bin_path ~prefix ~tc_ctx =
    match fast_key with
    | None -> ()
    | Some key -> (
        try
          let path = fast_cache_path key in
          let dir = Filename.dirname path in
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
          let entry : string * string * (string * bool) option =
            ( bin_path,
              prefix,
              Option.map
                (fun (tc : Oi.Solver.Ctx.toolchain) ->
                  (tc.install_prefix, tc.relocatable))
                tc_ctx )
          in
          let tmp = path ^ ".tmp" in
          Out_channel.with_open_bin tmp (fun oc ->
              Marshal.to_channel oc entry []);
          Sys.rename tmp path
        with _ -> ())
  in
  (match fast_key with
  | Some key -> (
      match
        try
          let ic = open_in_bin (fast_cache_path key) in
          Fun.protect
            ~finally:(fun () -> close_in_noerr ic)
            (fun () ->
              Some
                (Marshal.from_channel ic
                  : string * string * (string * bool) option))
        with _ -> None
      with
      | Some (bin_path, prefix, tc_info) when Sys.file_exists bin_path ->
          let tc_ctx =
            match tc_info with
            | None -> None
            | Some (install_prefix, relocatable) ->
                Some
                  {
                    Oi.Solver.Ctx.install_prefix;
                    hash = "";
                    relocatable;
                    packages = OpamPackage.Set.empty;
                    root_names = OpamPackage.Name.Set.empty;
                  }
          in
          let env =
            Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
          in
          exit (Subprocess.run proc_mgr ~env (bin_path :: args))
      | _ -> ())
  | None -> ());
  (* Normal path: initialize opam, resolve toolchain, solve, build. *)
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let conf =
    Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let { Terms.layer_remote; source_remote } =
    Terms.remotes_of ~url:registry ~mode:use_registry
  in
  (* URL-projects in [--with=…]: clone each URL into the pin cache,
       scan its *.opam files, and merge the contribution as pins +
       solver roots + overlays + extra_repos. *)
  let extra_deps, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let extra_constraints = Oi.Project.Script.constraints extra_deps in
  (* Resolve the cwd once; reused for project-extras loading and the
       script-file existence check below. *)
  let cwd_s, cwd = Workspace.resolved_cwd fs in
  (* Load project extras (if any *.opam in cwd). A missing/unreadable
       directory degrades to "no extras"; malformed metadata still raises
       [Error.E] so the user sees the problem. *)
  let project_extras, project_pins, project_overlays, project_packages_dir =
    if skip_local then ([], [], [], None)
    else
      match Oi.Project.load ~fs cwd_s with
      | exception Sys_error _ -> ([], [], [], None)
      | exception Eio.Exn.Io _ -> ([], [], [], None)
      | p -> (p.extra_repos, p.pins, p.overlays, p.packages_dir)
  in
  let local_packages_dir =
    match project_packages_dir with
    | Some _ -> project_packages_dir
    | None -> url_project.packages_dir
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
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
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
    Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
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
            when OpamPackage.Name.to_string (OpamPackage.name p) = want_name ->
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
  (* Phase budget for the overall bar: [oi run]'s flow only fires
     [Build_pipeline.build]'s phases (no [Sync] / [install_tools] / dune
     subprocess), so ~8 phases on cold cache, fewer when the layer
     cache hit short-circuits the source path. *)
  let with_preflight_bar f =
    Preflight_bar.Preflight.with_bar ~clock ~total_steps:10 f
  in
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
      |> Oi.Pipeline.strip_compiler_roots_for_override
           ~override:toolchain_override ~toolchain
    in
    let pipeline_env : Oi.Build_pipeline.env =
      {
        proc_mgr;
        fs;
        clock;
        sys;
        os_key;
        cache;
        data_dir;
        http_session;
      }
    in
    let req : Oi.Build_pipeline.request =
      {
        targets =
          [ Group { tokens = List.map OpamPackage.Name.to_string names; handles = [] } ];
        with_repos = [];
        pins = project_pins;
        extra_repos = all_extras;
        constraints = extra_constraints;
        toolchain_override;
        toolchain;
        conf;
        local_packages_dir;
        project_root = None;
        force_source = false;
        refresh;
      }
    in
    let layer_hashes =
      Progress_ui.with_ui ~target:binary_name
        ~clock:(clock :> _ Eio.Resource.t)
        ~enabled:(Tty.is_tty ())
      @@ fun reporter ->
      let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
      if dry_run then begin
        (* Match [Build_pipeline.build]'s old [~dry_run] semantic: print
           the plan and exit 0 without fetching or building. *)
        (match solved.merged with
        | None -> Fmt.pr "(no packages to build)@."
        | Some merged ->
            List.iter
              (fun (n : D10ir.Plan.node) ->
                Fmt.pr "%s.%s  %s@." n.package.name n.package.version
                  (D10ir.Layer_hash.to_string n.layer_hash))
              merged.nodes);
        exit 0
      end;
      (match save_d10ir with
      | None -> ()
      | Some dir ->
          List.iter
            (fun (gr : Oi.Build_pipeline.group_result) ->
              match gr.recipe with
              | None -> ()
              | Some recipe ->
                  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
                    Eio.Path.(fs / dir);
                  let stem =
                    match gr.group.tokens with
                    | t :: _ ->
                        String.map
                          (fun c ->
                            if (c >= 'a' && c <= 'z')
                               || (c >= 'A' && c <= 'Z')
                               || (c >= '0' && c <= '9')
                               || c = '.' || c = '-' || c = '_'
                            then c
                            else '_')
                          t
                    | [] -> "recipe"
                  in
                  let dst =
                    Filename.concat dir (stem ^ ".d10ir.json")
                  in
                  D10ir.Plan.save Eio.Path.(fs / dst) recipe)
            solved.groups);
      let build_result =
        Oi.Build_pipeline.build pipeline_env ~reporter
          { solved; layer_remote; source_remote; jobs }
      in
      (* Surface build outcomes: previously [_ = build …] discarded
         this, so a failed [D10ir.Direct.run] (or one that returned
         immediately with an empty plan) silently produced an empty
         prefix, and we'd later mis-report "solved but does not install
         bin/<target>". *)
      (match build_result with
      | None ->
          (* Build_pipeline.build returns [None] when [solved.merged]
             is [None] — every solve group failed elaborate or recipe
             emit. The per-group [error] field carries the reason. *)
          let group_msgs =
            List.filter_map
              (fun (gr : Oi.Build_pipeline.group_result) ->
                match gr.error with
                | Ok () -> None
                | Error e ->
                    let kind =
                      match e with
                      | Solve_failed _ -> "solve"
                      | Cycle _ -> "cycle"
                      | Empty_after_strip -> "empty"
                      | Elaborate_failed _ -> "elaborate"
                      | Emit_failed _ -> "emit"
                    in
                    Some (Fmt.str "%s: %s" gr.group.label kind))
              solved.groups
          in
          Oi.Error.config_error
            "build pipeline produced no executable plan (%s). Re-run with \
             --verbosity=debug for the per-group trace."
            (String.concat ", " group_msgs)
      | Some r when r.failed = 0 && r.skipped = 0 && r.built = 0 && r.cached = 0 ->
          (* Direct.run was handed an empty plan. Likely cause: every
             package was filtered out of the d10ir recipe (e.g. all
             marked Binary against a stale d10 cache, or recipe emit
             skipped them silently). Without this guard we'd assemble
             an empty prefix and report a misleading "no bin/<X>". *)
          Oi.Error.config_error
            "build pipeline ran with an empty d10ir plan: solver \
             picked packages but the d10ir executor saw 0 nodes. The \
             most likely cause is a stale d10 layer cache; try \
             [oi clean --layers] and re-run."
      | Some r when r.failed = 0 && r.skipped = 0 -> ()
      | Some r ->
          let pp_fail (f : D10ir.Direct.failure) =
            Fmt.str "%s.%s @ %s — see %s" f.package.name f.package.version
              (D10ir.Direct.phase_to_string f.phase) f.log_path
          in
          if r.failures <> [] then begin
            let summary = List.map pp_fail r.failures |> String.concat "\n  " in
            Oi.Error.config_error
              "build failed: %d node(s) failed, %d skipped.@\n  %s"
              r.failed r.skipped summary
          end
          else
            Oi.Error.config_error
              "build failed: 0 nodes built, %d skipped (likely a dep \
               chain broke upstream). Re-run with --verbosity=debug for \
               the per-node trace."
              r.skipped);
      Oi.Build_pipeline.layer_hashes solved
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
      store_fast_cache ~bin_path:bin ~prefix ~tc_ctx;
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
        Logs.info (fun m -> m "Found %s in toolchain prefix: %s" binary_name p);
        store_fast_cache ~bin_path:p ~prefix ~tc_ctx;
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
      if not (D10.Sysops.Http.fetch sys ~url:target ~dst:Eio.Path.(fs / local))
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
          if OpamPackage.Name.equal d.name ocaml_name then None else Some d.name)
        all_script_deps
    in
    let constraints = Oi.Project.Script.constraints all_script_deps in
    let dep_opam_names =
      Oi.Pipeline.strip_compiler_roots_for_override ~override:toolchain_override
        ~toolchain dep_opam_names
    in
    let layer_hashes =
      if dep_opam_names = [] then []
      else
        let pipeline_env : Oi.Build_pipeline.env =
          {
            proc_mgr;
            fs;
            clock;
            sys;
            os_key;
            cache;
            data_dir;
            http_session;
          }
        in
        let req : Oi.Build_pipeline.request =
          {
            targets =
              [
                Group { tokens = List.map OpamPackage.Name.to_string dep_opam_names; handles = [] };
              ];
            with_repos = [];
            pins = project_pins;
            extra_repos = all_extras;
            constraints;
            toolchain_override;
            toolchain;
            conf;
            local_packages_dir;
            project_root = None;
        force_source = false;
            refresh;
          }
        in
        Progress_ui.with_ui ~target ~clock:(clock :> _ Eio.Resource.t)
          ~enabled:(Tty.is_tty ())
        @@ fun reporter ->
        let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
        if dry_run then begin
          (match solved.merged with
          | None -> Fmt.pr "(no packages to build)@."
          | Some merged ->
              List.iter
                (fun (n : D10ir.Plan.node) ->
                  Fmt.pr "%s.%s  %s@." n.package.name n.package.version
                    (D10ir.Layer_hash.to_string n.layer_hash))
                merged.nodes);
          exit 0
        end;
        (match save_d10ir with
        | None -> ()
        | Some dir ->
            List.iter
              (fun (gr : Oi.Build_pipeline.group_result) ->
                match gr.recipe with
                | None -> ()
                | Some recipe ->
                    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
                      Eio.Path.(fs / dir);
                    let dst = Filename.concat dir "recipe.d10ir.json" in
                    D10ir.Plan.save Eio.Path.(fs / dst) recipe)
              solved.groups);
        let _ : D10ir.Direct.result option =
          Oi.Build_pipeline.build pipeline_env ~reporter
            { solved; layer_remote; source_remote; jobs }
        in
        Oi.Build_pipeline.layer_hashes solved
    in
    if dry_run && dep_opam_names = [] then
      (* No deps to solve, but still in dry-run mode — just exit *)
      exit 0;
    let prefix =
      Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
    in
    Script_runner.run ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache
      ~data_dir ?toolchain ?source_remote target extra_deps args
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
         when set (matches what Build_pipeline.build does), so the precheck
         sees the same package universe the actual solve will. *)
    let packages_dirs =
      lazy
        (let pin_dir =
           Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh project_pins
         in
         Stdlib.Option.to_list local_packages_dir
         @ Stdlib.Option.to_list pin_dir
         @ Oi.Source.Repo.ensure_many ~fs ~data_dir ~refresh all_extras
         @
         match toolchain with
         | Some (info : Oi.Toolchain.info) -> info.packages_dirs
         | None -> Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ())
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
    (* Step-fallback wrapper: only catches solve-time failures
       ([No_solution]) so the caller can move on to the next lookup
       strategy. Anything else — a build failure, a fetch failure,
       a config error — must propagate, otherwise the user sees a
       misleading "binary not found" message after a real error
       further down the stack (e.g. a dep that didn't compile). *)
    let try_step label f =
      try f ()
      with Oi.Error.E e as exn -> (
        match Oi.Error.kind e with
        | K_no_solution | K_not_found ->
            Logs.info (fun m -> m "%s failed: %a" label Oi.Error.pp e);
            false
        | _ -> Stdlib.raise exn)
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
      Oi.Error.not_found binary_name "%s solved but does not install bin/%s.%s"
        qualified binary_name suggestion
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
      (* Open a TTY-only spinner around the index lookup so the multi-
         second registry fetch isn't a silent freeze on cold caches.
         The bar auto-clears when the lookup returns. *)
      let from_index =
        with_preflight_bar
        @@ fun ~on_phase ~on_text:_ ~preflight_done ~shared_display:_ ->
        let r =
          Layer_index.binary_to_package ~on_phase ~sys ~fs ~clock:clk ~cache
            ~os_key ~registry binary_name
        in
        preflight_done ();
        r
      in
      let from_index =
        match from_index with
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

let target =
  Arg.(
    required
    & pos 0 (some string) None
    & info ~docv:"TARGET"
        ~doc:
          "Binary name, $(b,@HANDLE/PKG) overlay shortcut, $(b,.ml) script \
           path, or $(b,https://) URL pointing at a $(b,.ml) script ($(b,http://) \
           also accepted)."
        [])

let dry_run =
  Arg.(
    value & flag
    & info ~doc:"Print the build plan and exit."
        [ "n"; "dry-run" ])

let args =
  Arg.(
    value & pos_right 0 string []
    & info ~docv:"ARG" ~doc:"Forwarded to $(b,TARGET)." [])

let save_d10ir =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"DIR"
        ~doc:
          "Write the d10ir recipe for each solve group to \
           $(b,DIR/<root>.d10ir.json). Does not skip the build."
        [ "save-d10ir" ])

let info_run =
  Cmd.info "run" ~doc:"Run an opam-packaged binary or OCaml script"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Resolve $(b,TARGET)'s dependencies into the shared cache and exec \
           $(b,TARGET). Arguments after $(b,TARGET) are forwarded unchanged.";
        `P "$(b,TARGET) is one of:";
        `I ("$(b,name)", "Binary name. Looked up in the layer index; \
                          dash-prefixes fall back ($(b,ocluster-admin) tries \
                          $(b,ocluster)).");
        `I ("$(b,@HANDLE/PKG)", "Take $(b,PKG) from the named overlay.");
        `I ("$(b,path/to/script.ml)", "Local OCaml script.");
        `I ("$(b,https://...ml)", "Remote OCaml script ($(b,http://) also \
                                    accepted). Refetched each run; cached \
                                    by content hash.");
        `S "BINARIES";
        `Pre
          "  oi run utop\n\
          \  oi run ocamlformat -- --help\n\
          \  oi run --with=crockford roguedoi";
        `S "SCRIPTS";
        `P "Declare deps on the first line:";
        `Pre "  [@@@opam fmt cmdliner lwt>=5.0]";
        `P
          "Each token is an opam package with optional constraint \
           ($(b,>=), $(b,>), $(b,<=), $(b,<), $(b,=)) and optional findlib \
           sub-library ($(b,ppx_deriving.show)). $(b,ppx_*) packages are wired \
           as preprocessors.";
        `Pre
          "  oi run my_script.ml\n\
          \  oi run my_script.ml --with=tls -- arg1 arg2\n\
          \  oi run https://gist.example.com/hello.ml";
        `S "OVERLAYS";
        `P
          "$(i,@HANDLE/PKG) on $(b,TARGET) or $(b,--with) takes $(b,PKG) from \
           an overlay. Bare $(i,@HANDLE) on $(b,--with-repo) stacks the whole \
           overlay. $(b,x-repos: [\"@HANDLE\"]) in a project's $(b,*.opam) \
           applies it automatically.";
        `Pre
          "  oi run @avsm/owntracks\n  oi run --with=@avsm/crockford roguedoi";
        `S "PROJECT EXTRAS";
        `P "Read from the cwd (or a $(b,--with=URL) clone):";
        `I ("$(b,*.opam)",
            "$(b,depends:), $(b,pin-depends:), $(b,x-repos:) merge into the \
             solve.");
        `I ("$(b,packages/) + $(b,repo)",
            "Highest-priority opam-repository overlay for patched transitive \
             deps.");
        `S "TOOLCHAIN";
        `P "Picked in order:";
        `I ("1.", "$(b,--toolchain=NAME).");
        `I ("2.", "$(b,x-oi-toolchain) on an in-scope $(b,@HANDLE). Conflicts \
                  error.");
        `I ("3.", "Reporepo's $(b,x-oi-default-toolchain).");
        `S "GIT URLS";
        `P
          "$(b,--with=URL) clones the repo and pins every root $(b,*.opam). \
           Schemes: $(b,http://), $(b,https://), $(b,git+), $(b,git@), \
           $(b,git://), $(b,ssh://). Append $(b,#REF) for a tag, branch, \
           or commit.";
        `Pre
          "  oi run --with=https://github.com/owner/project.git target\n\
          \  oi run --with=git+https://example.org/foo.git#branch foo";
        `S "VERSION CONSTRAINTS";
        `P "$(b,pkg.VERSION) or $(b,pkg=VERSION) on $(b,--with).";
        `Pre
          "  oi run --with=dune.3.20.0 -- dune --version\n\
          \  oi run --with=fmt>=0.9 my_script.ml";
        `S "DRY RUN";
        `P "$(b,-n) prints the plan and exits. Per-package tags:";
        `I ("$(b,binary)", "Cached locally.");
        `I ("$(b,remote)", "Fetchable from the registry.");
        `I ("$(b,source)", "Compiles from source.");
        `I ("$(b,virtual)", "No-op placeholder ($(b,conf-pkg-config) etc.).");
      ]

let info_oix =
  Cmd.info "oix" ~doc:"Run a binary from opam overlays"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Resolve $(b,TARGET), build its dependencies into the shared cache, \
           and exec it. Arguments after $(b,TARGET) are forwarded unchanged. \
           Equivalent to $(b,oi run --skip-local).";
        `Pre
          "  oix utop\n\
          \  oix patdiff a.txt b.txt\n\
          \  oix ocamlformat --check .\n\
          \  oix hello.ml\n\
          \  oix --with=ocamlformat.0.29.0 ocamlformat --check .\n\
          \  oix --with=tls --with=cohttp my-client\n\
          \  oix --toolchain=oxcaml utop\n\
          \  oix @avsm/owntracks\n\
          \  oix -n utop";
        `S "EXTRA DEPENDENCIES";
        `P
          "$(b,--with) accepts a bare name, opam atom \
           ($(b,fmt>=0.9), $(b,dune.3.20.0)), or git URL \
           (every root $(b,*.opam) becomes a pin). Repeatable.";
        `Pre
          "  oix --with=tls --with=cohttp my_client\n\
          \  oix --with=dune.3.20.0 dune --version\n\
          \  oix --with=https://github.com/owner/proj.git my-tool";
        `S "OVERLAYS";
        `P
          "$(b,@HANDLE/PKG) on $(b,TARGET) or $(b,--with) takes one package \
           from an overlay. $(b,--with-repo=@HANDLE) stacks the whole \
           overlay.";
        `Pre "  oix @avsm/owntracks\n  oix --with-repo=@avsm crockford";
        `S "TOOLCHAIN";
        `P
          "$(b,--toolchain=NAME) pins the compiler. Otherwise an overlay's \
           $(b,x-oi-toolchain) wins, falling back to the reporepo default.";
        `S "SCRIPT FORMAT";
        `P
          "$(b,TARGET) may be a $(b,.ml) script path or an $(b,https://) \
           URL ($(b,http://) also accepted). Declare deps on the first \
           line:";
        `Pre "  [@@@opam fmt cmdliner>=1.2.0 lwt]";
        `P
          "Each token is an opam package with optional constraint \
           ($(b,>=), $(b,>), $(b,<=), $(b,<), $(b,=)) and optional findlib \
           sub-library ($(b,ppx_deriving.show)). $(b,ppx_*) packages are \
           wired as preprocessors.";
        `S Manpage.s_see_also;
        `P "$(b,oi)(1).";
      ]

let term ~skip_local =
  Term.(
    const run_impl $ Terms.common $ Terms.refresh $ Terms.locked $ skip_local
    $ dry_run $ Terms.registry $ Terms.use_registry $ Terms.toolchain $ target
    $ Terms.with_deps $ Terms.with_repos $ Terms.jobs $ save_d10ir $ args)

let cmd = Cmd.v info_run (term ~skip_local:Terms.skip_local)
let cmd_x = Cmd.v info_oix (term ~skip_local:(Term.const true))
