let ( / ) = Filename.concat
let short_hash h = String.sub h 0 (min 12 (String.length h))

(* Return the layer hash whose [layer.json] declares package name
   [want_name] (any version). Tools get assembled from only their own
   leaf layer — the transitive deps (ocaml, dune, ocamlfind…) are
   already present in the shared d10 cache but are not needed at
   runtime by a native-compiled tool binary, so we leave them out of
   [_oi/tools/] to keep it small and focused. *)
let leaf_hash_for ~fs ~cache ~os_key ~want_name hashes =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let leaf hash =
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
  List.find_map leaf hashes

(* Solve and install every dev tool into [cwd/_oi/tools/]. The set is
   the union of:
   - tools listed in the active toolchain's [x-oi-toolchain-tools]
     field (always-on for that toolchain — odoc, merlin, lsp);
   - tools whose project-state trigger fires (mdx if dune-project uses
     it, ocamlformat if .ocamlformat is present), via [Project.Tool.probe].
   Each tool is its own independent solve so its dep closure never
   leaks into the main project's OCAMLLIB / OCAMLPATH. A tool that
   fails to solve (e.g. pinned to an older ocaml) warns and is
   skipped; other tools still install. Returns the assembled path if
   at least one tool made it in, or [None] if nothing to install. *)
let install_tools ?(quiet = false) ?refresh ?jobs ~proc_mgr ~fs ~clock ~sys
    ~cache ~data_dir ~conf ~os_key ~session ~extra_repos ~pins ?toolchain
    ?layer_remote ?source_remote ~cwd () =
  let say_step fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Oi.Say.step "%s" s) fmt
  in
  let say_info fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Oi.Say.info "%s" s) fmt
  in
  let warn_named name fmt =
    Fmt.kstr (fun s -> Oi.Say.warn "tool %s: %s" name s) fmt
  in
  let install_named ~tool_name ~constraints =
    let name = OpamPackage.Name.of_string tool_name in
    (* Surface the per-tool [Build_pipeline.build] phases (build solver
       context, solve, plan, fetch layers) and Execute's per-package
       progress so a long tool build doesn't look like a hang. In
       quiet mode the phase narration drops to [Logs.info], matching
       the rest of [install_tools]. *)
    try
      let pipeline_env : Oi.Build_pipeline.env =
        {
          proc_mgr;
          fs;
          clock;
          sys;
          os_key;
          cache;
          data_dir;
          http_session = session;
        }
      in
      let req : Oi.Build_pipeline.request =
        {
          targets = [ Plain (OpamPackage.Name.to_string name) ];
          with_repos = [];
          pins;
          extra_repos;
          constraints;
          toolchain_override = None;
          toolchain;
          conf;
          local_packages_dir = None;
          project_root = Some cwd;
        force_source = false;
          refresh = Stdlib.Option.value ~default:false refresh;
        }
      in
      let hashes =
        Progress_ui.with_ui ~target:tool_name
          ~clock:(clock :> _ Eio.Resource.t)
          ~enabled:(not quiet && Tty.is_tty ())
        @@ fun reporter ->
        let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
        let _ : D10ir.Direct.result option =
          Oi.Build_pipeline.build pipeline_env ~reporter
            { solved; layer_remote; source_remote; jobs }
        in
        Oi.Build_pipeline.layer_hashes solved
      in
      match leaf_hash_for ~fs ~cache ~os_key ~want_name:tool_name hashes with
      | None ->
          warn_named tool_name "layer for leaf package not found";
          None
      | Some h ->
          say_info "tool %s: %d dep(s) built, leaf layer %s" tool_name
            (List.length hashes - 1)
            (short_hash h);
          Some h
    with
    | Oi.Error.E e ->
        warn_named tool_name "%a" Oi.Error.pp e;
        None
    | exn ->
        warn_named tool_name "%s" (Printexc.to_string exn);
        None
  in
  let install_probed (r : Oi.Project.Tool.result) =
    let constraints =
      match r.version with
      | None -> OpamPackage.Name.Map.empty
      | Some v ->
          OpamPackage.Name.Map.singleton
            (OpamPackage.Name.of_string r.spec.name)
            (`Eq, OpamPackage.Version.of_string v)
    in
    install_named ~tool_name:r.spec.name ~constraints
  in
  let toolchain_tools =
    match (toolchain : Oi.Toolchain.info option) with
    | None -> []
    | Some info -> info.tools
  in
  let probed_hits = Oi.Project.Tool.(hits (probe ~fs cwd)) in
  match (toolchain_tools, probed_hits) with
  | [], [] ->
      say_info "no dev tools to install";
      None
  | _ -> (
      let from_toolchain =
        List.filter_map
          (fun n ->
            install_named ~tool_name:n ~constraints:OpamPackage.Name.Map.empty)
          toolchain_tools
      in
      let from_probes = List.filter_map install_probed probed_hits in
      let leaves = from_toolchain @ from_probes in
      match leaves with
      | [] -> None
      | _ ->
          let tools_dir = cwd / "_oi" / "tools" in
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / tools_dir);
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / tools_dir);
          let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
          let unique = List.sort_uniq String.compare leaves in
          D10.Prefix.assemble d10 ~layer_hashes:unique
            ~dst:Eio.Path.(fs / tools_dir);
          say_step "Tools assembled at %s (%d tool(s), %d leaf layer(s))"
            tools_dir (List.length leaves) (List.length unique);
          Some tools_dir)

(* -- sync ---------------------------------------------------------------- *)

(* Load [cwd]/*.opam + URL-project overlays + [--with-repo] handles
   into one deduped handle list, then resolve the toolchain against it.
   This is the project-aware [tc_handles] formula every command should
   use; [oi sync] does it inline below, [oi exec] / [oi env] call this
   helper directly so they pick the same toolchain. *)
let resolve_project_toolchain ?(refresh = false) ?(skip_local = false)
    ?(with_repos = []) ?(with_deps = []) ~fs ~sys ~cache ~data_dir ~conf
    ~install ~override ~cwd () =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let project_overlays =
    if skip_local then []
    else
      match Oi.Project.load ~fs cwd with
      | exception Sys_error _ -> []
      | exception Eio.Exn.Io _ -> []
      | p -> p.overlays
  in
  let _, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let tc_handles =
    project_overlays @ url_project.overlays
    @ Target.handles_of_tokens with_repos
    |> List.sort_uniq String.compare
  in
  Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install ~override
    ~handles:tc_handles ()

(* Run a full sync in [cwd]: solve the deps declared in *.opam files,
   build/fetch layers, assemble [cwd]/_oi/prefix, and (re)write .envrc.
   Returns the assembled prefix path and the resolved toolchain. When
   [quiet] is true, narration goes to Logs.info (hidden at default
   verbosity); otherwise it prints to stdout. *)
type envrc_mode = [ `Skip | `Always | `Detect ]

let pp_envrc_mode ppf = function
  | `Skip -> Fmt.string ppf "skip"
  | `Always -> Fmt.string ppf "always"
  | `Detect -> Fmt.string ppf "detect"

(* True if [direnv] is on PATH. Resolved at sync time so users who add
   direnv mid-project pick it up on the next [oi sync] / [oi build]. *)
let direnv_on_path () =
  match Sys.getenv_opt "PATH" with
  | None | Some "" -> false
  | Some path ->
      String.split_on_char ':' path
      |> List.exists (fun d ->
          if d = "" then false
          else
            let p = d / "direnv" in
            try
              Unix.access p [ Unix.X_OK ];
              true
            with Unix.Unix_error _ -> false)

let envrc_should_write = function
  | `Skip -> false
  | `Always -> true
  | `Detect -> direnv_on_path ()

let run ?(quiet = false) ?(refresh = false) ?(skip_local = false)
    ?(with_repos = []) ?(with_deps = []) ?jobs ?(toolchain : string option)
    ?(envrc_mode = `Detect) ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache
    ~data_dir ~registry ~use_registry ~session ~cwd () =
  let toolchain_override = toolchain in
  (* [quiet]: route narration to [Logs.info] (debug-only). Otherwise
     persistent [Oi.Say.step] / [Oi.Say.info] lines. The
     [Progress_ui] bar (opened around the [Build_pipeline.build] call
     below) interjects properly so Say lines and bar redraws don't
     tear each other. *)
  let say_step fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Oi.Say.step "%s" s) fmt
  in
  let say_info fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Oi.Say.info "%s" s) fmt
  in
  let say_field_list label items =
    if quiet then begin
      if items <> [] then
        Logs.info (fun m -> m "%s: %s" label (String.concat ", " items))
    end
    else Oi.Say.field_list label items
  in
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let project =
    if skip_local then Oi.Project.empty else Oi.Project.load ~fs cwd
  in
  let extra_cli, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let deps = project.deps in
  if deps = [] && extra_cli = [] && url_project.roots = [] then
    Oi.Error.config_error "No .opam files found in %s." cwd;
  say_step "Sync %s" cwd;
  say_field_list "deps" deps;
  say_field_list "with-deps" url_project.roots;
  let conf =
    Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  (* Same handle scope as {resolve_project_toolchain} — kept inline
     here so we can reuse the already-loaded [project] / [url_project]
     side-products below. *)
  let candidate_overlays = project.overlays @ url_project.overlays in
  let tc_handles =
    candidate_overlays @ Target.handles_of_tokens with_repos
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
      ~override:toolchain ~handles:tc_handles ()
  in
  let conf, _ = Oi.Pipeline.solver_inputs toolchain conf in
  let project_overlays =
    Oi.Pipeline.filter_compatible_overlays
      ~reporepo_path:(Terms.reporepo_path ()) ~toolchain candidate_overlays
  in
  say_field_list "overlays" project_overlays;
  let with_repos = project_overlays @ with_repos in
  let all_extras =
    Target.merge_extras
      ~cli:(Target.cli_extra_repos ~fs ~sys ?toolchain with_repos)
      ~project:(project.extra_repos @ url_project.extra_repos)
  in
  let extra_repo_labels =
    List.map
      (fun (e : Oi.Project.extra_repo) -> Fmt.str "%s (%s)" e.name e.url)
      all_extras
  in
  say_field_list "extra-repos" extra_repo_labels;
  let { Terms.layer_remote; source_remote } =
    Terms.remotes_of ~url:registry ~mode:use_registry
  in
  let extra_constraints = Oi.Project.Script.constraints extra_cli in
  let extra_names =
    List.filter_map
      (fun (d : Oi.Project.Script.dep) ->
        if OpamPackage.Name.to_string d.name = "ocaml" then None
        else Some d.name)
      extra_cli
  in
  let url_names = List.map OpamPackage.Name.of_string url_project.roots in
  let names =
    List.map OpamPackage.Name.of_string deps @ extra_names @ url_names
    |> Oi.Pipeline.strip_compiler_roots_for_override
         ~override:toolchain_override ~toolchain
  in
  let layer_hashes =
    let local_packages_dir =
      match project.packages_dir with
      | Some _ -> project.packages_dir
      | None -> url_project.packages_dir
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
        http_session = session;
      }
    in
    let req : Oi.Build_pipeline.request =
      {
        targets =
          [ Group { tokens = List.map OpamPackage.Name.to_string names; handles = [] } ];
        with_repos = [];
        pins = project.pins @ url_project.pins;
        extra_repos = all_extras;
        constraints = extra_constraints;
        toolchain_override;
        toolchain;
        conf;
        local_packages_dir;
        project_root = Some cwd;
        force_source = false;
        refresh;
      }
    in
    Progress_ui.with_ui ~target:cwd ~clock:(clock :> _ Eio.Resource.t)
      ~enabled:(not quiet && Tty.is_tty ())
    @@ fun reporter ->
    let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
    let _ : D10ir.Direct.result option =
      Oi.Build_pipeline.build pipeline_env ~reporter
        { solved; layer_remote; source_remote; jobs }
    in
    Oi.Build_pipeline.layer_hashes solved
  in
  let oi_dir = cwd / "_oi" in
  let prefix = oi_dir / "prefix" in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / oi_dir);
  let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
  say_step "Assembling project prefix";
  D10.Prefix.assemble d10 ~layer_hashes ~dst:Eio.Path.(fs / prefix);
  say_step "Installing dev tools";
  let tools =
    install_tools ~quiet ?refresh:(Some refresh) ?jobs ~proc_mgr ~fs ~clock
      ~sys ~cache ~data_dir ~conf ~os_key ~session ~extra_repos:all_extras
      ~pins:project.pins ?toolchain ?layer_remote ?source_remote ~cwd ()
  in
  if envrc_should_write envrc_mode then begin
    let envrc_path = Eio.Path.(fs / cwd / ".envrc") in
    let dune_cache_root = Oi.Cache.dune_root cache in
    let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info toolchain in
    let envrc =
      Oi.Solver.Env.envrc_content ?toolchain:tc_ctx ~prefix ?tools
        ~dune_cache_root ()
    in
    (try Eio.Path.unlink envrc_path with Eio.Exn.Io _ -> ());
    Eio.Path.save ~create:(`Exclusive 0o644) envrc_path envrc;
    say_step "Writing .envrc";
    say_info "run 'direnv allow' to activate"
  end
  else
    Logs.info (fun m ->
        m "Skipping .envrc (--envrc=%a)" pp_envrc_mode envrc_mode);
  say_step "Prefix assembled at %s (%d packages)" prefix
    (List.length layer_hashes);
  (prefix, toolchain)

(* True if [cwd]/_oi/prefix is missing, or any *.opam in [cwd] has been
   modified more recently than the prefix directory. *)
let needs_sync ~cwd ~prefix =
  match Unix.stat prefix with
  | exception Unix.Unix_error _ -> true
  | st ->
      let prefix_mtime = st.Unix.st_mtime in
      let opam_files =
        try
          Sys.readdir cwd |> Array.to_list
          |> List.filter (fun f ->
              Filename.check_suffix f ".opam"
              && Filename.chop_suffix f ".opam" <> "")
        with Sys_error _ -> []
      in
      List.exists
        (fun f ->
          try (Unix.stat (cwd / f)).Unix.st_mtime > prefix_mtime
          with Unix.Unix_error _ -> false)
        opam_files

(* Cmdliner term for $(b,--envrc=skip|always|detect). Shared with
   $(b,oi build). *)
let envrc_mode_arg =
  let modes =
    Cmdliner.Arg.enum
      [ ("skip", `Skip); ("always", `Always); ("detect", `Detect) ]
  in
  Cmdliner.Arg.(
    value & opt modes `Detect
    & info ~docv:"MODE"
        ~doc:
          "When to write $(b,.envrc): $(b,skip), $(b,always), or $(b,detect) \
           (default — write only if $(b,direnv) is on PATH)."
        [ "envrc" ])
