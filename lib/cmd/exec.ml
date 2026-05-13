open Cmdliner

let log_src = Logs.Src.create "oi.cmd.exec" ~doc:"oi exec command"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

let cmd =
  let run (c : Terms.common) refresh skip_local registry use_registry with_repos
      with_deps jobs toolchain cmd args =
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
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let data_dir = c.data_dir in
    let cwd, _ = Workspace.resolved_cwd fs in
    let prefix = cwd / "_oi" / "prefix" in
    let conf =
      Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    (* Any --with-repo / --with / --toolchain flag forces a re-sync
       even if the prefix is fresh, so the extras and toolchain make
       it into the build. [--skip-local] also forces a fresh sync so
       the project's [_oi/prefix/] is bypassed. The toolchain we use
       for env vars is the one sync just resolved (when sync ran) or
       the same project-aware resolve sync would have done (when we
       skipped it). *)
    let forced =
      skip_local || with_repos <> [] || with_deps <> [] || toolchain <> None
    in
    let tc_info =
      if forced || Sync.needs_sync ~cwd ~prefix then begin
        Log.info (fun m -> m "Syncing %s before exec" cwd);
        let _, tc =
          Sync.run ~quiet:true ~refresh ~skip_local ~with_repos ~with_deps ?jobs
            ?toolchain ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache
            ~data_dir ~registry ~use_registry ~session:http_session ~cwd ()
        in
        tc
      end
      else
        Sync.resolve_project_toolchain ~refresh ~skip_local ~with_repos
          ~with_deps ~fs ~sys ~cache ~data_dir ~conf ~install:true
          ~override:toolchain ~cwd ()
    in
    let tools = Workspace.tools_dir_for ~cwd in
    let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info tc_info in
    let env_arr =
      Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ?tools
        ~dune_cache_root:(Oi.Cache.dune_root cache) ()
    in
    exit (Subprocess.run proc_mgr ~env:env_arr (cmd :: args))
  in
  let cmd =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"CMD" ~doc:"Command to run." [])
  in
  let args =
    Arg.(
      value & pos_right 0 string []
      & info ~docv:"ARG"
          ~doc:
            "Arguments for $(b,CMD). Use $(b,--) to separate them from \
             $(mname)'s flags."
          [])
  in
  let info =
    Cmd.info "exec" ~doc:"Run a command in the project environment"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Run $(b,CMD) with $(b,_oi/tools/bin) and $(b,_oi/prefix/bin) \
             prepended to $(b,PATH), and OCaml environment variables pointing \
             at $(b,_oi/prefix/).";
          `P
            "Syncs first when $(b,_oi/prefix/) is missing or older than any \
             $(b,*.opam). $(b,--with), $(b,--with-repo), $(b,--toolchain), and \
             $(b,--skip-local) force a re-sync.";
          `S "TOOLCHAIN";
          `P "Active toolchain, in order:";
          `I ("1.", "$(b,--toolchain=NAME).");
          `I
            ( "2.",
              "$(b,x-oi-toolchain) on any in-scope $(b,@HANDLE) — from \
               $(b,--with-repo=@h), $(b,--with=@h/pkg), or the project's \
               $(b,x-repos:). Conflicting tags error out." );
          `I ("3.", "Reporepo entry flagged $(b,x-oi-default-toolchain: true).");
          `Pre
            "  oi exec dune build\n\
            \  oi exec -- ocamlformat --check .\n\
            \  oi exec utop";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.skip_local
      $ Terms.registry $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps
      $ Terms.jobs $ Terms.toolchain $ cmd $ args)
