open Cmdliner

let ( / ) = Filename.concat

let cmd =
  let run () data_dir cache_dir refresh skip_local registry with_repos with_deps
      jobs toolchain cmd args =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache } =
      Harness.bootstrap env cache_dir
    in
    let cwd, _ = Workspace.resolved_cwd fs in
    let prefix = cwd / "_oi" / "prefix" in
    let conf =
      Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
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
        Logs.info (fun m -> m "Syncing %s before exec" cwd);
        let _, tc =
          Sync.do_sync ~quiet:true ~refresh ~skip_local ~with_repos ~with_deps
            ?jobs ?toolchain ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache
            ~data_dir ~registry ~cwd ()
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
      & info ~docv:"CMD" ~doc:"The command to execute." [])
  in
  let args =
    Arg.(
      value & pos_right 0 string []
      & info ~docv:"ARG"
          ~doc:
            "Arguments passed through to $(b,CMD). Use $(b,--) to separate \
             them from $(b,oi)'s own flags."
          [])
  in
  let info =
    Cmd.info "exec" ~doc:"Run a command in the project environment"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Run $(b,CMD) with $(b,_oi/tools/bin) and $(b,_oi/prefix/bin) \
             prepended to $(b,PATH) and the OCaml environment variables \
             pointing at $(b,_oi/prefix/).";
          `P
            "Auto-syncs first when $(b,_oi/prefix/) is missing or older than \
             any $(b,*.opam). $(b,--with), $(b,--with-repo), and \
             $(b,--toolchain) force a re-sync.";
          `S "TOOLCHAIN";
          `P "The active toolchain is picked in this order:";
          `I ("1.", "$(b,--toolchain=NAME), if given.");
          `I
            ( "2.",
              "The $(b,x-oi-toolchain) tag on any in-scope $(b,@HANDLE) — from \
               $(b,--with-repo=@h), $(b,--with=@h/pkg), or the project's \
               $(b,x-repos:). Conflicting tags are an error." );
          `I
            ( "3.",
              "The reporepo entry flagged $(b,x-oi-default-toolchain: true)." );
          `Pre
            "  oi exec dune build\n\
            \  oi exec -- ocamlformat --check .\n\
            \  oi exec utop";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ Terms.skip_local $ Terms.registry $ Terms.with_repos $ Terms.with_deps
      $ Terms.jobs $ Terms.toolchain $ cmd $ args)

(* -- config -------------------------------------------------------------- *)
