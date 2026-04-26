open Cmdliner

let ( / ) = Filename.concat

let cmd =
  let run () data_dir cache_dir refresh registry with_repos with_deps jobs
      toolchain cmd args =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache } =
      Harness.bootstrap env cache_dir
    in
    let cwd, _ = Workspace.resolved_cwd fs in
    let prefix = cwd / "_oi" / "prefix" in
    (* Any --with-repo / --with / --toolchain flag forces a re-sync
       even if the prefix is fresh, so the extras and toolchain make
       it into the build. *)
    let forced = with_repos <> [] || with_deps <> [] || toolchain <> None in
    if forced || Sync.needs_sync ~cwd ~prefix then begin
      Logs.info (fun m -> m "Syncing %s before exec" cwd);
      ignore
        (Sync.do_sync ~quiet:true ~refresh ~with_repos ~with_deps ?jobs ?toolchain
           ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache ~data_dir
           ~registry ~cwd ())
    end;
    let tools = Workspace.tools_dir_for ~cwd in
    let conf = Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version in
    let tc_info =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:false
        toolchain
    in
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
            "Run $(b,CMD) in the same environment $(b,oi sync) sets up: \
             $(b,_oi/prefix/bin) first on $(b,PATH), OCaml env vars pointing \
             at the prefix, dev tools ($(b,ocamlformat), \
             $(b,ocaml-lsp-server), $(b,odoc), $(b,merlin), $(b,mdx)) on \
             $(b,PATH).";
          `P
            "Auto-syncs first when the prefix is missing or older than any \
             $(b,*.opam). $(b,--with), $(b,--with-repo), and $(b,--toolchain) \
             force a re-sync.";
          `Pre
            "  oi exec dune build\n\
            \  oi exec -- ocamlformat --check .\n\
            \  oi exec utop";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ Terms.registry $ Terms.with_repos $ Terms.with_deps $ Terms.jobs
      $ Terms.toolchain $ cmd $ args)

(* -- config -------------------------------------------------------------- *)

