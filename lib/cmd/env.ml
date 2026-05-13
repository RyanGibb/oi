open Cmdliner

let ( / ) = Filename.concat

let cmd =
  let run (c : Terms.common) refresh skip_local with_repos with_deps jobs
      toolchain =
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
    let dune_cache_root = Oi.Cache.dune_root cache in
    (* Detect _oi/ project directory. A pre-existing _oi/prefix is
       reused as-is UNLESS the user passes --with-repo or --with, which
       demands a fresh solve to honour the additions. [--skip-local]
       forces a one-shot prefix and ignores any existing [_oi/prefix]. *)
    let cwd_s, cwd = Workspace.resolved_cwd fs in
    let oi_prefix = cwd_s / "_oi" / "prefix" in
    let want_extras =
      skip_local || with_repos <> [] || with_deps <> [] || toolchain <> None
    in
    let conf =
      Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    (* Use the same project-aware resolve [oi sync] uses, so [oi env]
       picks the toolchain the existing prefix was actually built with
       (project [x-repos:] declarations and all). *)
    let tc_info =
      Sync.resolve_project_toolchain ~refresh ~skip_local ~with_repos ~with_deps
        ~fs ~sys ~cache ~data_dir ~conf ~install:true ~override:toolchain
        ~cwd:cwd_s ()
    in
    let conf, tc_ctx = Oi.Pipeline.solver_inputs tc_info conf in
    let prefix =
      if (not want_extras) && Workspace.path_exists cwd "_oi/prefix" then
        oi_prefix
      else begin
        (* Fall back to a minimal compiler-only prefix, optionally
           extended with CLI extras + with-deps. *)
        Oi.Pipeline.init_opam_root ~fs ~data_dir;
        ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
        let extra_cli, url_project =
          Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
        in
        let url_overlays =
          Oi.Pipeline.filter_compatible_overlays
            ~reporepo_path:(Terms.reporepo_path ()) ~toolchain:tc_info
            url_project.overlays
        in
        let extras =
          Target.merge_extras
            ~cli:
              (Target.cli_extra_repos ~fs ~sys ?toolchain:tc_info
                 (with_repos @ url_overlays))
            ~project:url_project.extra_repos
        in
        let extra_constraints = Oi.Project.Script.constraints extra_cli in
        let extra_names =
          List.filter_map
            (fun (d : Oi.Project.Script.dep) ->
              if OpamPackage.Name.to_string d.name = "ocaml" then None
              else Some d.name)
            extra_cli
          @ List.map OpamPackage.Name.of_string url_project.roots
        in
        let pipeline_env : Oi.Build_pipeline.env =
          { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
        in
        let names =
          "ocaml" :: List.map OpamPackage.Name.to_string extra_names
        in
        let req : Oi.Build_pipeline.request =
          {
            targets = [ Group { tokens = names; handles = [] } ];
            with_repos = [];
            pins = url_project.pins;
            extra_repos = extras;
            constraints = extra_constraints;
            toolchain_override = None;
            toolchain = tc_info;
            conf;
            local_packages_dir = url_project.packages_dir;
            project_root = Some cwd_s;
            force_source = false;
            refresh;
          }
        in
        let solved = Oi.Build_pipeline.solve pipeline_env req in
        let _ : D10ir.Direct.result option =
          Oi.Build_pipeline.build pipeline_env
            {
              solved;
              layer_remote = None;
              source_remote = None;
              jobs;
              upload_archive_url = None;
            }
        in
        Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key
          ~layer_hashes:(Oi.Build_pipeline.layer_hashes solved)
      end
    in
    let vars =
      Oi.Solver.Env.env_vars ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
    in
    let current_path =
      try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin"
    in
    let tools = Workspace.tools_dir_for ~cwd:cwd_s in
    let path_prefix =
      match tools with
      | None -> prefix / "bin"
      | Some t -> (t / "bin") ^ ":" ^ (prefix / "bin")
    in
    List.iter
      (fun (k, v) ->
        let v = if k = "PATH" then path_prefix ^ ":" ^ current_path else v in
        Fmt.pr "export %s=\"%s\"@." k v)
      vars
  in
  let info =
    Cmd.info "env" ~doc:"Print shell exports for the project environment"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Print $(b,export) statements for $(b,PATH), $(b,OCAMLLIB), and \
             the other OCaml variables pointing at $(b,_oi/prefix/). \
             Non-$(b,direnv) equivalent of $(b,.envrc).";
          `Pre "  eval \"\\$(oi env)\"";
          `P
            "Reuses $(b,_oi/prefix/) when it exists and no extras are \
             requested. With $(b,--with), $(b,--with-repo), $(b,--toolchain), \
             $(b,--skip-local), or a missing prefix, builds a one-shot prefix \
             in-process; $(b,.envrc) and dev tools are not updated. Use $(b,oi \
             build --deps-only) for that.";
          `S "TOOLCHAIN";
          `P "Active toolchain, in order:";
          `I ("1.", "$(b,--toolchain=NAME).");
          `I
            ( "2.",
              "$(b,x-oi-toolchain) on any in-scope $(b,@HANDLE) — from \
               $(b,--with-repo=@h), $(b,--with=@h/pkg), or the project's \
               $(b,x-repos:). Conflicting tags error out." );
          `I ("3.", "Reporepo entry flagged $(b,x-oi-default-toolchain: true).");
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.skip_local
      $ Terms.with_repos $ Terms.with_deps $ Terms.jobs $ Terms.toolchain)
