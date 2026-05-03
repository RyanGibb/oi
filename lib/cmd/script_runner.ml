let ( / ) = Filename.concat

let run ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache ~data_dir
    ?toolchain ?source_remote script_path cli_deps args =
  let file_deps = Oi.Project.Script.parse_deps_from_file ~fs script_path in
  let all_deps = Oi.Project.Script.dedup (file_deps @ cli_deps) in
  if all_deps = [] then
    Oi.Error.msg
      "No dependencies found. Add [@@@opam pkg1 pkg2] to the first line or use \
       --with=pkg";
  let script_hash = Oi.Project.Script.script_hash script_path all_deps in
  let run_dir = Oi.Cache.run_dir cache ~hash:script_hash in
  let run_dir_s = Eio.Path.native_exn run_dir in
  let cached_bin = run_dir_s / "main.exe" in
  if Workspace.path_exists fs cached_bin then
    exit
      (Subprocess.run proc_mgr
         ~env:
           (Oi.Solver.Env.make_env ~prefix
              ~dune_cache_root:(Oi.Cache.dune_root cache) ())
         (cached_bin :: args))
  else begin
    let packages_dirs = Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir () in
    let ocaml_name = OpamPackage.Name.of_string "ocaml" in
    let dep_names =
      List.filter_map
        (fun (d : Oi.Project.Script.dep) ->
          if OpamPackage.Name.equal d.name ocaml_name then None else Some d.name)
        all_deps
    in
    let constraints = Oi.Project.Script.constraints all_deps in
    if dep_names <> [] then begin
      let cache_root = Oi.Cache.root_s cache in
      let build_prefix = cache_root / "build" / "prefix" in
      let tc_ctx = Stdlib.Option.map Oi.Toolchain.opam_ctx_of_info toolchain in
      let ctx =
        Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs ~conf
          ?toolchain:tc_ctx ()
      in
      let pkgs =
        match
          Oi.Solver.solve ~fs ~cache_root ctx ~packages_dirs ~constraints
            dep_names
        with
        | Ok pkgs -> pkgs
        | Error msg ->
            Fmt.epr "No solution: %s@." msg;
            exit 1
      in
      let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
      let plan = Oi.Plan.build ctx ~d10 ~packages_dirs pkgs in
      let exec_plan =
        Oi.Plan.resolve ctx ~packages_dirs ~cache_root ~os_key
          ~ocaml_version:conf.ocaml_version plan
      in
      let cache_urls = Oi.Pipeline.cache_urls ~cache ~source_remote in
      Oi.Execute.run ~cache_urls ~proc_mgr ~fs
        ~clock:(clock :> D10.Config.clk)
        ~sys ~os_key exec_plan
    end;
    let build_dir = run_dir_s in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / build_dir);
    Oi.Project.Script.generate_project ~script:script_path ~deps:all_deps
      ~dir:build_dir;
    let build_env =
      Oi.Solver.Env.make_env ~prefix ~dune_cache_root:(Oi.Cache.dune_root cache)
        ()
    in
    Eio.Process.run proc_mgr ~env:build_env
      [ "/bin/sh"; "-c"; Fmt.str "cd %s && dune build main.exe 2>&1" build_dir ];
    let built = build_dir / "_build" / "default" / "main.exe" in
    if Workspace.path_exists fs built then begin
      let content = Eio.Path.load Eio.Path.(fs / built) in
      Eio.Path.save ~create:(`Or_truncate 0o755)
        Eio.Path.(fs / cached_bin)
        content
    end;
    let exe =
      if Workspace.path_exists fs cached_bin then cached_bin else built
    in
    exit (Subprocess.run proc_mgr ~env:build_env (exe :: args))
  end
