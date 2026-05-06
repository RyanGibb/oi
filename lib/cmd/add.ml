open Cmdliner

let ( / ) = Filename.concat

let constr_to_op_ver (op, ver) =
  (OpamFormula.string_of_relop op, OpamPackage.Version.to_string ver)

let cmd =
  let run (c : Terms.common) refresh registry use_registry with_repos toolchain
      package pkg_spec =
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
    (* Fail fast before the sync's 10-second repo refresh. *)
    let dp = Oi.Project.Dune.load ~fs ~cwd in
    if not (Oi.Project.Dune.generate_opam_files dp) then
      Oi.Error.config_error
        "dune-project does not have (generate_opam_files): oi add only \
         supports projects where dune owns the *.opam files";
    (match (package, Oi.Project.Dune.package_names dp) with
    | Some p, names when not (List.mem p names) ->
        Oi.Error.config_error
          "no (package (name %s) …) stanza in dune-project (declared: %s)" p
          (if names = [] then "none" else String.concat ", " names)
    | Some _, _ | _, [ _ ] | _, [] -> ()
    | None, many ->
        Oi.Error.config_error
          "multiple packages in dune-project (%s); re-run with -p PKG to pick \
           one"
          (String.concat ", " many));
    let dep = Oi.Project.Script.parse_cli_dep pkg_spec in
    let dep_name = OpamPackage.Name.to_string dep.name in
    let op_ver = Stdlib.Option.map constr_to_op_ver dep.constraint_ in
    let render_constraint = function
      | None -> ""
      | Some (op, ver) -> Fmt.str " %s %s" op ver
    in
    (* Phase 1: prove the solve succeeds with the new dep included. If
       solve fails, [Sync.run] raises before we touch any project files. *)
    Fmt.pr "Solving %s%s into the project...@." dep_name
      (render_constraint op_ver);
    ignore
      (Sync.run ~refresh ~with_repos ~with_deps:[ pkg_spec ] ?toolchain
         ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache ~data_dir ~registry
         ~use_registry ~session:http_session ~cwd ());
    (* Phase 2: edit dune-project. Reload in case something touched it
       during the sync (shouldn't, but cheap to be defensive). *)
    let dp = Oi.Project.Dune.load ~fs ~cwd in
    let dp' =
      Oi.Project.Dune.add_dependency dp ?package ~name:dep_name
        ~constraint_:op_ver ()
    in
    Oi.Project.Dune.save ~fs dp';
    Fmt.pr "Updated dune-project: added %s%s@." dep_name
      (render_constraint op_ver);
    (* Phase 3: regenerate *.opam via dune build inside the assembled
       prefix — dune itself comes from [_oi/prefix/bin]. *)
    let prefix = cwd / "_oi" / "prefix" in
    let tools = Workspace.tools_dir_for ~cwd in
    let env =
      Oi.Solver.Env.make_env ~prefix ?tools
        ~dune_cache_root:(Oi.Cache.dune_root cache) ()
    in
    Fmt.pr "Running dune build to regenerate *.opam...@.";
    ( Eio.Switch.run @@ fun sw ->
      let child =
        Eio.Process.spawn ~sw proc_mgr ~env
          ~cwd:Eio.Path.(fs / cwd)
          [ prefix / "bin" / "dune"; "build" ]
      in
      match Eio.Process.await child with
      | `Exited 0 -> ()
      | `Exited n ->
          Oi.Error.msg
            "dune build exited with code %d; dune-project was updated but \
             *.opam regeneration failed"
            n
      | `Signaled n -> Oi.Error.msg "dune build killed by signal %d" n );
    (* Phase 4: re-sync so the prefix reflects the committed *.opam. *)
    Fmt.pr "Re-syncing to pick up regenerated *.opam...@.";
    ignore
      (Sync.run ~quiet:true ~refresh:false ~with_repos ~with_deps:[]
         ?toolchain ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache ~data_dir
         ~registry ~use_registry ~session:http_session ~cwd ());
    Fmt.pr "Done.@."
  in
  let pkg_spec =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PKG"
          ~doc:
            "The opam package to add. A plain name ($(b,fmt)) takes the \
             version the solver picks; a dotted form ($(b,fmt.0.9.5)) or a \
             relop ($(b,fmt>=0.9)) pins the dependency to a specific version."
          [])
  in
  let package =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"NAME"
          ~doc:
            "The name of the $(b,\\(package …\\)) stanza in $(b,dune-project) \
             that receives the new dependency. Required only when the project \
             declares more than one package."
          [ "p"; "package" ])
  in
  let info =
    Cmd.info "add" ~doc:"Add a new dependency to the current project"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Bring $(b,PKG) into the current project in four steps: solve with \
             $(b,PKG) added; if the solve succeeds, edit $(b,dune-project); \
             run $(b,dune build) so dune regenerates $(b,*.opam); re-sync to \
             reconcile the prefix.";
          `P
            "Failed solves leave the tree untouched, so $(b,oi add) doubles as \
             a compatibility probe.";
          `P
            "Requires $(b,\\(generate_opam_files\\)) in $(b,dune-project). \
             Pass $(b,-p NAME) to pick a stanza when the project declares more \
             than one package.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.with_repos $ Terms.toolchain $ package
      $ pkg_spec)

(* -- exec ---------------------------------------------------------------- *)
