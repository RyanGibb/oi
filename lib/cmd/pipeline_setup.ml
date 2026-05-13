type prepared = {
  env : Oi.Build_pipeline.env;
  request : Oi.Build_pipeline.request;
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
  toolchain : Oi.Toolchain.info option;
  cwd : string;
}

(* Pull every [@handle] reference out of a parsed target list — used as a
   seed for toolchain detection. [Group { handles; _ }] carries an explicit
   handle list (the call site that builds [Group] already split it out);
   [Overlay_pkg]/[Overlay_all] are sugar for a one-handle Group. *)
let target_handles (targets : Oi.Build_pipeline.target list) : string list =
  List.concat_map
    (fun (t : Oi.Build_pipeline.target) ->
      match t with
      | Plain _ -> []
      | Group { handles; _ } -> handles
      | Overlay_pkg { handle; _ } -> [ handle ]
      | Overlay_all h -> [ h ])
    targets

let prepare ~(harness : Harness.env) ~refresh ~locked ~skip_local ~registry
    ~use_registry ~with_repos ~with_deps ~toolchain_override ~targets
    ?(extra_handles = []) ?(extra_pins = [])
    ?(extra_constraints = OpamPackage.Name.Map.empty) () : prepared =
  let {
    Harness.proc_mgr;
    fs;
    clock;
    sys;
    platform;
    os_key;
    cache;
    data_dir;
    http_session;
    _;
  } =
    harness
  in
  let refresh = refresh && not locked in
  let use_registry = if locked then Oi.Use_registry.Never else use_registry in
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let { Terms.layer_remote; source_remote } =
    Terms.remotes_of ~url:registry ~mode:use_registry
  in
  (* URL projects (e.g. [--with=https://github.com/foo/bar#tag]) are
     cloned into the pin cache and contribute pins / solver roots /
     overlays / extra_repos. *)
  let extra_deps_loaded, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let with_deps_constraints = Oi.Project.Script.constraints extra_deps_loaded in
  let cwd_s, _ = Workspace.resolved_cwd fs in
  (* Project metadata (if any [*.opam] in cwd) — pins / overlays /
     extras feed the solve. Sys / IO errors degrade to "no project"
     rather than aborting (a missing cwd, an unreadable dir): the
     command might still have a valid TARGET passed in. *)
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
  let tc_handles =
    extra_handles @ target_handles targets
    @ Target.handles_of_tokens with_repos
    @ project_overlays
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
      ~override:toolchain_override ~handles:tc_handles ()
  in
  (* When the toolchain was auto-picked, drop project overlays tagged
     for a different one. When the user passed [--toolchain=NAME]
     ([toolchain_override = Some _]), keep every declared overlay
     verbatim — they explicitly overrode the project's preference, so
     we shouldn't second-guess by silently filtering. *)
  let project_overlays =
    Oi.Pipeline.filter_compatible_overlays
      ~reporepo_path:(Terms.reporepo_path ()) ~override:toolchain_override
      ~toolchain project_overlays
  in
  let with_repos = project_overlays @ with_repos in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
  let all_extras =
    Target.merge_extras ~cli:cli_extras ~project:project_extras
  in
  let pins = project_pins @ extra_pins in
  let constraints =
    OpamPackage.Name.Map.union
      (fun a _ -> a)
      extra_constraints with_deps_constraints
  in
  let env : Oi.Build_pipeline.env =
    { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
  in
  let request : Oi.Build_pipeline.request =
    {
      targets;
      with_repos;
      pins;
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
  { env; request; layer_remote; source_remote; toolchain; cwd = cwd_s }
