open Cmdliner

let ( / ) = Filename.concat

(* [--distro=NAME] parsed via dockerfile-opam. Defaults to debian-stable. *)
let parse_distro tag =
  match Dockerfile_opam.Distro.distro_of_tag tag with
  | Some d -> Ok d
  | None -> Error (`Msg (Fmt.str "unknown distro tag: %s" tag))
  | exception _ -> Error (`Msg (Fmt.str "unknown distro tag: %s" tag))

let pp_distro ppf d =
  Fmt.string ppf
    (Dockerfile_opam.Distro.tag_of_distro
       (Dockerfile_opam.Distro.resolve_alias d :> Dockerfile_opam.Distro.t))

let distro_tag d =
  Dockerfile_opam.Distro.tag_of_distro
    (Dockerfile_opam.Distro.resolve_alias d :> Dockerfile_opam.Distro.t)

(* Per-distro list driving [--all]. Sized so each release CI run produces
   the canonical registry tree. *)
let default_distros : Registry_docker.Distro.t list =
  [
    `Alpine `Latest;
    `Debian `Stable;
    `Ubuntu `V24_04;
    `Ubuntu `V25_10;
    `Fedora `Latest;
  ]

(* Project-mode emit: one Dockerfile that runs [cmd] (e.g. [oi build]
   or [oi test]) inside [distro]. Filename encodes both the action and
   the distro so multiple variants coexist in the same dir. *)
let emit_project ~cmd ~suffix ~tag_label ~generator ~cwd_s ~distro ~output =
  let tag = distro_tag distro in
  let path =
    match output with
    | Some p -> p
    | None -> cwd_s / Fmt.str "Dockerfile.oi-%s.%s" suffix tag
  in
  let df = Registry_docker.dockerfile_project ~cmd ~generator distro in
  Registry_docker.write_dockerfile path df;
  Oi.Say.step "Wrote %s" path;
  Oi.Say.info "build with: docker build -t %s -f %s %s" tag_label path cwd_s

(* Multi-distro registry build project: one [Dockerfile.<distro>] per
   target distribution, plus [Dockerfile.oi] (static oi builder) and
   [docker-compose.yml]. Each service runs [oi build --all --export
   /out]. *)
let emit_all ~fs ~sys ~platform ~cache ~data_dir ~refresh ~src_context ~output
    () =
  (try Unix.mkdir output 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  let df_oi = Registry_docker.dockerfile_oi ~src_context in
  let oi_path = output / "Dockerfile.oi" in
  Registry_docker.write_dockerfile oi_path df_oi;
  Oi.Say.step "Computing overlay depexts for %d distros"
    (List.length default_distros);
  let per_distro_depexts =
    try
      Build.compute_overlay_depexts_per_distro ~fs ~sys ~cache ~data_dir
        ~refresh ~platform ~distros:default_distros
    with _ -> List.map (fun d -> (d, [])) default_distros
  in
  let per_distro_paths =
    List.map
      (fun d ->
        let fname = Registry_docker.one_distro_filename d in
        let path = output / fname in
        let overlay_depexts =
          Stdlib.Option.value (List.assoc_opt d per_distro_depexts) ~default:[]
        in
        let df = Registry_docker.dockerfile_one_distro ~overlay_depexts d in
        Registry_docker.write_dockerfile path df;
        (d, path, List.length overlay_depexts))
      default_distros
  in
  let compose_path = output / "docker-compose.yml" in
  let compose_yaml =
    Registry_docker.docker_compose_yaml ~distros:default_distros
      ~registry_host_path:"./registry" ()
  in
  Registry_docker.write_file compose_path compose_yaml;
  Oi.Say.step "Wrote";
  Oi.Say.info "%s" oi_path;
  List.iter
    (fun (_, path, n) ->
      if n = 0 then Oi.Say.info "%s" path
      else
        Oi.Say.info "%s  %a" path Oi.Style.dim_string
          (Fmt.str "(%d overlay depexts)" n))
    per_distro_paths;
  Oi.Say.info "%s" compose_path;
  Oi.Say.newline ();
  Oi.Say.step "Static oi release binary";
  Oi.Say.info "docker buildx build -f %s --output type=local,dest=./oi-bin ."
    oi_path;
  Oi.Say.step "Run the build + export";
  Oi.Say.info "docker compose up --build   %a" Oi.Style.dim_string
    "# writes ./registry/<os_key>/"

let cmd =
  let run (c : Terms.common) refresh test_mode all distro src_context output =
    Harness.run @@ fun ~sw env ->
    let { Harness.fs; sys; platform; cache; _ } =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let data_dir = c.data_dir in
    if test_mode && all then
      Oi.Error.config_error "oi docker: --test and --all are mutually exclusive";
    let cwd_s, _ = Workspace.resolved_cwd fs in
    if all then
      let dir = Stdlib.Option.value output ~default:cwd_s in
      emit_all ~fs ~sys ~platform ~cache ~data_dir ~refresh ~src_context
        ~output:dir ()
    else if test_mode then
      emit_project ~cmd:"oi test" ~suffix:"test" ~tag_label:"my-project-test"
        ~generator:"oi docker --test" ~cwd_s ~distro ~output
    else
      emit_project ~cmd:"oi build" ~suffix:"build" ~tag_label:"my-project"
        ~generator:"oi docker" ~cwd_s ~distro ~output
  in
  let test_mode =
    Arg.(
      value & flag
      & info ~doc:"Emit a Dockerfile running $(b,oi test) instead of $(b,oi build)."
          [ "test" ])
  in
  let all =
    Arg.(
      value & flag
      & info
          ~doc:
            "Emit multi-distro project: $(b,Dockerfile.oi), one \
             $(b,Dockerfile.<distro>) per target, and $(b,docker-compose.yml)."
          [ "all" ])
  in
  let distro =
    let distro_conv =
      Cmdliner.Arg.conv ~docv:"DISTRO" (parse_distro, pp_distro)
    in
    Arg.(
      value
      & opt distro_conv (`Debian `Stable)
      & info ~docv:"DISTRO"
          ~doc:
            "Base distro tag (e.g. $(b,debian-13), $(b,alpine-3.23), \
             $(b,fedora-43)). Ignored with $(b,--all)."
          [ "distro" ])
  in
  let src_context =
    Arg.(
      value & opt string "."
      & info ~docv:"DIR"
          ~doc:
            "Path to the $(b,oi) source tree within the Docker build context. \
             $(b,--all) only."
          [ "src" ])
  in
  let output =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"PATH"
          ~doc:
            "Output path. Default file: $(b,Dockerfile.oi-{build,test}.<distro>) \
             in cwd. With $(b,--all): directory for the project (default: cwd)."
          [ "o"; "output" ])
  in
  let info =
    Cmd.info "docker" ~doc:"Generate Dockerfiles for project builds and CI."
      ~man:
        [
          `S Manpage.s_description;
          `P "Emit Dockerfiles for building or testing the current project.";
          `I
            ( "(default)",
              "$(b,Dockerfile.oi-build.<distro>) running $(b,oi build)." );
          `I
            ( "$(b,--test)",
              "$(b,Dockerfile.oi-test.<distro>) running $(b,oi test)." );
          `I
            ( "$(b,--all)",
              "$(b,Dockerfile.oi), per-distro Dockerfiles, and \
               $(b,docker-compose.yml). Each service runs $(b,oi build --all \
               --export /out)." );
          `S Manpage.s_examples;
          `Pre
            "  oi docker\n\
            \  oi docker --test --distro=alpine-3.23\n\
            \  oi docker --all -o ./registry-build\n\
            \  cd ./registry-build && docker compose up --build";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ test_mode $ all $ distro
      $ src_context $ output)
