open Cmdliner

let ( / ) = Filename.concat

let cmd =
  let default_distros : Registry_docker.Distro.t list =
    [
      `Alpine `Latest;
      `Debian `Stable;
      `Ubuntu `V24_04;
      `Ubuntu `V25_10;
      `Fedora `Latest;
    ]
  in
  let run () data_dir cache_dir output_dir src_context refresh =
    Harness.run @@ fun env ->
    let { Harness.fs; sys; platform; cache; _ } =
      Harness.bootstrap env cache_dir
    in
    (try Unix.mkdir output_dir 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
    let df_oi = Registry_docker.dockerfile_oi ~src_context in
    let oi_path = output_dir / "Dockerfile.oi" in
    Registry_docker.write_dockerfile oi_path df_oi;
    Fmt.pr "Computing overlay depexts for %d distros...@."
      (List.length default_distros);
    let per_distro_depexts =
      try
        Build.compute_overlay_depexts_per_distro ~fs ~sys ~cache
          ~data_dir ~refresh ~platform ~distros:default_distros
      with _ -> List.map (fun d -> (d, [])) default_distros
    in
    let per_distro_paths =
      List.map
        (fun d ->
          let fname = Registry_docker.one_distro_filename d in
          let path = output_dir / fname in
          let overlay_depexts =
            Stdlib.Option.value
              (List.assoc_opt d per_distro_depexts)
              ~default:[]
          in
          let df = Registry_docker.dockerfile_one_distro ~overlay_depexts d in
          Registry_docker.write_dockerfile path df;
          (d, path, List.length overlay_depexts))
        default_distros
    in
    let compose_path = output_dir / "docker-compose.yml" in
    let compose_yaml =
      Registry_docker.docker_compose_yaml ~distros:default_distros
        ~registry_host_path:"./registry" ()
    in
    Registry_docker.write_file compose_path compose_yaml;
    Fmt.pr "Wrote:@.";
    Fmt.pr "  %s@." oi_path;
    List.iter
      (fun (_, path, n) ->
        if n = 0 then Fmt.pr "  %s@." path
        else Fmt.pr "  %s  (%d overlay depexts)@." path n)
      per_distro_paths;
    Fmt.pr "  %s@." compose_path;
    Fmt.pr "@.";
    Fmt.pr "Static oi release binary:@.";
    Fmt.pr "  docker buildx build -f %s --output type=local,dest=./oi-bin .@."
      oi_path;
    Fmt.pr "Run the build + export:@.";
    Fmt.pr "  docker compose up --build   # writes ./registry/<os_key>/@."
  in
  let output_dir =
    Arg.(
      value & opt string "."
      & info ~docv:"DIR"
          ~doc:
            "The directory to write the Dockerfiles and the \
             $(b,docker-compose.yml) into. Created if it does not already \
             exist."
          [ "o"; "output" ])
  in
  let src_context =
    Arg.(
      value & opt string "."
      & info ~docv:"DIR"
          ~doc:
            "Path to the $(b,oi) source tree, relative to the Docker build \
             context. Defaults to the context root."
          [ "src" ])
  in
  let info =
    Cmd.info "docker"
      ~doc:"Generate a multi-distro registry build compose project"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Writes $(b,Dockerfile.oi) (static musl $(b,oi) builder), one \
             $(b,Dockerfile.<distro>) per target distribution, and a \
             $(b,docker-compose.yml) that ties them together. Each service \
             bind-mounts $(b,./registry) on $(b,/out) and runs $(b,oi build \
             --all --export /out). Containers own their own state and run \
             in parallel; the resulting tree is ready for static HTTP or \
             $(b,rsync).";
          `P
            "Override the upstream reporepo URL via $(b,OI_REPOREPO_URL) in \
             the service environment.";
          `S Manpage.s_examples;
          `Pre
            "  oi registry docker -o ./registry-build\n\
            \  cd ./registry-build && docker compose up --build";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ output_dir
      $ src_context $ Terms.refresh)

(* -- registry mirror ------------------------------------------------------ *)
