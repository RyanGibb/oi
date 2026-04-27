open Cmdliner

let ( / ) = Filename.concat

[@@@warning "-32"]

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
    let {
      Harness.proc_mgr = _proc_mgr;
      fs;
      clock = _clock;
      sys;
      platform;
      os_key = _os_key;
      cache;
    } =
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
        Registry_build.compute_overlay_depexts_per_distro ~fs ~sys ~cache
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
    Fmt.pr "Run the registry build + export:@.";
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
      ~doc:
        "Generate per-distro Dockerfiles and a docker-compose.yml that run oi \
         registry build + export"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry docker) writes a small project that will build \
             $(b,oi) in a static musl container and then run a $(b,registry \
             build --all) followed by a $(b,registry export) once per target \
             distribution. The generated files are $(b,Dockerfile.oi) (the \
             static $(b,oi) builder), one $(b,Dockerfile.<distro>) per target, \
             and a $(b,docker-compose.yml) that ties them together. Each \
             service bind-mounts $(b,./registry) onto $(b,/out) so the \
             resulting layer archives end up on the host.";
          `P
            "The per-distribution images are intentionally generic. They \
             install the required system packages and drop in the \
             statically-linked $(b,oi) binary. The real work is driven from \
             $(b,docker-compose.yml) via a $(b,command:) override. Every \
             container owns its own $(b,oi) state. On first use it clones the \
             reporepo from the built-in default URL (override with \
             $(b,OI_REPOREPO_URL) in the service environment), iterates each \
             overlay's $(b,x-root-packages) list, builds each one as \
             $(b,@HANDLE/PKG), and finishes with $(b,oi registry export /out). \
             The containers do not share state, so they run safely in \
             parallel.";
          `P
            "The resulting tree at $(b,./registry/<os_key>/) carries one \
             archive per layer along with a sqlite $(b,index.db) tagged with \
             each layer's overlay handle and version. Clients can scope to a \
             specific overlay by querying the index directly. Build the whole \
             project with:";
          `Pre "  docker compose up --build";
          `P
            "$(b,compose up) returns once every distribution has finished. The \
             output is ready to serve over static HTTP, or to $(b,rsync) onto \
             a registry server. No further $(b,oi) commands are needed on the \
             server.";
          `S Manpage.s_examples;
          `P "Generate the compose project in the current directory:";
          `Pre "  oi registry docker -o ./registry-build";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ output_dir
      $ src_context $ Terms.refresh)

(* -- registry mirror ------------------------------------------------------ *)
