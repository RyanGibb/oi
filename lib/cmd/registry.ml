open Cmdliner

let cmd =
  let info =
    Cmd.info "registry"
      ~doc:"Pre-fetch sources and generate registry build images"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,docker) generates a multi-distro registry-build compose \
             project. $(b,mirror) pre-fetches the source tarballs the \
             reporepo references.";
          `P
            "Build, publish, depexts, and per-overlay listing all live \
             on $(b,oi build) / $(b,oi show) directly.";
        ]
  in
  Cmd.group info
    [
      Registry_docker_cmd.cmd;
      Registry_mirror.cmd;
    ]
