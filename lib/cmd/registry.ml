open Cmdliner

[@@@warning "-32"]

let cmd =
  let info =
    Cmd.info "registry"
      ~doc:"Manage the cache of pre-built packages and the remote registry"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Inspect and manage the local cache of pre-built packages and the \
             remote registry it pulls from.";
          `P
            "$(b,list) inspects the local cache. $(b,build) and $(b,export) \
             populate a registry you serve to others. $(b,mirror) handles the \
             companion mirror of upstream source tarballs.";
        ]
  in
  Cmd.group info
    [
      Registry_list.cmd;
      Registry_index.cmd;
      Registry_export.cmd;
      Registry_build.cmd;
      Registry_depexts.cmd;
      Registry_docker_cmd.cmd;
      Registry_mirror.cmd;
    ]
