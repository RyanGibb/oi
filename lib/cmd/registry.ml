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
            "$(b,oi) keeps a local cache of pre-built OCaml packages so it can \
             avoid repeated work, and it can pull pre-built packages from a \
             remote registry instead of compiling them. This group of commands \
             inspects and manages both sides of that arrangement.";
          `P
            "Most users only need $(b,oi registry list) to inspect the local \
             cache. The $(b,build) and $(b,export) subcommands come in when \
             you are running your own registry for a team or a set of \
             machines. The $(b,mirror) subgroup handles the companion mirror \
             of upstream source tarballs.";
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
