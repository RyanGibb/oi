let log_src = Logs.Src.create "oi.cmd.os_override" ~doc:"oi --os override"

module Log = (val Logs.src_log log_src : Logs.LOG)

let resolve (conf_host : Oi.Solver.Ctx.conf) os_str =
  let known_os_values =
    [ "linux"; "macos"; "freebsd"; "openbsd"; "netbsd"; "win32"; "cygwin" ]
  in
  let opam_os_of_docker_family = function
    | `Linux -> "linux"
    | `Cygwin -> "cygwin"
    | `Windows -> "win32"
  in
  match
    try Dockerfile_opam.Distro.distro_of_tag os_str with Failure _ -> None
  with
  | Some d ->
      let vars = Registry_docker.opam_vars_of_distro d in
      let os =
        opam_os_of_docker_family (Dockerfile_opam.Distro.os_family_of_distro d)
      in
      {
        conf_host with
        os;
        os_family = vars.os_family;
        os_distribution = vars.os_distribution;
        os_version = vars.os_version;
      }
  | None ->
      if not (List.mem os_str known_os_values) then
        Log.warn (fun m ->
            m
              "--os=%s is not a known dockerfile-opam distro tag or opam os \
               value; only the 'os' variable was changed so depext filters \
               keyed on os-family or os-distribution will not reflect this \
               override. Try a tagged form such as debian-13, centos-9, or \
               alpine-3.23."
              os_str);
      { conf_host with os = os_str }
