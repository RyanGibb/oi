open Cmdliner

let cmd =
  let run () data_dir cache_dir refresh os_override =
    Harness.run @@ fun env ->
    let { Harness.fs; sys; platform; cache; _ } =
      Harness.bootstrap env cache_dir
    in
    let host_conf =
      Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    let conf =
      match os_override with
      | None -> host_conf
      | Some os -> Os_override.resolve host_conf os
    in
    let names =
      Registry_build.compute_overlay_depexts_for_conf ~fs ~sys ~cache ~data_dir
        ~refresh ~conf
    in
    List.iter (fun n -> Fmt.pr "%s@." n) names
  in
  let os_override =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"NAME"
          ~doc:
            "Evaluate depext filters under the named distribution rather than \
             the host. Accepts any tag $(b,Dockerfile_opam.Distro) recognises \
             ($(b,alpine-3.23), $(b,debian-13), $(b,fedora-43), \
             $(b,ubuntu-25.10), …) and the bare opam $(b,os) values \
             ($(b,linux), $(b,macos), $(b,freebsd), $(b,win32))."
          [ "os" ])
  in
  let info =
    Cmd.info "depexts"
      ~doc:"Print the union of overlay depexts for the current platform"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solves every overlay's $(b,x-root-packages) once and prints the \
             union of $(b,depexts:) declared by the resulting package set, \
             evaluated against the active opam filter variables. The output \
             is one package name per line, suitable for piping into a \
             package manager.";
          `P
            "Equivalent to what $(b,oi registry docker) bakes into each \
             generated Dockerfile, but for the current host (or any \
             $(b,--os=)-overridden one). The intended audience is CI \
             workflows that build a registry on macOS or another OS without \
             a generated Dockerfile to drive the install.";
          `S Manpage.s_examples;
          `Pre
            "  brew install \\$(oi registry depexts)\n\
            \  oi registry depexts --os=alpine-3.23\n\
            \  apt-get install -y \\$(oi registry depexts \
             --os=ubuntu-25.10)";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ os_override)
