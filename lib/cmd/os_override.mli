(** Cross-platform [--os] parsing for [oi show] and [oi registry build].

    Resolves the string passed to [--os] into an {!Oi.Solver.Ctx.conf}. The
    parser is {!Dockerfile_opam.Distro.distro_of_tag}, which knows every tag
    dockerfile-opam supports ([alpine-3.23], [ubuntu-22.04], [debian-13],
    [fedora-43], [centos-9], [debian-stable], [archlinux], …) plus bare forms
    ([alpine], [ubuntu], [fedora], [opensuse], …) that map to each distro's
    [Latest] variant. *)

val resolve : Oi.Solver.Ctx.conf -> string -> Oi.Solver.Ctx.conf
(** [resolve host_conf os_str] applies the [--os=…] override to a host-derived
    conf. Tagged distros set (os, os-family, os-distribution, os-version)
    consistently; bare opam values ([linux], [macos], …) only rewrite [os] and
    warn that depext filters keyed on the other variables will not reflect the
    override; truly unknown values warn and pass [os] through verbatim. *)
