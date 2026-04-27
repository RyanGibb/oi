(** [oi registry depexts]: print the union of overlay depexts for the host
    platform (or the [--os=]-overridden one), one per line.

    Used by CI workflows that need to install system packages before
    [oi registry build --all] runs — the macOS GitHub-Actions job pipes the
    output through [brew install], paralleling what [oi registry docker] bakes
    into each Linux Dockerfile. *)

val cmd : unit Cmdliner.Cmd.t
