(** [oi docker]: generate Dockerfiles. Project build (default), project test
    ($(b,--test)), or multi-distro registry build project ($(b,--all)). *)

val cmd : unit Cmdliner.Cmd.t
