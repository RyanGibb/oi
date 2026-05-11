(** [oi exec]: run an arbitrary command inside the current project's activated
    environment (the same one [oi env] prints), making sure the toolchain has
    been provisioned first. The man page lives next to the implementation. *)

val cmd : unit Cmdliner.Cmd.t
