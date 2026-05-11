(** [oi config]: inspect and edit persistent [oi] settings (cache locations,
    default remotes, mirror preferences, etc.). The man page enumerating each
    subcommand lives next to the implementation. *)

val cmd : unit Cmdliner.Cmd.t
