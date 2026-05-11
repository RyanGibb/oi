(** [oi show]: render a solver-resolved view of the current project — root
    set, the dependency tree below each root, layer hashes, install paths, and
    other diagnostic detail. The man page describing the flags lives next to
    the implementation. *)

val cmd : unit Cmdliner.Cmd.t
