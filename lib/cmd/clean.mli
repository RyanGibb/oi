(** [oi clean]: remove cached layers, scratch trees and build outputs for the
    current project (or globally with the appropriate flag). The man page
    describing the exact flags lives next to the implementation. *)

val cmd : unit Cmdliner.Cmd.t
