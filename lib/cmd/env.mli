(** [oi env]: print shell exports for the current project's toolchain
    ([PATH], [OCAMLPATH], opam roots, etc.) so a user can [eval $(oi env)] in
    place of activating an opam switch. *)

val cmd : unit Cmdliner.Cmd.t
