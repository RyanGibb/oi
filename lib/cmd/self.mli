(** [oi self] — manage the running [oi] binary.

    The [where] subcommand reports the current executable path and whether we
    can write to it; future [update] machinery (downloading a new binary,
    swapping it in, falling back to [~/.local/bin/oi]) will live in this module
    too. *)

val cmd : unit Cmdliner.Cmd.t
