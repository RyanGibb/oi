(** [oi install]: install the binaries from a solved root layer (or the cwd's
    [_build/install/default] tree, in project mode) into a target prefix
    (defaults to [$HOME/.local]).

    Pre-flight checks every [bin/]/[sbin/] file for conflicts and aborts unless
    [--force] is set; prints a [$PATH] hint with shell-specific persisted-form
    when the bin dir isn't on the user's PATH. *)

val cmd : unit Cmdliner.Cmd.t
