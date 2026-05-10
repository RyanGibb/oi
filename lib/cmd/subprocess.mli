(** Subprocess execution helpers.

    [Eio.Process.spawn] uses execvp-style lookup, which resolves bare executable
    names against the {e caller's} PATH — not the PATH inside the [~env]
    argument. {!run} resolves the first token against [env]'s PATH first so the
    child finds binaries from the assembled prefix. *)

val run : _ Eio.Process.mgr -> env:string array -> string list -> int
(** [run proc_mgr ~env cmd] spawns [cmd] under [env], blocks until it exits, and
    returns its exit code (128+signal for signal-terminated children). Never
    raises on non-zero exit. *)
