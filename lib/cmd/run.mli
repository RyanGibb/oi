(** [oi run] and the standalone [oix] tool runner. *)

val cmd : unit Cmdliner.Cmd.t
(** The [oi run] subcommand. Honours [--skip-local] from the user. *)

val cmd_x : unit Cmdliner.Cmd.t
(** Top-level command for the [oix] binary: same logic as [oi run] but with
    [--skip-local] forced on (and the flag itself omitted from the help). *)
