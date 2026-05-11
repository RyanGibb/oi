(** Cmdliner terms for the d10ir-only subcommands.

    Kept separate from oi's main cmd library so the terms only depend on the
    d10ir runtime — none of the wider oi pipeline (toolchain resolution, opam
    solving, reporepo) is reachable from here. Composed into the [oi ir] group
    by [Oi_cmd.Ir]; can also be wired into a standalone [d10ir] CLI. *)

val validate_cmd : unit Cmdliner.Cmd.t
(** [d10ir validate DIR]: structural check against the on-disk [DIR/recipe.json]
    — schema version, layer-hash uniqueness, cycle freedom, dep satisfiability
    against the d10 cache, archive presence and sha. *)

val merge_cmd : unit Cmdliner.Cmd.t
(** [d10ir merge RECIPE… -o OUT]: fold several recipes into a single batched
    recipe at $(i,OUT). Nodes dedupe by layer hash; roots union. Fails on
    schema/os/toolchain disagreements. *)

val show_cmd : unit Cmdliner.Cmd.t
(** [d10ir show DIR]: pretty-print the recipe's dependency tree (default) or
    full plan details ($(b,--plan)). *)

val run_cmd : unit Cmdliner.Cmd.t
(** [d10ir run DIR]: execute the recipe at [DIR/recipe.json] via
    {!D10ir.Direct.run}. Per-event progress is emitted to stderr as plain log
    lines; richer UI (progress bars, sparklines) is the caller's responsibility
    — typically [oi]'s [Progress_ui] wraps this for its [oi ir run] entry point.
*)
