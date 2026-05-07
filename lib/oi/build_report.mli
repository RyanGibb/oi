(** Per-package events emitted during a build, consumed by the
    [oi build --all] progress UI.

    Decoupled from the executor: today {!D10ir.Direct.run} runs the
    layers, but [oi build --all]'s solve-group loop also fires
    cycle-skip and fully-cached fast-path events that aren't tied to
    any executor. Those callers want a single typed channel for the
    UI counters. *)

type pkg_event =
  | Started of { pkg : string; phase : string }
  | Cached of { pkg : string }
  | Built of { pkg : string }
  | Build_failed of { pkg : string; log : string }
  | Dep_failed of { pkg : string; upstream_log : string }
  | Install_failed of { pkg : string; log : string }

type reporter = { pkg_event : pkg_event -> unit }
