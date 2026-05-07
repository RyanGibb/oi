(** Unified multi-line progress UI for [oi build] / [oi run] /
    [oi ir run].

    Consumes {!Oi.Build_progress.event}s emitted by lib/oi
    subsystems and renders them as a single multi-line
    [Progress.Display] with three zones:

    {v
       phase  [█████-----] 12/86  (running 4)         <- aggregate
       ── plan ──
       tangled.dev          ◉ build       3.2s        <- per-package
       ├── atp-identity.dev  ✓                         dep tree, status
       │   ├── atp.dev       ◉ build      12.1s        column updates
       │   │   ├── base64.3.5.2  ✓ (cached)            in place
       │   │   └── dune.3.23.0   ✓ (cached)
       └── ↰ ocaml.5.4.1    ✓
    v}

    The dep tree is materialised on the [Plan_ready] event; until
    then only the aggregate row is shown (covering [Solving] /
    [Fetching] phases). *)

val with_ui :
  ?target:string ->
  clock:[ `Clock of float ] Eio.Resource.t ->
  enabled:bool ->
  (Oi.Build_progress.reporter -> 'a) ->
  'a
(** [with_ui ~clock ~enabled f] runs [f reporter] with a configured
    multi-line progress UI driving off [reporter]'s events. The bar
    and any per-package rows are torn down cleanly when [f]
    returns.

    [target] is the user-facing target label (e.g. ["doi2bib"],
    ["@avsm/bsky"]) shown in the aggregate row's phase column. The
    phase short-tag (["build"], ["fetch"], …) is prepended.

    [enabled = false] (typically [not (Tty.is_tty ())]) returns the
    null reporter and runs [f] directly — no display, no escape
    codes, identical control flow. *)
