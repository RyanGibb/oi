(** Schema versions for every on-disk store [oi] persists across runs.

    Every persistent format [oi] writes — the cache dir, the data dir, the XDG
    toolchain dir, the in-key solver memo — has a version constant here. Bump
    the relevant constant when a format changes; on next run {!ensure} detects
    the mismatch and clears the affected store automatically.

    Three of the constants ({!cache_schema}, {!data_schema},
    {!toolchains_schema}) drive whole-store invalidation via [.oi-stamp] files
    at each store's root. The fourth ({!solver_cache_schema}) is folded into the
    per-entry cache key inside [solve-cache/]; bumping it invalidates just the
    solver memos. The reporepo at [<data>/reporepo] is user-authored data and is
    never stamped or auto-cleared.

    Bump policy: bump only when stale on-disk data would mislead new code.
    Additive JSON fields a decoder ignores don't need a bump. *)

val cache_schema : int
(** [<cache>/] format. Bump for [layer.json] / [provenance.json] / [plan.json]
    field renames, prefix-cache layout changes, run-cache key changes, or sqlite
    layer-index schema changes. *)

val data_schema : int
(** [<data>/opam-root/] and [<data>/repos/] layout. Reporepo is NOT swept by
    this bump. *)

val toolchains_schema : int
(** [$XDG_CACHE_HOME/oi/toolchains/<handle>-<version>/] layout. A bump forces
    every previously-built compiler to be rebuilt on first use. *)

val solver_cache_schema : string
(** Per-entry version baked into solver-cache keys at [<cache>/solve-cache/].
    Bump when the solver's input handling changes in a way that would make an
    older memoised result wrong (e.g. a new dependency-quirk filter, a change to
    the package universe shape). Stored as a string for backwards compatibility
    with prior ["v7"]/["v8"] keys on disk. *)

val ensure :
  fs:Eio.Fs.dir_ty Eio.Path.t -> cache:Cache.t -> data_dir:string -> unit
(** Read each store's [.oi-stamp]; for any whose recorded schema is below the
    compiled-in constant, or whose stamp is missing while tracked subpaths exist
    on disk, purge that store with [rmtree] and write a fresh stamp. Prints one
    [▸] line per affected store; silent on the happy path. Safe on a fresh
    install. *)
