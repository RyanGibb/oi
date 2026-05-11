(** Fetch the published [<remote>/<os_key>/index.db] and project the [layers]
    rows that have a tarball published into the [Layer.remote_index] map.

    Lives in its own module so [Index.rebuild] can call [Layer.load_meta]
    without closing a dependency cycle when the remote-fetch path needs both
    [Layer] and [Index]. *)

val fetch :
  Config.t ->
  session:Sysops.Http.session ->
  remote:Layer.remote ->
  Layer.remote_index
(** [fetch c ~session ~remote] downloads [<remote>/<os_key>/index.db] once per
    process and returns the map from layer hash to its tarball SHA-256 + size.
    Bin-index registries (no published tarballs) yield an empty map; failed
    fetches do too. Re-entries with the same [(remote, os_key)] return the
    cached map without a second HTTP round-trip.

    [Layer.fetch_remote_index] is a thin alias for this function. *)
