(** Pin-depends materialisation. Was [Source.Pin] in [source.mli]. *)

val materialize :
  ?reporter:Build_progress.reporter ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Cache.t ->
  ?refresh:bool ->
  Project.pin list ->
  string option
(** Returns [Some packages_dir] (an absolute path to a synthesized [packages/]
    tree) when [pins] is non-empty, [None] when [pins = []].

    [?reporter] receives a [Status "Materialising N pin(s)"] event followed by
    one [Status "Fetching pin <pkg>"] per pin and an
    [Aggregate { phase = Fetching; total; current }] tick after each pin is
    materialised. *)

val resolve_pins :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  ?project_root:string ->
  Project.pin list ->
  Project.pin list
(** Pin the URL of every entry in [pins] to a deterministic form (sha-pinned git
    URL or unchanged tarball), consulting/refreshing
    [<project_root>/_oi/oi.lock] when supplied. With no [project_root], or with
    an empty list, the input is returned unchanged. The lock file is transient
    state under [_oi/] — never committed; deleting it forces a re-resolution on
    the next [oi build].

    Errors when a pin's URL cannot be resolved (network down, ref gone). *)
