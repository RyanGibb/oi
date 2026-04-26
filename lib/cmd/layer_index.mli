(** Local + remote {!D10.Index} plumbing for [oi run] and [oi search].

    Maintains the on-disk binary→package index that backs the binary-name
    fallback in [oi run] (when the user types [oi run utop] and [utop]
    isn't an opam package name) and the [oi search] command. Bridges the
    local d10 index with the remote registry's published [index.db]. *)

val remote_index_max_age : float
(** Seconds. Default [3600.0] (1 hour). The remote registry index is
    re-downloaded on next use after this TTL. *)

val url_join : string -> string -> string
(** Join a registry base URL and a relative path with a single ['/']
    regardless of whether the base has a trailing slash. *)

val ensure_local :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  cache:Oi.Cache.t ->
  os_key:string ->
  string
(** Build/refresh the local index at [{cache_root}/layers/{os_key}/index.db].
    Cheap staleness check rebuilds when the [layers/{os_key}/] directory
    count exceeds the indexed-row count. Returns the index path. *)

val ensure_remote :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache:Oi.Cache.t ->
  os_key:string ->
  registry:string ->
  string option
(** Download the remote registry's [{os_key}/index.db] to a local cache
    location ([<=1h] freshness window, atomic rename). Returns the local
    path on success, [None] when the registry is empty/unreachable. *)

val merge_remote_into_local :
  index_path:string -> remote_path:string -> unit
(** Open [index_path] (the local index) and merge every row from
    [remote_path]. A corrupt remote file is unlinked so the next call
    re-downloads it. *)

val binary_to_package :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:_ Eio.Time.clock ->
  cache:Oi.Cache.t ->
  os_key:string ->
  registry:string ->
  string ->
  (string * string option) option
(** [binary_to_package … name] returns [Some (pkg, overlay_handle)] when
    the merged local+remote index has a row for the binary [name]. The
    overlay handle (if any) is load-bearing: the package may only be
    available in that overlay's [packages/] tree, so callers should add
    the handle to their repo set before solving. *)
