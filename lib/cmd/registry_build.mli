(** [oi registry build]: bulk-build packages and overlays into the registry. *)

val compute_overlay_depexts_per_distro :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  refresh:bool ->
  platform:Osrel.t ->
  distros:Registry_docker.Distro.t list ->
  (Registry_docker.Distro.t * string list) list
(** Solve every overlay's root packages on each [distros] entry and return
    the per-distro union of declared depexts. Shared with
    [oi registry docker] which needs the same data to parametrise the
    generated Dockerfiles. *)

val cmd : unit Cmdliner.Cmd.t
