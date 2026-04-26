(** [oi registry index]: see implementation for the man page. *)

val do_registry_export :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  sys:D10.Sysops.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  registry:string ->
  output:string ->
  unit
(** Bulk export the registry index + sources tarball to [output]. Shared
    with {!Registry_export} since both commands fall through to the same
    operation. *)

val cmd : unit Cmdliner.Cmd.t
