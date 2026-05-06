(** Publish helper: writes the local layer cache, index, and source mirror
    into a tree an [oi] client expects from a remote registry. Driven by
    [oi build --export DIR]; no standalone command. *)

val run :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  sys:D10.Sysops.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  registry:string ->
  output:string ->
  unit
(** [run ~os_key ~cache ~registry ~output] writes the publishable registry
    tree under [output]. Idempotent on repeat invocations. *)
