(** [oi build] and [oi test]: build (or run tests for) a project, package,
    overlay, or the whole reporepo. *)

val compute_overlay_depexts_for_conf :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  refresh:bool ->
  conf:Oi.Solver.Ctx.conf ->
  ?override:string ->
  ?handle:string ->
  unit ->
  string list
(** Solve overlays' [x-root-packages] and return the union of depexts under
    [conf]'s opam filter variables. [?handle] restricts to a single overlay
    (default: every overlay). [?override] forces a [--toolchain=NAME] across
    the board. *)

val compute_overlay_depexts_per_distro :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  refresh:bool ->
  platform:Osrel.t ->
  distros:Registry_docker.Distro.t list ->
  (Registry_docker.Distro.t * string list) list
(** Solve every overlay's root packages on each [distros] entry and return the
    per-distro union of declared depexts. Shared with [oi docker --all] which
    needs the same data to parametrise the generated Dockerfiles. *)

val cmd : unit Cmdliner.Cmd.t
(** $(b,oi build). *)

val test_cmd : unit Cmdliner.Cmd.t
(** $(b,oi test). Defined here so the test path shares
    [find_target_layer], [run_target_test], and the rest of the build
    machinery without a one-function module. *)
