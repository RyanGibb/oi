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
(** Compute the union of depexts under [conf]'s opam filter variables for every
    reporepo handle (other than [default]). For each handle:

    - if it declares [x-root-packages], solve each group and union the depexts
      of the closure;
    - else if it declares [x-oi-toolchain-roots] (toolchain definition), solve
      those roots under the entry's own [x-oi-toolchain-name] and union the
      closure's depexts — picks up compiler-stack depexts (libgmp-dev for
      [zarith.+ox], etc.);
    - else if it has its own clone (non-toolchain overlay with a [url{}]), walk
      every [opam] in [v2/<handle>/packages/] and union their depexts directly —
      mirrors [oi build --all]'s "fan out to every package" fallback for
      overlays without explicit roots.

    [?handle] restricts to a single handle. [?override] forces a
    [--toolchain=NAME] across the board. *)

val compute_overlay_depexts_per_distro :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  refresh:bool ->
  platform:Osrel.t ->
  distros:Registry_docker.Distro.t list ->
  (Registry_docker.Distro.t * string list) list
(** Same expansion as {!compute_overlay_depexts_for_conf} but evaluated on each
    [distros] entry's filter context (os, os-distribution, os-family,
    os-version). Solves and overlay-tree walks happen once under the host conf
    and are reused across distros — only the per-distro depext filter is
    re-evaluated. Shared with [oi docker --all] which uses the result to
    parametrise the generated Dockerfiles. *)

val cmd : unit Cmdliner.Cmd.t
(** $(b,oi build). *)

val test_cmd : unit Cmdliner.Cmd.t
(** $(b,oi test). Defined here so the test path shares [find_target_layer],
    [run_target_test], and the rest of the build machinery without a
    one-function module. *)
