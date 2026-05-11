(** [oi docker TARGET]: emit a Dockerfile (plus sidecar [recipe.json]) that
    reproduces a single solve target inside a container.

    The Dockerfile is d10ir-aware: it walks the solved [D10ir.Plan.t], lists
    every unique source-archive sha256, and bakes them into one heredoc'd RUN
    that curls them in parallel under a BuildKit cache mount. The recipe is
    written out next to the Dockerfile so [oi ir run /work] inside the container
    has the plan to replay. *)

module Distro = Dockerfile_opam.Distro

val emit_no_recipe :
  distro:Distro.t ->
  oi_version:string ->
  registry:string ->
  output:string option ->
  targets:string list ->
  unit
(** Source-independent counterpart to {!emit}: emit a Dockerfile that doesn't
    bake a recipe.json. At [docker build] time, [oi] itself solves [targets]
    against the configured registry / reporepo, fetches archives, and builds.
    The image is reproducible only insofar as [OI_VERSION], the reporepo URL,
    and the registry index are stable; every other input is resolved at build
    time. Useful when you want a one-file Dockerfile you can hand off without a
    recipe sidecar, accepting that "the build runs at docker-build time" rather
    than "the build replays a pre-computed plan". *)

val emit :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  sys:D10.Sysops.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  session:D10.Sysops.Http.session ->
  platform:Osrel.t ->
  refresh:bool ->
  registry:string ->
  distro:Distro.t ->
  oi_version:string ->
  output:string option ->
  targets:string list ->
  unit
(** [emit ~targets ~distro ~output …] solves [targets] under [distro]'s os_key
    via [Build_pipeline.solve], collects unique archive shas from the merged
    [D10ir.Plan.t], and writes:

    - [<output>/Dockerfile.oi-<slug>.<distro>] (or that name in the cwd when
      [output] is [None])
    - [<output>/recipe.json] — the d10ir plan the Dockerfile's [oi ir run] step
      consumes.

    [oi_version] is passed through as the default for the [ARG OI_VERSION] in
    the emitted Dockerfile ([latest] resolves at docker-build time via the
    GitHub releases API). [registry] becomes the default [ARG OI_REGISTRY] so
    the archive URLs point at the user's configured mirror. *)
