[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** End-to-end pipeline: solve → plan → build → assemble.

    The CLI commands compose these operations: most do {!pick_toolchain} →
    {!build} → {!assemble_prefix}, then exec into the result. The smaller
    helpers ({!cache_urls}, {!fetch_remote_layers}) are exported so callers can
    intercept the pipeline at intermediate points (e.g. registry-build wraps
    {!build} with its own progress reporter). *)

(** {1 Platform configuration and d10 wiring} *)

val make_conf : platform:Osrel.t -> ocaml_version:string -> Solver.Ctx.conf
(** Build a platform conf from a detected {!Osrel.t} plus an explicit OCaml
    version. *)

val make_d10 :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  cache:Cache.t ->
  os_key:string ->
  D10.Config.t

val init_opam_root : fs:Eio.Fs.dir_ty Eio.Path.t -> data_dir:string -> unit
(** Create [<data_dir>/opam-root/] (if absent) and call {!Solver.Ctx.init_opam}
    so the rest of the pipeline can construct {!Solver.Ctx.t}s. Idempotent. *)

(** {1 Toolchain resolution} *)

val solver_inputs :
  Toolchain.info option ->
  Solver.Ctx.conf ->
  Solver.Ctx.conf * Solver.Ctx.toolchain option
(** [solver_inputs info conf] derives the two views the rest of the pipeline
    consumes: a [conf] with [ocaml_version] aligned to the toolchain's compiler,
    and the {!Solver.Ctx.toolchain} subset {!Solver.Ctx.create} /
    {!Solver.Env.make_env} take. *)

val pick_toolchain :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  data_dir:string ->
  conf:Solver.Ctx.conf ->
  install:bool ->
  override:string option ->
  handles:string list ->
  ?reporter:Build_progress.reporter ->
  unit ->
  Toolchain.info option
(** [pick_toolchain ~override ~handles ()] is the single source of truth for
    picking which toolchain a command runs against. Selection precedence (first
    match wins):

    + [override = Some h]: resolve [h] directly. The [--toolchain=NAME] flag.
    + Implicit pickup from [handles]: scan each handle's latest reporepo entry
      for an [x-oi-toolchain] field, and check whether the handle itself names a
      toolchain definition ([x-oi-toolchain-name]). A unique non-empty result
      wins; multiple distinct names hard-error with a "pass --toolchain=NAME to
      disambiguate" hint.
    + Reporepo default: the entry flagged [x-oi-default-toolchain: true].
    + Nothing matched: hard-error. The reporepo is expected to declare a
      default; [oi repo lint] enforces this.

    [install:true] runs {!Toolchain.ensure_installed} on the chosen toolchain;
    [install:false] returns the [info] without preparing its on-disk prefix.
    Compose with {!solver_inputs} when the caller also needs the [conf] /
    [Ctx.toolchain] views. The lower-level [Toolchain.resolve] handles a single
    named handle without precedence rules. *)

val strip_compiler_roots_for_override :
  override:string option ->
  toolchain:Toolchain.info option ->
  OpamPackage.Name.t list ->
  OpamPackage.Name.t list
(** [strip_compiler_roots_for_override ~override ~toolchain names] removes
    compiler-family root names (i.e. those in [tc.root_names]) from [names] when
    [override] is set. Used to substitute the explicit [--toolchain=NAME]'s
    compiler pins for whatever the call-site's overlays / project would
    otherwise have asked for, avoiding [conflict-class] failures on
    [ocaml-core-compiler]. No-op when [override] is [None] (the toolchain came
    from implicit / default pickup, so its roots already line up with the
    handles in scope). *)

(** {1 Sources} *)

val classify_with_args :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Cache.t ->
  ?refresh:bool ->
  ?reporter:Build_progress.reporter ->
  string list ->
  Project.Script.dep list * Project.Url.t
(** [classify_with_args tokens] partitions every [--with=…] token in one pass:
    URLs get cloned into the pin cache and produce pins + solver roots; opam
    package specs come back as already-parsed {!Project.Script.dep}. *)

val filter_compatible_overlays :
  reporepo_path:string ->
  toolchain:Toolchain.info option ->
  string list ->
  string list
(** Drop project-declared reporepo overlays tagged with an [x-oi-toolchain] that
    doesn't match the active toolchain handle. Overlays without [x-oi-toolchain]
    and overlays not present in the reporepo (URL-only handles) are kept. *)

(** {1 Build pipeline} *)

val cache_urls :
  cache:Cache.t -> source_remote:D10.Layer.remote option -> OpamUrl.t list
(** [cache_urls] for opam's [pull_tree]/[pull_file] to probe before falling back
    to upstream: always includes the local {!Source.Mirror}; with a remote
    [source_remote], also the registry's [sources/] subtree. *)

val fetch_remote_layers :
  ?reporter:Build_progress.reporter ->
  ?jobs:int ->
  session:D10.Sysops.Http.session ->
  layer_remote:D10.Layer.remote option ->
  d10:D10.Config.t ->
  packages_dirs:string list ->
  ctx:Solver.Ctx.t ->
  pkgs:OpamPackage.t list ->
  Plan.graph ->
  Plan.graph
(** Try fetching uncached [Source] layers from [layer_remote]. Returns a new
    plan graph with downloaded layers promoted to [Binary]. No-op when
    [layer_remote = None] or every layer is already cached.

    [session] is the shared connection pool for the index probe and every layer
    pull — typically created once per CLI invocation by the caller via
    {!D10.Sysops.Http.with_session}, so an [oi build --all] over [N] solve
    groups does one TLS handshake and one [OINDEX.txt] fetch instead of [N].

    [reporter] receives typed {!Build_progress.event}s — [Phase_started
    Fetching], [Fetch_started/_progress/_finished], [Phase_done]. The
    cmdliner layer renders them; defaults to {!Build_progress.null}. *)

val build :
  sys:D10.Sysops.t ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:_ Eio.Time.clock ->
  cache:Cache.t ->
  data_dir:string ->
  conf:Solver.Ctx.conf ->
  os_key:string ->
  session:D10.Sysops.Http.session ->
  ?dry_run:bool ->
  ?extra_repos:Project.extra_repo list ->
  ?pins:Project.pin list ->
  ?refresh:bool ->
  ?layer_remote:D10.Layer.remote ->
  ?source_remote:D10.Layer.remote ->
  ?jobs:int ->
  ?toolchain:Toolchain.info ->
  ?constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  ?project_root:string ->
  ?local_packages_dir:string ->
  ?reporter:Build_progress.reporter ->
  ?emit_recipe:string ->
  ?save_recipe:string ->
  OpamPackage.Name.t list ->
  string list
(** [build] solves for [names], ensures every needed layer exists (building from
    source via the build prefix when not cached), and returns the layer hashes
    in topological order.

    [project_root] is the directory that holds the project's [_oi/] tree; when
    supplied, every pin's URL is sha-pinned via [_oi/oi.lock] before fetch. The
    lock is transient build state, regenerated as needed.

    [reporter] receives every progress event from this build: phase
    transitions ([Solving] → [Fetching] → [Building]), per-archive fetch
    progress, per-layer build events from [D10ir.Direct.run], and a
    final [Build_summary]. The library never opens or drives a UI
    itself; the cmdliner layer constructs whatever rendering it wants
    on top of {!Build_progress.event}s.

    [emit_recipe] when set, after the plan has been elaborated, writes a
    serialised d10ir recipe to that directory and returns without running
    [Execute.run]. The archives that the recipe references are
    materialised as a side-effect of [Recipe_emitter.emit] and live under
    [<cache>/d10ir/archives/]; the recipe references them by relative
    path. Mutually exclusive with the build path — when [emit_recipe] is
    set, no layers are built.

    [save_recipe] is the build-and-also-save companion: when set, the
    recipe is serialised into the supplied directory before
    [D10ir.Direct.run] takes over, with a filename derived from the
    plan's first root package so multiple groups in an [oi build
    --all] / [oi build @overlay] flow each leave a distinct recipe.
    The build itself continues normally. Different from [emit_recipe]
    which is mutually exclusive with the build.

    When [dry_run] is [true] the function prints the build plan and calls
    [Stdlib.exit 0] — same behaviour as [oi show]. *)

val assemble_prefix :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:_ Eio.Time.clock ->
  cache:Cache.t ->
  os_key:string ->
  layer_hashes:string list ->
  string
(** Hardlink-assemble a consumer prefix from [layer_hashes] (in topo order) and
    return its absolute path. *)
