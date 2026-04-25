[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** End-to-end pipeline: solve → plan → build → assemble.

    The CLI commands compose these operations: most do
    {!resolve_toolchain} → {!build} → {!assemble_prefix}, then exec into
    the result. The smaller helpers ({!cache_urls}, {!record_sources},
    {!fetch_remote_layers}) are exported so callers can intercept the
    pipeline at intermediate points (e.g. registry-build wraps {!build}
    with its own progress reporter). *)

(** {1 Platform configuration and d10 wiring} *)

val make_conf :
  platform:Osrel.t -> ocaml_version:string -> Solver.Ctx.conf
(** Build a platform conf from a detected {!Osrel.t} plus an explicit
    OCaml version. *)

val make_d10 :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  cache:Cache.t ->
  os_key:string ->
  D10.Config.t

val init_opam_root :
  fs:Eio.Fs.dir_ty Eio.Path.t -> data_dir:string -> unit
(** Create [<data_dir>/opam-root/] (if absent) and call
    {!Solver.Ctx.init_opam} so the rest of the pipeline can construct
    {!Solver.Ctx.t}s. Idempotent. *)

(** {1 Toolchain resolution} *)

val toolchain_views :
  Toolchain.info option ->
  Solver.Ctx.conf ->
  Solver.Ctx.conf * Solver.Ctx.toolchain option
(** [toolchain_views info conf] derives the two views the rest of the
    pipeline consumes: a [conf] with [ocaml_version] aligned to the
    toolchain's compiler, and the {!Solver.Ctx.toolchain} subset
    {!Solver.Ctx.create} / {!Solver.Env.make_env} take. *)

val resolve_toolchain :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  data_dir:string ->
  conf:Solver.Ctx.conf ->
  install:bool ->
  string option ->
  Toolchain.info option
(** [resolve_toolchain ~install handle] looks up [handle] in the
    reporepo. When [install:true] and the toolchain is non-relocatable,
    builds the toolchain into its fixed prefix on first use. Returns
    [None] for [handle = None]. Compose with {!toolchain_views} when the
    caller also needs the [conf] / [Ctx.toolchain] views. *)

(** {1 Sources} *)

val materialize_with_deps :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Cache.t ->
  ?refresh:bool ->
  string list ->
  Project.Script.dep list * Project.Url.t
(** Classify every [--with=…] token in one pass: URLs get cloned into
    the pin cache and produce pins + solver roots; opam package specs
    come back as already-parsed {!Project.Script.dep}. *)

val filter_compatible_overlays :
  reporepo_path:string ->
  toolchain:Toolchain.info option ->
  string list ->
  string list
(** Drop project-declared reporepo overlays tagged with an
    [x-oi-toolchain] that doesn't match the active toolchain handle.
    Overlays without [x-oi-toolchain] and overlays not present in the
    reporepo (URL-only handles) are kept. *)

(** {1 Build pipeline} *)

val cache_urls :
  cache:Cache.t -> remote:D10.Layer.remote option -> OpamUrl.t list
(** [cache_urls] for opam's [pull_tree]/[pull_file] to probe before
    falling back to upstream: always includes the local
    {!Source.Mirror}; with a remote registry, also the registry's
    [sources/] subtree. *)

val record_sources :
  sys:D10.Sysops.t -> cache:Cache.t -> Plan.t -> unit
(** After a successful {!Execute.run}, promote every source blob in
    opam's download-cache into the local {!Source.Mirror} and record
    metadata rows. Idempotent and best-effort. *)

val fetch_remote_layers :
  ?jobs:int ->
  remote:D10.Layer.remote option ->
  d10:D10.Config.t ->
  packages_dirs:string list ->
  ctx:Solver.Ctx.t ->
  pkgs:OpamPackage.t list ->
  Plan.graph ->
  Plan.graph
(** Try fetching uncached [Source] layers from [remote]. Returns a new
    plan graph with downloaded layers promoted to [Binary]. No-op when
    [remote = None] or every layer is already cached. *)

val build :
  sys:D10.Sysops.t ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:_ Eio.Time.clock ->
  cache:Cache.t ->
  data_dir:string ->
  conf:Solver.Ctx.conf ->
  os_key:string ->
  ?dry_run:bool ->
  ?extra_repos:Project.extra_repo list ->
  ?pins:Project.pin list ->
  ?refresh:bool ->
  ?remote:D10.Layer.remote ->
  ?jobs:int ->
  ?toolchain:Toolchain.info ->
  ?constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  OpamPackage.Name.t list ->
  string list
(** [build] solves for [names], ensures every needed layer exists
    (building from source via the build prefix when not cached), and
    returns the layer hashes in topological order.

    When [dry_run] is [true] the function prints the build plan and
    calls [Stdlib.exit 0] — same behaviour as [oi show]. *)

val assemble_prefix :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:_ Eio.Time.clock ->
  cache:Cache.t ->
  os_key:string ->
  layer_hashes:string list ->
  string
(** Hardlink-assemble a consumer prefix from [layer_hashes] (in topo
    order) and return its absolute path. *)
