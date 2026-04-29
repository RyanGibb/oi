[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Build plan: dependency graph of package actions, and the fully-resolved
    execution plan derived from it.

    Two-stage:
    - {!build} returns a {!graph} (the action DAG with [method_] / layer hashes
      decided per package). This is the structural plan suitable for dry-run
      output, parallel-group inspection, and layer-cache lookups.
    - {!resolve} expands a {!graph} into a {!t} with all opam variables,
      filters, and commands turned into concrete shell commands grouped by stage
      — what {!Execute.run} consumes.

    Build directories are deterministic under the oi cache root at
    [{cache_root}/build/_build/{pkg}-{short_hash}/], with the shared prefix at
    [{cache_root}/build/prefix/]. Each package lists the dependency layers that
    must be hardlinked into the prefix before building. *)

(** {1 Action graph}

    Captures the full dependency DAG with per-package build phases, preserving
    the parallel structure from the solver. When [~d10] is provided to {!build},
    packages whose layers already exist in the cache are marked as {!Binary} and
    restored directly instead of being built from source. *)

(** How a package will be installed. *)
type method_ =
  | Source  (** Build from source (or fetch from remote registry). *)
  | Binary  (** Restore from the cached layer at [node.layer_hash]. *)

type node = {
  pkg : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  method_ : method_;
  deps : OpamPackage.Name.t list;
      (** Direct dependencies within the plan (determines execution order). *)
  layer_hash : string;
      (** Content hash of the package plus its transitive in-plan deps. *)
}
(** A single package action in the plan. *)

type graph
(** A DAG of nodes keyed by package name, preserving parallel structure. *)

val build :
  Solver.Ctx.t ->
  ?d10:D10.Config.t ->
  packages_dirs:string list ->
  OpamPackage.t list ->
  graph
(** [build ctx ?d10 ~packages_dirs pkgs] builds an action graph from a
    topologically-sorted package list. When [d10] is given, checks the layer
    cache and marks cached packages as [Binary]. *)

val nodes : graph -> node list
(** [nodes g] is the list of all nodes in topological order. *)

val roots : graph -> node list
(** [roots g] is the list of nodes with no in-plan dependencies (can start
    immediately). *)

val dependents : graph -> OpamPackage.Name.t -> node list
(** [dependents g name] is the list of nodes that directly depend on [name]. *)

val total : graph -> int
(** [total g] is the number of nodes in the plan. *)

val find : graph -> OpamPackage.Name.t -> node
val dep_pkgs_of : graph -> node -> OpamPackage.t list
val nodes_by_name : graph -> node OpamPackage.Name.Map.t
val topo_order : graph -> OpamPackage.Name.t list
val parallel_groups : graph -> OpamPackage.Name.t list list

val layer_hash_for : graph -> OpamPackage.Name.t -> string option
(** [layer_hash_for g name] returns the layer hash for a specific package, or
    [None] if the package is not in the plan. *)

val layer_hashes : graph -> string list
(** [layer_hashes g] returns the layer hash for each package in the plan, in
    topological order. Used for prefix assembly. *)

val pp_tree : ?remote_has:(string -> bool) -> graph Fmt.t
(** [pp_tree ?remote_has] renders the action graph as a dependency tree.
    {!Source} packages whose layer hash satisfies [remote_has] are shown as
    "remote" (cyan) instead of "source" (blue). *)

(** {1 Executable plan}

    Fully resolved build/install commands, grouped into parallel stages —
    packages within a stage have no inter-dependencies and can build
    concurrently in separate prefixes. *)

type source_info = { url : string; checksums : string list }
type patch = { file : string; filter : string option }
type subst = string

type dep_layer = {
  pkg : string;  (** dependency package name.version *)
  hash : string;  (** d10 layer hash *)
}

type package_plan = {
  pkg : string;
  layer_hash : string;
  method_ : method_;
  dep_layers : dep_layer list;
  source : source_info option;
  extra_sources : (string * source_info) list;
  extra_files : (string * string) list;
      (** Opam [extra-files:] entries: pairs of [(basename, src_path)] where
          [src_path] is the absolute path to the file in
          [<packages_dir>/<name>/<name.version>/files/<basename>]. Copied into
          [build_dir] before patches are applied, so inline patches the recipe
          references actually exist when [patch -i] runs. *)
  patches : patch list;
  substs : subst list;
  subst_vars : (string * string) list;
  build_commands : string list list;
  install_commands : string list list;
  install_file : string;
  env : string array;
  build_dir : string;
  prefix : string;
  overlay_handle : string option;
      (** Reporepo overlay handle that contributed this package's opam file
          (e.g. ["default"], ["avsm"]), or [None] when the package came from a
          non-overlay source like a pin-depends tree. *)
  overlay_version : string option;
      (** Reporepo overlay version (e.g. ["20260418.6"]). Present iff
          [overlay_handle] is. *)
  depexts : string list;
      (** Declared system packages active under the build's filter env. Names
          are taken straight from [OpamFile.OPAM.depexts] and only retained for
          the entries whose filter evaluates true. Empty if the package declares
          none. *)
}

type group = { stage : int; packages : package_plan list }

type t = {
  groups : group list;
  cache_root : string;
  os_key : string;
  ocaml_version : string;
  build_prefix : string;
      (** Prefix into which every package in this plan is built and installed.
          Usually [{cache_root}/build/prefix] for consumer solves; set to a
          toolchain dir when building a fixed-prefix toolchain. *)
  total_packages : int;
}

val resolve :
  Solver.Ctx.t ->
  packages_dirs:string list ->
  cache_root:string ->
  os_key:string ->
  ocaml_version:string ->
  ?build_prefix:string ->
  graph ->
  t
(** [resolve ctx ~packages_dirs ~cache_root ~os_key ~ocaml_version ?build_prefix
     g] turns an action graph into a fully-resolved execution plan.

    [packages_dirs] is the same list passed to the solver, used to attribute
    each package to its source directory so the resulting plan (and any layer
    built from it) can be tagged with the overlay that contributed the opam
    file. [?build_prefix] defaults to [{cache_root}/build/prefix]; pass a
    different path to build into a fixed location (e.g. a toolchain prefix). *)

val pp : t Fmt.t
