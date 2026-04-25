[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Dependency solving.

    Uses opam-0install-solver with {!Opam_ctx}'s synthetic switch state to
    resolve platform variables. Compiler and base packages provided by the
    toolchain are constrained to the correct versions and excluded from the
    result.

    When multiple package directories are given (from multiple repos via
    {!Repo}), the solver uses the first (default) repo for candidate
    enumeration, and {!load_opam} searches all repos in priority order. *)

val solve :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  Opam_ctx.t ->
  packages_dirs:string list ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  OpamPackage.Name.t list ->
  (OpamPackage.t list, string) result
(** [solve ~fs ~cache_root ctx ~packages_dirs ~constraints names] resolves the
    dependency closure for [names]. Returns packages in topological order.

    Successful solves are persisted to [<cache_root>/solve-cache/] and re-used
    when an identical input is presented again (see {!Solve_cache}). Failed
    solves are not cached. *)

val solve_dir :
  env:(string -> OpamVariable.variable_contents option) ->
  packages_dirs:string list ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  OpamPackage.Name.t list ->
  (OpamPackage.t list, string) result
(** Lower-level solver entrypoint. Runs [opam-0install] over [packages_dirs]
    with exactly the [constraints] and [env] supplied — no auto-pinning of
    OCaml-family packages, no [Opam_ctx], no solve cache. Suitable for one-off
    solves where the caller fully owns the constraint set, e.g. resolving the
    package closure for a fixed-prefix toolchain whose pinned compiler version
    would collide with {!solve}'s default [conf.ocaml_version] injection. *)

val dep_names :
  packages_dirs:string list ->
  conf:Opam_ctx.conf ->
  OpamPackage.t ->
  OpamPackage.Name.Set.t ->
  OpamPackage.Name.Set.t
(** [dep_names ~packages_dirs ~conf pkg in_solution] returns the set of
    dependency names of [pkg] that appear in [in_solution], after filtering by
    the platform variables in [conf]. *)

val load_opam : string list -> OpamPackage.t -> OpamFile.OPAM.t option
(** [load_opam packages_dirs pkg] searches [packages_dirs] in order for the opam
    file of [pkg]. *)

val filter_env : Opam_ctx.conf -> OpamFilter.env
(** [filter_env conf] builds an opam filter environment from a synthetic
    platform configuration. The returned function answers [arch], [os],
    [os-distribution], [os-version], [os-family] from [conf], yields the current
    opam version for [opam-version], and returns empty strings for [sys-ocaml-*]
    (no host compiler is assumed at plan time). Suitable for evaluating any
    filter that reads only platform variables, such as those in [depexts:]. *)

val topo_sort :
  packages_dirs:string list ->
  conf:Opam_ctx.conf ->
  OpamPackage.t list ->
  OpamPackage.t list
(** [topo_sort ~packages_dirs ~conf pkgs] returns [pkgs] in topological order
    (dependencies before dependents). Safe to call on the union of several
    solver results — the relative order already produced by the solver is not
    required. *)
