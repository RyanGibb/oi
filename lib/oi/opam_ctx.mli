[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Synthetic opam switch state for building packages.

    Creates an OpamSwitchState.t backed by a build prefix and package
    repositories, without using ~/.opam. Owns the platform configuration type
    ([conf]) and provides variable resolution for solvers and build commands. *)

(** {1 Platform configuration} *)

type conf = {
  arch : string;
  os : string;
  os_distribution : string;
  os_version : string;
  os_family : string;
  ocaml_version : string;
  jobs : int;
}

val pp_conf : conf Fmt.t

(** {1 Context} *)

type t

type toolchain = {
  install_prefix : string;
  hash : string;
      (** Content hash of the toolchain's solved opam files + platform conf,
          used by {!Solve_cache} to key consumer solves on the toolchain
          identity without enumerating its full package set. *)
  relocatable : bool;
      (** [true] when the compiler can be installed into the consumer prefix
          rather than at [install_prefix]. {!create} skips the [mark_installed]
          pre-population in that case so the consumer solve actually builds
          the compiler, and {!switch_env} skips the toolchain PATH/lib
          layering — the binary cache then matches what no-toolchain mode
          would produce. The version pinning via [packages]/[root_names]
          still applies. *)
  packages : OpamPackage.Set.t;
  root_names : OpamPackage.Name.Set.t;
      (** Solver-root subset of [packages]. The consumer solve uses this to
          force the originally-specified toolchain roots into the solution
          (via [conflict-class] those then exclude the wrong compiler) without
          dragging in every transitive dep — adding the full [packages] set as
          roots pulls in oxcaml meta packages that conflict with each other. *)
}
(** Subset of {!Toolchain.info} that [Opam_ctx] actually needs. Keeps [Opam_ctx]
    independent of the toolchain resolution pipeline while still letting
    [create] layer env vars and mark toolchain packages as pre-installed. *)

val create :
  prefix:string ->
  packages_dirs:string list ->
  conf:conf ->
  ?toolchain:toolchain ->
  unit ->
  t
(** [?toolchain] pins a fixed-prefix OCaml toolchain (compiler, ocamlfind,
    ocamlbuild, ...) to this context. When set:
    - the switch env prepends toolchain [bin], [lib], and [lib/stublibs] to
      [PATH], [OCAMLPATH], and [CAML_LD_LIBRARY_PATH] so consumer builds
      resolve the compiler from the toolchain prefix;
    - toolchain packages are recorded so downstream code (solver constraints,
      execute-skip) can special-case them. *)

val conf : t -> conf
(** Platform configuration used to create this context. *)

val prefix : t -> string
(** Build prefix path. *)

val toolchain : t -> toolchain option
(** Toolchain attached at {!create} time, if any. *)

val resolve :
  t ->
  OpamFile.OPAM.t ->
  ?local:OpamVariable.variable_contents option OpamVariable.Map.t ->
  OpamFilter.env
(** Variable resolver for a package. *)

val resolve_commands :
  t ->
  test:bool ->
  doc:bool ->
  dev_setup:bool ->
  ?build_dir:string ->
  OpamFile.OPAM.t ->
  string list list
(** [resolve_commands t ~test ~doc ~dev_setup ?build_dir opam] returns the
    fully resolved build commands with all opam variables expanded and filters
    evaluated. When [?build_dir] is given, it overrides [%{build}%] to that
    path — needed for packages whose build commands reference [%{build}%]
    expecting it to equal the command's cwd. *)

val compilation_env : t -> OpamFile.OPAM.t -> string array
(** [compilation_env t opam] returns the full build environment for a package,
    including sanitized MAKEFLAGS, package-specific vars, and the switch
    environment. *)

val resolve_substs : t -> OpamFile.OPAM.t -> (string * string) list
(** [resolve_substs t opam] returns a sorted association list mapping opam
    variable names to their resolved values. This captures the variables needed
    for expanding [.in] files at execution time without opam libraries. *)

val mark_installed :
  t -> OpamPackage.t -> OpamFile.OPAM.t -> OpamFile.Dot_config.t option -> unit
(** Add a package to the installed set. *)

val synthetic_config :
  t -> OpamPackage.t -> OpamFile.OPAM.t -> OpamFile.Dot_config.t option
(** [synthetic_config t pkg opam] returns a hard-coded .config for well-known
    compiler packages (currently [ocaml]) so that variables like [ocaml:native]
    can be resolved at plan time, before the package has actually been built.
    Returns [None] for packages without a known synthetic config. *)

val switch_state : t -> OpamStateTypes.unlocked OpamStateTypes.switch_state
(** [switch_state t] returns the underlying switch state for use with
    opam-0install's {!Switch_context}. *)

val platform_env : t -> OpamFilter.env
(** [platform_env t] returns a variable resolver for platform/global variables
    only (no per-package scope). Suitable for filtering dependency formulas
    during solving and topo-sorting. *)

val switch_env :
  ?toolchain:toolchain -> prefix:string -> unit -> (string * string) list
(** [switch_env ?toolchain ~prefix ()] returns the OCaml switch environment
    variables for [prefix] (OCAMLLIB, CAML_LD_LIBRARY_PATH, OCAMLFIND_CONF,
    OCAMLPATH, OCAMLTOP_INCLUDE_PATH, OPAM_SWITCH_PREFIX). When [?toolchain] is
    set the toolchain's [bin]/[lib]/[lib/stublibs] are prepended to PATH,
    OCAMLPATH, CAML_LD_LIBRARY_PATH and OCAMLLIB / OCAMLFIND_CONF are dropped
    so the non-relocatable compiler picks up stdlib via its baked-in path. *)

val init_opam : root:string -> unit
(** [init_opam ~root] initialises opam's global config with an isolated root
    directory (not the user's [~/.opam]). Silent, no depexts. *)
