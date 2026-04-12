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

val create : prefix:string -> packages_dirs:string list -> conf:conf -> t

val conf : t -> conf
(** Platform configuration used to create this context. *)

val prefix : t -> string
(** Build prefix path. *)

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
  OpamFile.OPAM.t ->
  string list list
(** [resolve_commands t ~test ~doc ~dev_setup opam] returns the fully resolved
    build commands with all opam variables expanded and filters evaluated. *)

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

val switch_env : prefix:string -> (string * string) list
(** [switch_env ~prefix] returns the OCaml switch environment variables for a
    given prefix (OCAMLLIB, CAML_LD_LIBRARY_PATH, OCAMLFIND_CONF, OCAMLPATH,
    OCAMLTOP_INCLUDE_PATH, OPAM_SWITCH_PREFIX). Does not require a context —
    pure function of the prefix path. *)

val init_opam : root:string -> unit
(** [init_opam ~root] initialises opam's global config with an isolated root
    directory (not the user's [~/.opam]). Silent, no depexts. *)
