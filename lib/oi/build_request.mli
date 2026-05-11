(** Parsing of build / show / run target arguments.

    Knows about the [@handle/pkg] shortcut (an overlay-scoped package spec),
    URL-vs-handle classification of [--with-repo] tokens, and the resolution of
    overlay handles into the {!Project.extra_repo} list the solver consumes.

    This module is the foundation of the unified [Build_pipeline] — every
    command that solves ([oi show], [oi build], [oi run]) goes through the same
    target-classification surface. The cmdliner layer ([lib/cmd]) wraps these
    helpers but adds no logic. *)

val log_overlay : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Format-style debug logger for overlay / reporepo plumbing. *)

(** {1 Build target classification} *)

(** Kinds of "[@handle]-prefixed target" the solving commands accept. [oi run]
    only accepts the first two; [oi build] additionally understands [@handle]
    alone as "every package in the overlay". *)
type build_target =
  | Plain_target of string
  | Overlay_pkg of string * string  (** ([handle], [pkg_spec]) *)
  | Overlay_all of string  (** [handle] alone, expands to every overlay pkg *)

val parse_build_target : string -> build_target

val bare_handle : string -> string option
(** [Some h] when [s] is a bare [@h] handle (no [/pkg] suffix and a well-formed
    handle). [None] otherwise. *)

(** {1 Handle pins}

    A [@handle/pkg[constr]] shortcut once the handle has been routed into
    [with_repos] and the package spec is ready for the solver. Carries the
    handle alongside the package name and any user-supplied constraint so we can
    later pin the package to whatever version the named overlay ships. *)

type handle_pin = {
  handle : string;
  pkg : OpamPackage.Name.t;
  user_constr : OpamFormula.version_constraint option;
}

val split_handle_prefix : string -> (string * string) option
(** [(handle, stripped_spec)] for tokens of the form [@handle/pkg…], or [None]
    otherwise. Raises {!Error.config_error} when [@handle] is given without a
    package. *)

val extract_handle_pins :
  with_repos:string list ->
  string list ->
  string list * string list * handle_pin list
(** Strip [@handle/pkg] prefixes out of a list of tokens. Each hit routes
    [handle] into [with_repos], replaces the token with its stripped form, and
    records a {!handle_pin} so the caller can pin the package to the overlay's
    version. Returns [(stripped_tokens, updated_with_repos, handle_pins)]. *)

val pin_handles : handle_pin list -> string list
(** Project a list of handle-pins down to their handle strings. *)

val handle_pin_constraints :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  data_dir:string ->
  refresh:bool ->
  cli_extras:Project.extra_repo list ->
  handle_pin list ->
  OpamFormula.version_constraint OpamPackage.Name.Map.t
(** Turn a list of handle-pins into an [= VERSION] constraint map suitable for
    merging into the solver's [constraints] argument. A pin without an explicit
    user constraint is pinned to the highest version the overlay ships.
    [cli_extras] must be fully materialised on disk before this runs. *)

(** {1 Extras (--with-repo + project [x-repos:])} *)

val is_url_like : string -> bool
(** A [--with-repo] / [--with] token is a URL when it has a scheme-like prefix
    or contains a path separator; otherwise it's an overlay handle. *)

val handles_of_tokens : string list -> string list
(** Keep only the handle-shaped tokens (those for which {!is_url_like} returns
    [false]). *)

val cli_extra_repo_of_url : string -> Project.extra_repo
(** Convert a CLI [--with-repo URL] entry into a {!Project.extra_repo} with a
    deterministic hashed name. *)

val cli_extra_repos :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  ?reporepo_path:string ->
  ?reporepo_url:string ->
  ?toolchain:Toolchain.info ->
  string list ->
  Project.extra_repo list
(** Resolve a list of [--with-repo] tokens (a mix of URLs and reporepo handles)
    into the flat extra-repo list the solver wants. Later handles in the input
    list are given highest priority.

    When [?toolchain] is set, handle resolution is direct (each handle's latest
    reporepo entry only). [?reporepo_path] / [?reporepo_url] default to
    {!Source.Reporepo.env_path} / {!Source.Reporepo.env_url}. *)

val merge_extras :
  cli:Project.extra_repo list ->
  project:Project.extra_repo list ->
  Project.extra_repo list
(** Merge CLI [--with-repo] URLs and project-declared extras. Project extras win
    on a name collision. *)

(** {1 Misc} *)

val parse_pkg_target :
  string ->
  OpamPackage.Name.t * (OpamFormula.relop * OpamPackage.Version.t) option
(** Parse a CLI target as either ["name"], ["name.version"], or an opam atom
    like ["name>=1.0"] / ["name=1.0"]. Returns the bare name and an optional
    version constraint for the solver. *)

val latest_version_in_dirs : pkg:string -> string list -> string option
(** Highest version of [pkg] found across [dirs] (each expected to be a
    [packages/] tree). [None] when the package is absent from all of them. *)
