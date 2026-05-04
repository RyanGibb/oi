(** Cmdliner term builders for shared CLI flags.

    Every command picks up at least [--data-dir], [--cache-dir], and the log
    level. Most pick up [--refresh], [--with-repo], [--with], [-j]; the solving
    commands additionally take [--toolchain] and [--registry]. Repo-specific
    terms ([--reporepo], [--depend], …) live with the [oi repo] command since
    they don't appear elsewhere. *)

val log : unit Cmdliner.Term.t
(** Wires [Fmt_cli], [Logs_cli], and the progress-aware logs reporter. *)

val data_dir : string Cmdliner.Term.t
(** [--data-dir DIR]. Honours [$OI_DATA_DIR] then [$XDG_DATA_HOME] then
    [~/.local/share/oi]. *)

val cache_dir : string Cmdliner.Term.t
(** [--cache-dir DIR]. Honours [$OI_CACHE_DIR] / [$XDG_CACHE_HOME]. *)

(** {1 Shared "global" options} *)

type common = { cache_dir : string; data_dir : string }
(** Resolved values of the option set every harness-using command shares:
    [--cache-dir], [--data-dir], plus the log-setup side effects ([--verbosity],
    [--color], [--verbose-http]). Apply via {!common}. *)

val common : common Cmdliner.Term.t
(** Single Cmdliner term commands plug in once instead of stacking
    [Terms.log $ Terms.cache_dir $ Terms.data_dir]. New genuinely-global options
    should be added here so every command picks them up uniformly. *)

type format =
  | Text
  | Json  (** Output formats supported by commands that emit structured data. *)

val format : format Cmdliner.Term.t
(** [--format=text|json] (default [text]). Adopt this on any command whose
    output an agent might want to parse mechanically. *)

val refresh : bool Cmdliner.Term.t
(** [--refresh] flag: force re-fetch even within the 24h freshness window. *)

val skip_local : bool Cmdliner.Term.t
(** [--skip-local] flag: do not probe the cwd for project files ($(b,*.opam),
    pin-depends, x-repos, $(b,packages/) overlay, dev tools). *)

val with_repos : string list Cmdliner.Term.t
(** [--with-repo URL] (repeatable). *)

val with_deps : string list Cmdliner.Term.t
(** [--with PKG] / [--with URL] (repeatable). *)

val jobs : int option Cmdliner.Term.t
(** [-j N] / [--jobs N]. *)

val toolchain : string option Cmdliner.Term.t
(** [--toolchain HANDLE]. *)

val registry : string Cmdliner.Term.t
(** [--registry URL]. Default: {!default_registry}. *)

(** {1 Env-reading helpers tied to the terms} *)

val getenv_or : default:string -> string -> string
(** Returns the env-var value if set and non-empty, else [default]. *)

val reporepo_path : unit -> string
(** Active reporepo path: [$OI_REPOREPO] or {!Oi.Source.Reporepo.default_path}.
*)

val reporepo_url : unit -> string
(** Active reporepo URL: [$OI_REPOREPO_URL] or
    {!Oi.Source.Reporepo.default_url}. *)

val default_registry : string
(** [https://oi.ci.dev]. *)

val use_registry : Oi.Use_registry.t Cmdliner.Term.t
(** [--use-registry] term, accepting [all], [archives], or [never]. Defaults to
    {!Oi.Use_registry.All}. *)

type remotes = {
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
}

val remotes_of : url:string -> mode:Oi.Use_registry.t -> remotes
(** Split [(url, mode)] into the two remotes the pipeline consumes separately.
    Errors if [url] is empty unless [mode = Never]. *)
