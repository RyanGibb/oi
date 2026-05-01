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

val remote_of_registry : string -> D10.Layer.remote option
(** Convert a registry URL string to a {!D10.Layer.remote}. Empty string returns
    [None] (registry disabled). *)
