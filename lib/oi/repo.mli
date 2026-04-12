(** Opam repository management.

    Manages opam repository remotes using opam's {!OpamRepository} backend which
    handles git, HTTP ([index.tar.gz]), and other transports. The relocatable
    overlay repo takes priority over the default opam-repository.

    Repository packages are accessed as a list of directories (one per repo)
    rather than merged into a single directory — the solver uses
    {!Opam_ctx.switch_state} with {!Opam_0install.Switch_context} to resolve
    packages across all repos. *)

type remote = { name : string; url : string }
type config = { remotes : remote list; default : string }

val config : config
(** Default repo configuration: relocatable overlay (priority) + default. *)

val ensure : data_dir:string -> unit
(** [ensure ~data_dir] clones missing repos and updates stale ones. *)

val repo_dir : data_dir:string -> string -> string
(** [repo_dir ~data_dir name] is the local clone path for repo [name]. *)

val packages_dirs : data_dir:string -> string list
(** [packages_dirs ~data_dir] returns the [packages/] directories for all
    configured repos, in priority order (relocatable first). *)

val ensure_extra : data_dir:string -> string list -> string list
(** [ensure_extra ~data_dir urls] clones/updates extra repos and returns their
    [packages/] directories. *)

val pp_config : config Fmt.t
