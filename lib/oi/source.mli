[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Source acquisition: opam repositories, the reporepo (overlay-of-overlays
    metadata), pin-depends realisation, and the registry source mirror.

    Every package the solver sees comes from a [packages/] tree on disk. {!Repo}
    clones individual remotes; {!Reporepo} resolves and materialises a pinned
    set of overlays; {!Pin} synthesises a [packages/] tree from [pin-depends:]
    entries; {!Mirror} keeps a sqlite-indexed source-tarball cache for the
    registry. *)

(** {1 Opam repository clones} *)

module Repo : sig
  val repo_dir : data_dir:string -> string -> string
  (** [repo_dir ~data_dir name] is the local clone path for a repo named [name].
  *)

  val ensure_extra :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    data_dir:string ->
    ?refresh:bool ->
    Project.extra_repo list ->
    string list
  (** [ensure_extra ~fs ~data_dir extras] clones/updates each entry using the
      same age/force semantics as {!ensure_one}. Each entry is cloned into
      [data_dir/repos/<name>] — two entries with the same name collide by design
      (callers should deduplicate). Returns one [packages/] directory per entry
      in input order. *)

  val ensure_one :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    refresh:bool ->
    label:string ->
    url:string ->
    dir:string ->
    unit
  (** Low-level clone/update primitive. Clones [url] into [dir] if empty,
      otherwise refreshes when [refresh] is true or the clone is older than
      {!Cache.refresh_max_age}. *)
end

(** {1 Reporepo: overlay-of-overlays metadata}

    A reporepo is an opam-layout directory where each package represents an
    overlay: a handle (the package name) points at a specific commit of an
    upstream opam repository. Dependencies between overlay packages express
    transitive overlay composition, giving a lockfile-like view of the full repo
    set needed by a consumer. oi never solves or builds packages from a
    reporepo; it mines the pinned URLs out of them and threads those as
    additional opam remotes into the normal solver. *)

module Reporepo : sig
  type entry = {
    handle : string;
        (** Opam package name for the overlay (must be opam-valid, so no dots).
        *)
    version : string;  (** Full opam version string, e.g. [20250418.0]. *)
    url : string;
        (** Upstream git URL (without commit fragment). Empty for
            definition-only entries that compose existing overlays via
            [depends:] without their own clone. *)
    commit : string;
        (** 40-char commit sha that [url] is pinned to. Empty when [url] is
            empty. *)
    ref_ : string option;
        (** Upstream git ref this entry tracks (typically a branch like
            [refs/heads/relocatable]). *)
    toolchain : string option;
        (** [x-oi-toolchain]: this overlay targets the named toolchain (use-site
            relationship). *)
    toolchain_name : string option;
        (** [x-oi-toolchain-name]: when set, this entry DEFINES a toolchain with
            the given CLI name. The reporepo handle and the toolchain CLI name
            live in separate namespaces. *)
    toolchain_compiler : string option;
        (** [x-oi-toolchain-compiler]: the primary compiler package spec, e.g.
            ["ocaml-variants.5.2.0+ox"]. Only meaningful when {!toolchain_name}
            is set. *)
    relocatable : bool option;
        (** [x-oi-relocatable]: build mode for the toolchain this entry defines.
        *)
    toolchain_roots : string list list;
        (** [x-oi-toolchain-roots]: solver root specs for the toolchain. *)
    depends : (string * string option) list;
        (** Other overlay handles this one depends on, optionally with an exact
            version. *)
    root_packages : string list list;
        (** Package sets to pre-build when priming this overlay into a registry.
            Each outer entry is a solve group: a list of package specs fed to
            the solver together. *)
    opam_path : string;  (** Absolute path to the source opam file. *)
  }

  val load : path:string -> entry list
  (** [load ~path] walks [path/packages/*/*/opam] and parses every overlay
      package. Entries that don't have [x-oi-overlay: true] are skipped. *)

  val latest : entry list -> handle:string -> entry option
  (** Highest-versioned entry for a given handle. *)

  val find : entry list -> handle:string -> version:string -> entry option

  type root = { handle : string; version : string option }

  val resolve : entry list -> roots:root list -> entry list
  (** Transitive closure in dependency order (deps before dependents). If a root
      has no [version], the highest version is picked. *)

  val materialize :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    data_dir:string ->
    ?refresh:bool ->
    entry list ->
    string list
  (** For each entry, ensure its upstream opam repo is cloned at the pinned
      commit under [{data_dir}/repos/overlay-<handle>-<version>/]. Returns the
      list of [packages/] directories in the order supplied. *)

  val materialize_one :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    refresh:bool ->
    data_dir:string ->
    handle:string ->
    version:string ->
    url:string ->
    commit:string ->
    string

  (** {2 Base overlay resolution} *)

  val ensure_base :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    data_dir:string ->
    ?refresh:bool ->
    unit ->
    string list
  (** Resolves the [relocatable] overlay (and its transitive deps) from the
      reporepo, clones each at its pinned commit, and returns [packages/]
      directories in solver priority order. Auto-clones the reporepo itself if
      it doesn't already exist on disk. *)

  val base_entries : unit -> entry list
  (** Resolved base overlays without cloning. Useful for display in [oi config].
      Empty list when the reporepo is missing or has no [relocatable] entry. *)

  (** {2 Paths and bootstrapping} *)

  val default_path : string
  (** [$OI_DATA_DIR/reporepo], falling back to [$XDG_DATA_HOME/oi/reporepo] and
      then [~/.local/share/oi/reporepo]. *)

  val env_path : unit -> string
  val default_url : string
  val env_url : unit -> string

  val ensure_clone :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    refresh:bool ->
    path:string ->
    url:string ->
    unit

  val set_push_url : sys:D10.Sysops.t -> path:string -> string -> unit

  type push_step =
    | Step_commit of { files : string list }
    | Step_pull of { commits : int }
    | Step_push of { commits : int }

  type push_outcome = push_step list

  val push :
    ?on_step_start:(int -> string -> unit) ->
    sys:D10.Sysops.t ->
    path:string ->
    unit ->
    push_outcome
  (** Stage and commit any uncommitted changes, [git pull --rebase] to bring in
      upstream history, then [git push] if local is ahead. *)

  val add :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    path:string ->
    handle:string ->
    url:string ->
    ?ref_:string ->
    ?toolchain:string ->
    ?base_handles:string list ->
    ?depends:(string * string option) list ->
    ?root_packages:string list list ->
    ?synopsis:string ->
    ?force:bool ->
    unit ->
    entry

  val bump :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    path:string ->
    handle:string ->
    ?url:string ->
    ?ref_:string ->
    ?toolchain:string ->
    ?base_handles:string list ->
    ?depends:(string * string option) list ->
    ?root_packages:string list list ->
    unit ->
    [ `Bumped of entry | `Unchanged of entry ]

  val remove :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    path:string ->
    handle:string ->
    ?version:string ->
    unit ->
    unit

  val ls_remote_sha : sys:D10.Sysops.t -> ?ref_:string -> string -> string
end

(** {1 Pin-depends realisation}

    Fetch each pin's source, extract its [<pkgname>.opam], rewrite [url:] to
    point at the local clone (with a resolved revision baked in), and emit a
    synthetic opam [packages/] directory the solver can consume ahead of all
    other repositories. *)

module Pin : sig
  val materialize :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    cache:Cache.t ->
    ?refresh:bool ->
    Project.pin list ->
    string option
  (** Returns [Some packages_dir] (an absolute path to a synthesized [packages/]
      tree) when [pins] is non-empty, [None] when [pins = []]. *)
end

(** {1 Source tarball mirror}

    Populated automatically during [oi registry build] as a side-effect of each
    successful source fetch; exported to the registry's top-level [sources/]
    directory by [oi registry export]. The on-disk layout is what opam expects
    for a [cache_url]:

    <mirror>/<algo>/<first-2-chars>/<full-hash>

    where <algo> is [md5], [sha256], or [sha512]. A sibling sqlite database
    [<mirror>/index.db] records metadata for fast queries. *)

module Mirror : sig
  val dir : cache:Cache.t -> string
  (** Absolute path to the mirror root: [{cache_root}/mirror]. *)

  val url : cache:Cache.t -> OpamUrl.t
  (** [file://<dir>], suitable for use as a [cache_url] in
      [OpamRepository.pull_*]. *)

  val remote_url : registry:string -> OpamUrl.t
  (** [<registry>/sources] as an [OpamUrl.t]. *)

  val record :
    sys:D10.Sysops.t ->
    cache:Cache.t ->
    package:OpamPackage.t ->
    ?overlay:string * string ->
    kind:[ `Main | `Extra of string ] ->
    url:OpamUrl.t ->
    checksums:OpamHash.t list ->
    unit ->
    unit
  (** Promote a just-fetched source from opam's own download-cache into the
      mirror, and record metadata rows. Idempotent. *)

  type stats = { count : int; total_size : int64 }

  val stats : cache:Cache.t -> stats

  type entry = {
    sha256 : string;
    size : int64;
    package_name : string;
    package_version : string;
    kind : [ `Main | `Extra of string ];
    url : string;
  }

  val list : cache:Cache.t -> ?package:string -> unit -> entry list
  val gc : cache:Cache.t -> int
  val verify : sys:D10.Sysops.t -> cache:Cache.t -> (string * string) list
  val export : cache:Cache.t -> dst:Eio.Fs.dir_ty Eio.Path.t -> int

  val merge_remote :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    index_path:string ->
    remote_path:string ->
    unit
end
