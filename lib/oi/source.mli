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
    toolchain_tools : string list;
        (** [x-oi-toolchain-tools]: package names that [oi build] should always
            install when this toolchain is active (typically dev tools like
            [odoc], [merlin], [ocaml-lsp-server]). Empty for non-toolchain
            entries and toolchains that don't ship a default tool set. *)
    default_toolchain : bool;
        (** [x-oi-default-toolchain]: when [true], this toolchain is selected
            when no [--toolchain] is passed on the CLI. {!load} validates that
            at most one toolchain handle in the reporepo carries this flag. *)
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

  val default_toolchain : entry list -> entry option
  (** Latest version of the toolchain definition flagged
      [x-oi-default-toolchain: true], or [None] if no entry carries the flag.
      {!load} guarantees at most one toolchain handle is so flagged. *)

  type root = { handle : string; version : string option }

  val resolve : entry list -> roots:root list -> entry list
  (** Transitive closure in dependency order (deps before dependents). If a root
      has no [version], the highest version is picked. *)

  (** {2 v1 overlay layout}

      The reporepo's [v1/] subtree carries one fully-materialised opam
      repository per handle. Each handle's [opam] files have had every
      [url{}] block rewritten to be content-addressed (sha-pinned git URL or
      tarball+checksum), turning the reporepo into a deterministic snapshot
      of every overlay's package data. The [v1] prefix is a schema marker;
      a future incompatible change becomes [v2]. *)

  val v1_root : path:string -> string
  (** [<reporepo>/v1]. *)

  val overlay_dir : path:string -> handle:string -> string
  (** [<reporepo>/v1/<handle>]. *)

  val overlay_packages_dir : path:string -> handle:string -> string
  (** [<reporepo>/v1/<handle>/packages] — directly consumable as a solver
      [packages_dir]. *)

  val assert_overlay_dir : path:string -> handle:string -> string
  (** Like {!overlay_packages_dir}, but errors with a "run [oi repo bump]"
      hint when the directory is missing on disk. *)

  type materialise_summary = {
    handle : string;
    total : int;  (** number of packages walked *)
    kept : int;  (** url already content-addressed *)
    rewrote : int;  (** url rewritten to a sha-pinned form *)
    unavailable : (string * string) list;
        (** [(pkg.version, reason)] for entries marked [available: false]. *)
  }

  val materialise_handle :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    path:string ->
    handle:string ->
    url:string ->
    commit:string ->
    materialise_summary
  (** Clone [url] at [commit] into a scratch directory, walk every
      [packages/<pkg>/<pkg>.<ver>/opam] file, rewrite each [url{}] block
      and pin-depends entry to a content-addressed form, and write the
      result to [<reporepo>/v1/<handle>/]. The write is atomic from the
      caller's POV: a sibling [v1/<handle>.tmp/] is built first, then
      rotated into place at the end.

      Hard-errors on unsupported VCS backends (darcs, hg, rsync). Persistent
      network failures (git ls-remote unreachable, tarball 404 after retry)
      mark the offending package [available: false] with an
      [x-oi-unavailable:] explanation rather than failing the whole bump. *)

  val try_resolve_url :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    where:string ->
    OpamUrl.t ->
    has_checksum:bool ->
    [ `Keep
    | `Replace_url of OpamUrl.t
    | `Add_checksum of OpamHash.t
    | `Failed of string ]
  (** [try_resolve_url ~fs ~sys ~where url ~has_checksum] performs the same
      content-address resolution as the per-overlay materialiser does for one
      [url{}] block: applies host rewrites, returns [`Keep] when the URL is
      already pinned, [`Replace_url] for a sha-pinned/host-rewritten git URL,
      [`Add_checksum] for a tarball whose sha-256 we just computed, or
      [`Failed] when the URL cannot be content-addressed. [where] is a
      free-form label used in error messages. *)

  (** {2 Base overlay resolution} *)

  val ensure_base :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    data_dir:string ->
    ?refresh:bool ->
    unit ->
    string list
  (** Resolves the [relocatable] overlay (and its transitive deps) from the
      reporepo and returns the [packages/] directories under [v1/<handle>/]
      in solver priority order. Auto-clones the reporepo itself if it doesn't
      already exist on disk. Errors when any base handle's [v1/] tree is
      missing — run [oi repo bump <handle>] to populate. *)

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
    ?default:bool ->
    unit ->
    [ `Bumped of entry | `Unchanged of entry ]
  (** [?default] flips the [x-oi-default-toolchain] flag on the bumped entry.
      Only valid for entries that already carry [x-oi-toolchain-name] (the
      function errors otherwise). Callers that want to switch the default from
      one toolchain to another are responsible for clearing the flag on the
      previous default before setting it on the new one — {!load} otherwise
      refuses to load a multi-default reporepo. *)

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

  val resolve_pins :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    ?project_root:string ->
    Project.pin list ->
    Project.pin list
  (** Pin the URL of every entry in [pins] to a deterministic form (sha-pinned
      git URL or unchanged tarball), consulting/refreshing
      [<project_root>/_oi/oi.lock] when supplied. With no [project_root], or
      with an empty list, the input is returned unchanged. The lock file is
      transient state under [_oi/] — never committed; deleting it forces a
      re-resolution on the next [oi build].

      Errors when a pin's URL cannot be resolved (network down, ref gone). *)
end

(** {1 Source tarball mirror}

    Static, content-addressed source mirror produced by
    [oi registry mirror build] from the reporepo's [v1/] tree. The on-disk
    layout is what opam expects for a [cache_url]:

    <mirror>/<algo>/<first-2-chars>/<full-hash>

    where <algo> is [md5], [sha256], or [sha512]. The directory itself is
    the only metadata — [stats], [verify], and [export] all walk it
    directly. *)

module Mirror : sig
  val dir : cache:Cache.t -> string
  (** Absolute path to the mirror root: [{cache_root}/mirror]. *)

  val url : cache:Cache.t -> OpamUrl.t
  (** [file://<dir>], suitable for use as a [cache_url] in
      [OpamRepository.pull_*]. *)

  val remote_url : registry:string -> OpamUrl.t
  (** [<registry>/sources] as an [OpamUrl.t]. *)

  type stats = { count : int; total_size : int64 }

  val stats : cache:Cache.t -> stats
  (** Walk the mirror directory and report blob count + total size. *)

  val export : cache:Cache.t -> dst:Eio.Fs.dir_ty Eio.Path.t -> int
  (** Hardlink-copy the mirror tree to [<dst>/sources/]. Returns the number
      of blobs copied. *)
end
