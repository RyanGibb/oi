[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Fixed-prefix OCaml toolchains.

    A toolchain is a bundle of OCaml compiler + findlib + ocamlbuild (+ maybe
    dune) that lives at a persistent, well-known path and is shared across all
    consumer prefixes via PATH / OCAMLPATH layering rather than hardlinks.
    Needed for compilers whose [--prefix] is baked into the binary (oxcaml,
    upstream [ocaml-base-compiler]): copying such a compiler into a per-solve
    consumer prefix would break, because [Config.standard_library] points back
    at the original install path.

    Toolchains are reporepo-defined: each is a definition-only entry (no own
    [url:], just [depends:] composing existing overlays) carrying
    [x-oi-toolchain-name], [x-oi-toolchain-compiler], [x-oi-relocatable], and
    [x-oi-toolchain-roots]. The reporepo handle (e.g. [toolchain-oxcaml]) and
    the CLI toolchain name (e.g. [oxcaml] from [x-oi-toolchain-name]) live in
    separate namespaces; the latter is what [--toolchain=NAME] resolves against.

    On resolve the toolchain entry's [depends:] are walked transitively, the
    URL-bearing overlays in scope are materialised, the [x-oi-toolchain-roots]
    are solved, and the install prefix is derived as
    [$XDG_CACHE_HOME/oi/toolchains/<name>-<version>-<hash8>/]. On first use a
    non-relocatable toolchain is built there; subsequent runs skip the install.
    Relocatable toolchains skip the fixed-prefix install entirely. *)

type info = {
  handle : string;  (** Toolchain handle, e.g. [oxcaml]. *)
  ocaml_version : string;
      (** Effective [ocaml-variants] / [ocaml-base-compiler] version (e.g.
          [5.2.0+ox] or [5.3.0]). Derived from the solved packages. *)
  install_prefix : string;
      (** Absolute path where a non-relocatable toolchain lives. Computed even
          for relocatable toolchains (folded into the cache key / identity), but
          the directory is never created on disk in that case. *)
  hash : string;  (** 64-hex content hash of the resolved opam files + conf. *)
  relocatable : bool;
      (** [true] when the toolchain's compiler can be installed into a per-solve
          consumer prefix — {!ensure_installed} is then a no-op and downstream
          env / [mark_installed] paths skip the fixed-prefix treatment. The
          toolchain still pins the compiler version via {!opam_ctx_of_info}. *)
  packages : OpamPackage.Set.t;  (** Packages installed in the toolchain. *)
  compiler_name : OpamPackage.Name.t;
      (** The single compiler-package name this toolchain installs (parsed from
          [x-oi-toolchain-compiler], e.g. [ocaml-base-compiler] for upstream,
          [ocaml-variants] for relocatable / oxcaml). Replaces the hardcoded
          [["ocaml"; "ocaml-base-compiler"; "ocaml-variants"; ...]] families
          previously sprinkled across the codebase: every site that wants to ask
          "is this package the toolchain's compiler?" reads this field. *)
  root_names : OpamPackage.Name.Set.t;
      (** Names originally passed as root packages to the toolchain solver (e.g.
          [ocaml-variants], [ocamlfind]). Subset of [packages] by name. The
          consumer solver adds just these as required roots — adding the full
          [packages] set (transitive closure) pulls in oxcaml meta packages that
          trigger unsatisfiable conflicts. *)
  packages_dirs : string list;
      (** Packages directories used to resolve [packages]. Preserved for later
          solves that want to consult the toolchain's opam metadata. *)
  tools : string list;
      (** Package names from the toolchain's [x-oi-toolchain-tools] field.
          [oi build] installs these unconditionally into [_oi/tools/] when this
          toolchain is active. Empty when the toolchain ships no default tool
          set; per-project triggered tools (mdx when dune-project uses it,
          ocamlformat when .ocamlformat is present) live in code, not here. *)
  dep_handles : string list;
      (** Reporepo overlay handles the toolchain layers under (e.g.
          [["default"]]). Surfaced verbatim by [oi show]'s [Repositories] block.
      *)
}

val opam_ctx_of_info : info -> Solver.Ctx.toolchain
(** Project a {!info} down to the {!Solver.Ctx.toolchain} subset that
    [Solver.Ctx.create] / [Solver.solve] / [Prefix.make_env] need. Single source
    of truth for the conversion, used by all the CLI commands that thread a
    toolchain through. *)

val apply_conf : info option -> Solver.Ctx.conf -> Solver.Ctx.conf
(** [apply_conf info conf] is [conf] with [ocaml_version] replaced by the
    toolchain's solved version when [info] is set, unchanged otherwise.
    Consumers that thread an [Solver.Ctx.conf] through to a solve / build need
    this so [ocaml:version] resolves to the toolchain's compiler. *)

val default_root : unit -> string
(** Root dir under which toolchains are installed, i.e.
    [$XDG_CACHE_HOME/oi/toolchains]. *)

val resolve :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  data_dir:string ->
  conf:Solver.Ctx.conf ->
  handle:string ->
  info
(** Look up [handle] (the CLI toolchain name) in the reporepo: find the latest
    entry whose [x-oi-toolchain-name] matches, resolve its [depends:]
    transitively, materialise the URL-bearing overlays, solve the
    [x-oi-toolchain-roots], compute the effective hash, and return [info].
    Auto-clones the reporepo if missing. Raises {!Error.config_error} if no
    entry defines a toolchain with that name. *)

val url_of : handle:string -> string option
(** [url_of ~handle] returns the URL of the toolchain's primary source — by
    convention the URL of the first overlay listed in the toolchain entry's
    [depends:] (e.g. [oxcaml] for the oxcaml toolchain). [None] when no reporepo
    entry defines the named toolchain. Used by [oi show] to display the
    toolchain's source repo. *)

val depends_of : handle:string -> string list option
(** [depends_of ~handle] returns the reporepo overlay handles the named
    toolchain layers under (e.g. [Some ["oxcaml"; "default"]] for [oxcaml]).
    [None] when no reporepo entry defines the named toolchain. Used by
    [oi repo add]/[bump] when an overlay declares [x-oi-toolchain] so the
    auto-injected base pins match the toolchain's own base set. *)

type summary = {
  handle : string;
  url : string;
  ref_ : string option;  (** Git ref tracked, e.g. [Some "relocatable"]. *)
  relocatable : bool;
      (** [true] when the toolchain skips the fixed-prefix install. *)
  is_default : bool;
      (** [true] when this toolchain's latest entry carries
          [x-oi-default-toolchain: true]. *)
  depends : string list;
  roots : string list;
  tools : string list;
      (** Always-on dev tools the toolchain installs into [_oi/tools/] on each
          [oi build] (from [x-oi-toolchain-tools]). *)
  installs : (string * bool) list;
      (** Existing install prefixes on disk for this handle, paired with whether
          each one has its [.oi-toolchain-ready] sentinel. The list reflects
          whatever solves have been cached so far — without a solve we can't
          know the canonical prefix for this machine, so callers display all of
          them. *)
}

val available : unit -> summary list
(** Snapshot of every toolchain definition the reporepo currently advertises
    (entries carrying [x-oi-toolchain-name]) plus any of their install prefixes
    already present under {!default_root}. Cheap (no solve, no network), so safe
    for [oi config]. Empty when the reporepo has no toolchain entries — fresh
    machines need to clone or create them. *)

val is_ready : info -> bool
(** [true] when the install_prefix has an [.oi-toolchain-ready] sentinel
    (non-relocatable toolchains) or unconditionally for relocatable ones, which
    need no on-disk preparation. *)

val ensure_installed :
  ?reporter:Build_progress.reporter ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  info ->
  unit
(** Build the toolchain into its fixed prefix when {!is_ready} is false. No-op
    for relocatable toolchains and for already-prepared non-relocatable ones. On
    failure for a non-relocatable toolchain the install_prefix is left partial
    and {!is_ready} continues to return false.

    [?reporter] receives a [Status "Installing toolchain <handle>"] event
    when the install actually runs. *)
