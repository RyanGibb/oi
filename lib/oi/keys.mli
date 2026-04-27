(** Custom opam extension field names used by oi.

    Every [x-*] field oi reads from or writes to an opam file is defined here,
    so a future rename or audit only touches one module. The values are plain
    strings — pass them to {!OpamFile.OPAM.extensions} lookups or write them out
    via the renderers in {!Source}.

    Binding names drop the [x-]/[x-oi-] prefix so [Keys.overlay],
    [Keys.toolchain_name], [Keys.repos], etc. read naturally at call sites. *)

(** {1 Reporepo overlay markers} *)

val overlay : string
(** [x-oi-overlay: true] — marks a reporepo entry as an oi overlay. Entries
    without this flag are skipped by {!Source.Reporepo} loaders. *)

val ref : string
(** [x-oi-ref: <git-ref>] — the git ref the overlay was bumped from. *)

val toolchain : string
(** [x-oi-toolchain: <name>] — use-site tag declaring that this overlay targets
    the named toolchain (e.g. ["oxcaml"]). *)

(** {1 Toolchain definitions}

    An overlay entry is a {e toolchain definition} when it carries
    {!toolchain_name}. The other fields below are only meaningful on toolchain
    definitions and are validated together by [oi repo lint]. *)

val toolchain_name : string
(** [x-oi-toolchain-name: <name>] — when set, this entry DEFINES a toolchain
    with the given CLI name (e.g. ["ocaml-5.4"], ["oxcaml"]). The handle
    namespace and CLI-name namespace are separate; this field is the bridge. *)

val toolchain_compiler : string
(** [x-oi-toolchain-compiler: <pkg.version>] — the primary compiler package spec
    (e.g. ["ocaml-base-compiler.5.4.1"],
    ["ocaml-variants.\ 5.4.0+relocatable"]). Required on toolchain definitions.
*)

val toolchain_roots : string
(** [x-oi-toolchain-roots: [...]] — solver root specs for the toolchain. Either
    a flat list of strings or a list of lists of strings (each inner group acts
    as an OR). *)

val toolchain_tools : string
(** [x-oi-toolchain-tools: [...]] — package names that [oi sync] should always
    install into [_oi/tools/] alongside the toolchain (e.g. [odoc], [merlin],
    [ocaml-lsp-server]). *)

val relocatable : string
(** [x-oi-relocatable: <bool>] — build mode for the toolchain this entry
    defines. *)

val default_toolchain : string
(** [x-oi-default-toolchain: true] — when set, this toolchain is selected when
    no [--toolchain] is given. Exactly one toolchain definition in the reporepo
    may carry this flag (validated by [oi repo lint]). *)

(** {1 Project / overlay payload} *)

val root_packages : string
(** [x-root-packages: [...]] — solver root specs the registry build should solve
    for under this overlay. Same shape as {!toolchain_roots}: a flat list of
    strings or a list of lists. *)

val repos : string
(** [x-repos: [...]] — project [*.opam] field listing extra reporepo handles
    ([@HANDLE]) or repository URLs the project depends on. *)
