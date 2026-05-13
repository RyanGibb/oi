(** Where on disk the opam file for a package was sourced from.

    Single source of truth for path-shape parsing. {!of_packages_dir}, given the
    [packages/] directory the solver picked the opam from, plus the package's
    name and [name.version], returns a structured {!t} carrying both the kind
    tag and the (handle, version) pair where applicable. *)

(** What kind of source contributed this opam file. *)
type kind =
  | Reporepo
      (** Materialised reporepo overlay tree at
          [<reporepo>/v2/<handle>/packages]. The [overlay] field is set. *)
  | Pin
      (** Synthesised pin-depends tree at [<cache>/pins/sets/<hash>/packages].
      *)
  | Local
      (** Project-local [packages/] tree (a [repo] file marker), or anything
          else that doesn't match a known layout. *)

type t = {
  kind : kind;
  overlay : D10.Overlay.t option;
      (** Set for [Reporepo] when the version is known on disk; otherwise
          [None]. The reporepo's materialised v1 layout doesn't carry the
          overlay version in the path, so [version] may be empty even when
          [handle] is set — callers fall back to an unversioned overlay. *)
  path_in_repo : string;
      (** Filesystem path of the opam file relative to the source repo root
          (e.g. [v2/avsm/packages/bsky/bsky.dev/opam] for a reporepo entry,
          [packages/foo/foo.1.0.0/opam] for a pin or local entry). *)
}

val of_packages_dir : pkgs_dir:string -> name:string -> full:string -> t
(** [of_packages_dir ~pkgs_dir ~name ~full] derives the origin from the
    [packages/] directory the opam file lives in, plus the package's [name] and
    [full] ([name.version]) for path reconstruction. The path-shape rules are:

    - [<root>/v2/<handle>/packages] → [Reporepo (Some handle)].
    - [<root>/v2/reporepo/packages] → [Reporepo (Some "reporepo")].
    - [<root>/pins/sets/<hash>/packages] → [Pin].
    - everything else → [Local]. *)

val pp_kind : kind Fmt.t

val pp : t Fmt.t
(** [pp] renders [<kind>@<path_in_repo>]. *)

(** {1 Codec} *)

val kind_codec : kind Jsont.t
(** [kind_codec] (de)serialises a {!kind} tag as JSON. *)

val codec : t Jsont.t
(** [codec] (de)serialises an {!t} as JSON. *)
