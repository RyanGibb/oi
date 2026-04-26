(** Project metadata: read [*.opam] files in a directory, parse script-style
    dependency annotations, edit [dune-project] files, probe for dev tools, and
    clone remote URL-projects into the pin cache.

    The top-level module reads a project directory's [*.opam] files and surfaces
    the deps, pin-depends, and [x-repos:] declarations a CLI driver needs to
    drive the solver. The submodules cover the adjacent project concerns
    ({!Url}, {!Dune}, {!Script}, {!Tool}). *)

(** {1 Project metadata from a directory} *)

type extra_repo = { name : string; url : string }

type pin = { pkg : OpamPackage.t; url : OpamUrl.t; declared_in : string }
(** [declared_in] is the source [*.opam] filename, used in error messages. *)

type t = {
  deps : string list;
      (** Direct deps, sorted, deduplicated. Excludes local packages and
          "ocaml". *)
  local_packages : string list;
      (** Names of [*.opam] files in the dir (without the [.opam] suffix). *)
  extra_repos : extra_repo list;
      (** URL-form entries from [x-repos:] across all [*.opam]. *)
  pins : pin list;
      (** Union of [pin-depends:] entries across all [*.opam], in declared
          order. *)
  overlays : string list;
      (** Reporepo-handle entries from [x-repos:] (the [@HANDLE] form). *)
}

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> t

(** {1 Script dependency parser}

    OCaml scripts declare deps via [[\@\@\@opam ...]] on the first line.
    Dependency strings accept an opam package name plus an optional version
    constraint ([pkg>=1.0]) and an optional findlib sub-library ([pkg.sub] or
    [pkg.sub>=1.0]). *)

module Script : sig
  type dep = {
    name : OpamPackage.Name.t;
    findlib_name : string;
    constraint_ : OpamFormula.version_constraint option;
  }

  val parse_deps_from_file : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> dep list

  val parse_dep : string -> dep
  (** Script-style dep spec: [.] is a findlib sub-library separator. *)

  val parse_cli_dep : string -> dep
  (** CLI-style dep spec: [.] after the package name is opam's [pkg.version]
      shorthand. *)

  val name_s : dep -> string
  val dedup : dep list -> dep list
  val script_hash : string -> dep list -> string

  val constraints :
    dep list -> OpamFormula.version_constraint OpamTypes.name_map

  val generate_project : script:string -> deps:dep list -> dir:string -> unit
end

(** {1 URL-supplied projects}

    CLI callers pass [--with=URL] to pull a whole upstream opam project into the
    current solve. oi clones [URL] into the pin cache (sharing the
    sentinel-based freshness machinery with {!Source.Pin}), reads every [*.opam]
    at the clone's root, and synthesises one {!pin} per local package so the
    existing pin-depends pipeline can realise them through
    {!Source.Pin.materialize}. *)

module Url : sig
  type nonrec t = {
    pins : pin list;
        (** One synthetic pin per local package, plus every [pin-depends:] entry
            the URL project itself declared. *)
    roots : string list;
        (** Package names that should enter the solve as roots: the local
            packages provided by each URL project. *)
    extra_repos : extra_repo list;
        (** URL entries from the URL project's [x-repos:] field. *)
    overlays : string list;
        (** Reporepo handles from the URL project's [x-repos:] field. *)
  }

  type with_arg = Url of string | Dep of Script.dep

  val classify : string -> with_arg
  (** Strings whose scheme is [http(s)://], [git+…], [git@…], [git://], or
      [ssh://] become {!Url}. Everything else is parsed as an opam package spec
      via {!Script.parse_cli_dep} and returned as {!Dep}. *)

  val materialize :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    cache:Cache.t ->
    ?refresh:bool ->
    string list ->
    t

  val classify_all : string list -> string list * Script.dep list
end

(** {1 dune-project reader/writer} *)

module Dune : sig
  type t

  val load : fs:Eio.Fs.dir_ty Eio.Path.t -> cwd:string -> t
  val generate_opam_files : t -> bool
  val package_names : t -> string list

  val add_dependency :
    t ->
    ?package:string ->
    name:string ->
    constraint_:(string * string) option ->
    unit ->
    t

  val save : fs:Eio.Fs.dir_ty Eio.Path.t -> t -> unit
end

(** {1 Dev-tool probes}

    Tools are opam packages whose binaries are useful during development but
    whose [lib/<pkg>/] trees must not leak into the main project's OCaml search
    path. *)

module Tool : sig
  type trigger = Always | Ocamlformat_file | Dune_project_using of string
  type spec = { name : string; binary : string; trigger : trigger }

  val registry : spec list

  type result = {
    spec : spec;
    hit : bool;
    version : string option;
    detail : string;
  }

  val probe : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> result list
  val hits : result list -> result list
end
