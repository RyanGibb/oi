(** Per-layer content provenance — the immutable proof of what went into
    a successfully-built layer.

    Lives at [<cache>/layers/<os_key>/<hash>/provenance.json]. Written once by
    {!write} when the layer is committed and never overwritten. The file
    captures only content-addressed facts (opam origin, source URL+checksum,
    dep hashes) — caller context lives in the {!Audit} log instead.

    Together with the transitive closure of [deps[].hash] and the
    [opam.origin] pointer, a [provenance.json] is sufficient to refetch every
    input that contributed to the layer's hash. *)

type pkg_id = { name : string; version : string }
type method_ = Source | Binary

type opam_origin_kind = Reporepo | Pin | Url_project | Local

type opam_origin = {
  kind : opam_origin_kind;
  handle : string option;
      (** Reporepo overlay handle when [kind = Reporepo]. *)
  path_in_repo : string;
      (** Filesystem path of the opam file relative to the source repo
          root (e.g. ["v1/avsm/packages/bsky/bsky.dev/opam"]). *)
}

type opam_info = {
  sha256 : string;
      (** Hex SHA-256 of the opam file's [effective_part] bytes — the
          same bytes [D10.Layer.hash] consumed when computing
          [layer_hash]. *)
  origin : opam_origin;
}

type source_info = {
  url : string;
  kind : string;  (** "git" | "tar" | "local" | "" — derived from [url]. *)
  checksums : string list;
}

type dep = { name : string; version : string; hash : string }

type phases = {
  fetch : float option;
  build : float option;
  install : float option;
  restore : float option;
}

type build_env = { ocaml_version : string }

type t = {
  schema : int;  (** Always [1]. *)
  layer_hash : string;
  os_key : string;
  pkg : pkg_id;
  method_ : method_;
  built_at : float;  (** Unix seconds when this layer was committed. *)
  duration_s : float;
  phases : phases;
  opam : opam_info;
  source : source_info option;
  deps : dep list;
  depexts_declared : string list;
  build_env : build_env;
}

(** {1 Codecs} *)

val codec : t Jsont.t

(** Leaf codecs are exposed so {!Manifest} can re-encode the same shapes
    inside its own envelope without restating the schema. *)

val pkg_id_codec : pkg_id Jsont.t
val method_codec : method_ Jsont.t
val dep_codec : dep Jsont.t
val phases_codec : phases Jsont.t
val opam_info_codec : opam_info Jsont.t
val source_info_codec : source_info Jsont.t
val build_env_codec : build_env Jsont.t

(** {1 Storage} *)

val path : cache_root:string -> os_key:string -> hash:string -> string
(** [<cache_root>/layers/<os_key>/<hash>/provenance.json]. *)

val write : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> t -> unit
(** Encode [r] and write to {!path}. The layer dir must already exist
    (committed by [D10.Layer.store]); silently no-ops if it doesn't. Errors
    are logged and swallowed: a logging failure must never abort the build. *)

val read_all :
  fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> os_key:string -> t list
(** Walk every [<cache>/layers/<os_key>/<hash>/provenance.json] and decode.
    Layer dirs without a [provenance.json] are silently skipped. Files that
    fail to decode are logged at debug and skipped. *)

val load :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  os_key:string ->
  hash:string ->
  t option
(** [load ~fs ~cache_root ~os_key ~hash] reads a single [provenance.json]
    by layer hash. [None] when the file is missing or fails to decode. *)

(** {1 Helpers for producers} *)

val hash_opam_file : path:string -> string
(** Hex SHA-256 of [path]'s [effective_part]-equivalent bytes. Used by
    {!Plan} to populate {!opam_info.sha256}. Returns [""] if the file can't
    be read. *)

val classify_url : string -> string
(** ["git"] if URL starts with [git+] or contains [.git#], ["tar"] for known
    archive extensions, ["local"] for [file://]/local paths, [""] otherwise. *)

val origin_of_pkgs_dir : pkgs_dir:string -> name:string -> full:string -> opam_origin
(** Derive an [opam_origin] from the resolved [packages/] directory the opam
    file was found in, plus the package's [name] and [name.version]. The
    rules match {!Plan.overlay_of_packages_dir}: a [v1/<handle>/packages]
    parent yields [Reporepo], a [pins/sets/<hash>/packages] parent yields
    [Pin], anything else falls back to [Local]. *)
