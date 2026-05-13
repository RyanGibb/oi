(** Package identity types shared across the audit / provenance / manifest /
    plan modules.

    Every place that needs to talk about "this opam package, and how it relates
    to a layer" reaches for these — one set of records, one set of codecs, no
    conversion shims at the boundaries. *)

type t = { name : string; version : string }
(** A name + version pair. ["foo"] / ["1.2.3"]. *)

val of_string : string -> t
(** [of_string] Parse a [name.version] string. Falls back to
    [{ name = s; version = "" }] when the input doesn't match opam's package
    shape. *)

val to_string : t -> string
(** [to_string t] is [name ^ "." ^ version], or just [name] when [version] is
    empty. *)

val of_opam : OpamPackage.t -> t
(** [of_opam pkg] projects an opam package to its name/version pair. *)

type dep = { id : t; hash : string }
(** A layer dependency: an [Identity.t] paired with the d10 layer hash that
    holds the dependency's compiled output. Used by both the build plan
    ([Plan.package_plan.dep_layers]) and the provenance / audit records. *)

val dep_of_opam : OpamPackage.t -> hash:string -> dep
(** Build a {!dep} from an opam package and the layer hash that supplies its
    compiled output. *)

(** How a package made it into the prefix.

    - [Source] — built from source during this run (or fetched from a remote
      registry, then promoted).
    - [Binary] — restored from an existing layer in the local d10 cache. *)
type method_ = Source | Binary

val string_of_method : method_ -> string
(** [string_of_method] Lowercase tag (["source"] / ["binary"]) used in CLI
    output and JSON. *)

(** {1 Codecs} *)

val codec : t Jsont.t
(** [codec] (de)serialises an {!t} as JSON. *)

val dep_codec : dep Jsont.t
(** [dep_codec] (de)serialises a {!dep} as JSON. *)

val method_codec : method_ Jsont.t
(** [method_codec] (de)serialises a {!method_} as JSON. *)
