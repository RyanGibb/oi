(** A d10 layer hash.

    Identical format to {!D10.Layer.hash} output: hex string, MD5 of the
    concatenated SHA-512 effective-opam hashes of the package and its transitive
    deps. The d10ir library does not compute layer hashes itself — it consumes
    them as identifiers. *)

type t = private string

val of_string : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : t Fmt.t
