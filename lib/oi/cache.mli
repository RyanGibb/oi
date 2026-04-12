(** OI caches (runs, cleanup).

    Source tarballs are cached by opam's download cache at
    [{data_dir}/opam-root/download-cache/]. This module manages the script run
    cache and cleanup utilities. *)

type t

val create : root:string -> Eio.Fs.dir_ty Eio.Path.t -> t
val root : t -> Eio.Fs.dir_ty Eio.Path.t
val root_s : t -> string
val dune_root : t -> string

(** {1 Script run cache} *)

val run_dir : t -> hash:string -> Eio.Fs.dir_ty Eio.Path.t

(** {1 Cleanup} *)

type item = {
  label : string;
  path : Eio.Fs.dir_ty Eio.Path.t;
  description : string;
}

val cleanable_items : t -> data_dir:string -> item list
val size : sys:D10.Sysops.t -> Eio.Fs.dir_ty Eio.Path.t -> int64
val pp_size : int64 Fmt.t
