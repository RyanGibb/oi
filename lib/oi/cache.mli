(** OI caches (runs, cleanup, build logs, freshness).

    Source tarballs are cached by opam's download cache at
    [{data_dir}/opam-root/download-cache/]. This module manages the script run
    cache, the build-logs directory, sentinel-based freshness for pin and URL
    clones, and cleanup utilities. *)

type t

val create : root:string -> Eio.Fs.dir_ty Eio.Path.t -> t
val root : t -> Eio.Fs.dir_ty Eio.Path.t
val root_s : t -> string
val dune_root : t -> string

val fs : t -> Eio.Fs.dir_ty Eio.Path.t
(** The underlying filesystem capability used to construct the cache; exposed so
    modules that already thread a [Cache.t] don't need to duplicate the [fs]
    argument on every internal helper. *)

(** {1 Script run cache} *)

val run_dir : t -> hash:string -> Eio.Fs.dir_ty Eio.Path.t

(** {1 Pin-depends cache} *)

val pins_dir : t -> string
(** [pins_dir cache] is the root directory for pin-depends caches. Contains
    [sources/] (one dir per pin URL) and [sets/] (synthesized packages/ trees
    keyed by resolved pin-set hash). *)

(** {1 Toolchain install root} *)

val toolchains_root : unit -> string
(** [toolchains_root ()] is the XDG-derived directory under which fixed-prefix
    toolchains are installed ([$XDG_CACHE_HOME/oi/toolchains]). Independent of
    the cache root so {!Toolchain} can compute install prefixes without taking
    a [t]. *)

(** {1 Sentinel-based freshness}

    Shared by pin and URL-project clones. A sentinel file proves the cache
    entry was populated cleanly (partial fetches don't leave one behind). *)

val refresh_max_age : float
(** Seconds. Default [86_400.0] (24h). A clone older than this is pulled again
    on next use. *)

val fresh : refresh:bool -> sentinel:string -> max_age:float -> bool
(** [fresh ~refresh ~sentinel ~max_age] is [true] when a cache entry guarded by
    [sentinel] is still fresh: the sentinel exists and is younger than
    [max_age] seconds. Returns [false] unconditionally when [refresh] is
    [true]. *)

(** {1 Build logs}

    Diagnostic logs ([build], [solve], [fetch], [depext]) under
    [<cache_root>/build/logs/]. All writers are best-effort: I/O failures are
    swallowed so a full disk or permission error on the logs dir can't abort a
    build report. *)

module Logs : sig
  val dir : cache_root:string -> string
  (** [<cache_root>/build/logs] — the directory all log files live in. *)

  val ensure : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> unit
  (** Create the logs directory if absent. Silent on any filesystem error. *)

  val short_hash : string -> string
  (** First 12 chars of a hash — used as a disambiguator in log filenames. *)

  val path :
    cache_root:string -> kind:string -> name:string -> hash:string -> string
  (** Canonical log file path:
      [<cache_root>/build/logs/<kind>-<name>-<short_hash hash>.log]. *)

  val write :
    fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> string -> string -> unit
  (** [write ~fs ~cache_root path content] writes [content] to [path]
      (truncating), creating the parent logs directory if needed. *)
end

(** {1 Cleanup} *)

type item = {
  label : string;
  path : Eio.Fs.dir_ty Eio.Path.t;
  description : string;
}

val cleanable_items : t -> data_dir:string -> item list
val size : sys:D10.Sysops.t -> Eio.Fs.dir_ty Eio.Path.t -> int64
val pp_size : int64 Fmt.t
