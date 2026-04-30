(** Per-package build outcome record.

    Written to [<cache>/build/logs/<name>.<ver>-<short_hash>.json] after every
    build attempt (success or failure). Bundled into a single
    [<out>/<os_key>/logs/manifest.json] at registry export time so registry
    consumers can inspect what built, what failed, and why. *)

type pkg_id = { name : string; version : string }
type method_ = Source | Binary
type dep = { name : string; version : string; hash : string }

type depexts = {
  declared : string list;
  installed : string list;
  missing : string list;
  not_found : string list;
}

type source_info = { url : string; checksums : string list }
type overlay = { handle : string; version : string }

type log_pointer = {
  text_path : string;
      (** Cache-relative path to the plain-text log, when one was written. *)
  tail : string option;
      (** Last [n_tail_lines] of the text log, embedded for inline inspection.
          Only set for failure outcomes. *)
}

type fetch_kind =
  | Http_status of int  (** HTTP 4xx / 5xx returned by curl. *)
  | Checksum_mismatch  (** opam reported a hash mismatch. *)
  | Network_timeout  (** Curl/git timed out or hit a DNS failure. *)
  | Git_failed  (** [git clone] / [git fetch] non-zero. *)
  | Other of string  (** Anything that didn't match a known pattern. *)

type outcome =
  | Ok_built  (** Source build completed and the layer was stored. *)
  | Cached  (** Restored from a prior layer, no work done. *)
  | Skipped of { reason : string }
  | Build_failed of { command : string; exit_code : int option }
  | Install_failed of { command : string; exit_code : int option }
  | Dep_failed of { upstream : dep }
  | Fetch_failed of { url : string; kind : fetch_kind }
  | Depext_missing of { missing : string list; not_found : string list }
  | Solve_failed of { reason : string }

type phase_durations = {
  fetch : float option;  (** Seconds spent in [opam pull-tree]. *)
  build : float option;  (** Seconds spent in build commands. *)
  install : float option;  (** Seconds spent in install commands + .install. *)
  restore : float option;  (** Seconds spent restoring a binary layer. *)
}

type t = {
  schema : int;  (** Always [1] for now. *)
  pkg : pkg_id;
  layer_hash : string;
  os_key : string;
  method_ : method_;
  started_at : float;  (** Unix epoch seconds when the package was picked up. *)
  duration_s : float;  (** Wall-clock seconds end-to-end. *)
  outcome : outcome;
  deps : dep list;
  depexts : depexts;
  source : source_info option;
  log : log_pointer option;
  overlay : overlay option;
  phases : phase_durations;
}

(** {1 Codec} *)

val codec : t Jsont.t

(** {1 Storage} *)

val n_tail_lines : int
(** Number of trailing lines included in [log.tail]. *)

val sidecar_path :
  cache_root:string -> name:string -> version:string -> hash:string -> string
(** Path of the transient JSON sidecar for a (name, version, hash) tuple under
    [<cache_root>/build/logs/]. Wiped by [oi clean]; read again on next export.
*)

val write : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> t -> unit
(** Encode [t] and write to {!sidecar_path}. Errors are logged and swallowed: a
    logging failure must never abort the build. *)

val layer_log_path : cache_root:string -> os_key:string -> hash:string -> string
(** Path of the per-layer proof-of-build at
    [<cache_root>/layers/<os_key>/<hash>/build_log.json]. Lives next to
    [layer.json] and survives subsequent cache hits, so a manifest reconstructed
    from these records reflects how each layer was originally built rather than
    "cached" for every restore. *)

val write_layer :
  fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> os_key:string -> t -> unit
(** Write [t] to {!layer_log_path} when the layer dir exists. No-op when the
    layer hasn't been committed yet (e.g. mid-build) so the proof can never
    drift from the layer it documents. *)

val tail_of_file : path:string -> string option
(** Last {!n_tail_lines} of [path], or [None] when the file is missing. *)

val classify_fetch_msg : string -> fetch_kind
(** Best-effort classification of an OpamRepository / curl error message. *)

val declared_depexts : env:OpamFilter.env -> OpamFile.OPAM.t -> string list
(** Active system-package names declared by [opam] under [env]. Empty when the
    package declares no depexts or none survive filter evaluation. *)

(** {1 Parsing helpers} *)

val parse_pkg : string -> pkg_id
(** Split a [name.version] string. Falls back to [{name = s; version = ""}]. *)

val empty_phases : phase_durations
val empty_depexts : depexts

(** {1 Aggregate manifest} *)

module Manifest : sig
  type summary = {
    ok : int;
    cached : int;
    build_failed : int;
    install_failed : int;
    dep_failed : int;
    fetch_failed : int;
    depext_missing : int;
    solve_failed : int;
    skipped : int;
  }

  type manifest = {
    schema : int;
    os_key : string;
    exported_at : float;
    n_packages : int;
    summary : summary;
    results : t list;
  }

  val codec : manifest Jsont.t
  val of_records : os_key:string -> exported_at:float -> t list -> manifest

  val read_sidecars : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> t list
  (** Read every [*.json] under [<cache_root>/build/logs/]. Files that fail to
      decode are skipped with a debug log. *)

  val read_layer_logs :
    fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> os_key:string -> t list
  (** Read [<cache_root>/layers/<os_key>/*/build_log.json] — the proof-of-build
      records attached to each cached layer. *)

  val read_all :
    fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> os_key:string -> t list
  (** Union of {!read_layer_logs} and the {!read_sidecars} entries that match
      [os_key], keyed by [layer_hash]. Layer-stored records take precedence so a
      [Cached] sidecar from this run is replaced by the original [Ok_built]
      proof. Transient-only entries (failures, [Solve_failed], etc.) are kept
      as-is. *)
end
