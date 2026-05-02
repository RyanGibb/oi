(** Append-only build event log.

    Single jsonl file at [<cache>/build/audit.jsonl]. One line per terminal
    package event in any [oi build] (or [oi run]) invocation. Events carry
    caller-specific context (overlay handle, toolchain, project, trigger
    string) so the registry manifest can show the full set of callers that
    contributed to each layer rather than picking a single winner.

    Append is concurrency-safe under [O_APPEND] for line lengths well below
    [PIPE_BUF] (~4 KiB on Linux), which a single event easily fits within. *)

type pkg_id = { name : string; version : string }
type fetch_kind =
  | Http_status of int
  | Checksum_mismatch
  | Network_timeout
  | Git_failed
  | Other of string

type dep = { name : string; version : string; hash : string }

type outcome =
  | Ok
      (** Source build completed and the layer was committed. Pairs with a
          [Provenance.t] under the same [layer_hash]. *)
  | Cached  (** The fast-path emitted a Cached event without invoking Execute. *)
  | Restored
      (** A Binary package was restored from a layer that was already in the
          local cache (i.e. Execute.run's restore phase). *)
  | Build_failed of { command : string; exit_code : int option }
  | Install_failed of { command : string; exit_code : int option }
  | Dep_failed of { upstream : dep }
  | Fetch_failed of { url : string; kind : fetch_kind }
  | Depext_missing of { missing : string list; not_found : string list }
  | Solve_failed of { reason : string }
  | Skipped of { reason : string }

type overlay_ctx = { handle : string; version : string }

type context = {
  overlay : overlay_ctx option;
  toolchain : string option;
  trigger : string;  (** Argv summary of the [oi] invocation. *)
  project : string option;
  host : string;
}

type log_pointer = { text_path : string; tail : string option }

type event = {
  schema : int;
  event_id : string;  (** ULID — sortable, dedupable. *)
  invocation_id : string;
      (** Shared across every event from one [oi] run. *)
  ts : float;
  os_key : string;
  layer_hash : string;
      (** For terminal package events this is the layer hash. For
          [Solve_failed] events it's a stable digest of the solve key
          (matching the failure log filename). *)
  pkg : pkg_id;
  outcome : outcome;
  duration_s : float;
  context : context;
  log : log_pointer option;  (** Present on failure events. *)
}

(** {1 Codecs} *)

val event_codec : event Jsont.t

(** Leaf codecs are exposed so {!Manifest} can re-use them when encoding
    audit-derived shapes inside its envelope without restating the schema. *)

val pkg_id_codec : pkg_id Jsont.t
val dep_codec : dep Jsont.t
val overlay_ctx_codec : overlay_ctx Jsont.t
val log_pointer_codec : log_pointer Jsont.t

(** {1 Storage} *)

val path : cache_root:string -> string
(** [<cache_root>/build/audit.jsonl]. *)

val append : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> event -> unit
(** Append [event] as a single JSON line to {!path}. Errors are logged and
    swallowed so logging failure cannot abort the build. *)

val read_all :
  fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> os_key:string -> event list
(** Read every line of {!path}, decode, and filter by [os_key]. Lines that
    fail to decode are skipped with a debug log. *)

val per_os_path : output_dir:string -> os_key:string -> string
(** [<output_dir>/<os_key>/audit.jsonl] — the registry-side per-os audit
    file emitted by [oi build --export]. *)

val write_per_os :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  output_dir:string ->
  os_key:string ->
  event list ->
  unit
(** Write [events] (sorted by [event_id]) to {!per_os_path} as jsonl. *)

(** {1 IDs and helpers} *)

val ulid : unit -> string
(** A Crockford-base32 ULID. 26 chars: 48-bit ms-since-epoch + 80-bit random. *)

val invocation_id : unit -> string
(** Cached per-process ULID. Every event from a single [oi] invocation
    shares this id. *)

val default_context : unit -> context
(** Construct a base [context] from the current process: [trigger] from
    [Sys.argv], [host] from [Unix.gethostname], and [overlay] / [toolchain]
    / [project] all left [None]. Producers fill in those three fields
    locally before calling {!append}. *)

val classify_fetch_msg : string -> fetch_kind
(** Best-effort classification of an OpamRepository / curl error message. *)

val tail_of_file : path:string -> string option
(** Last ~150 lines of [path], or [None] when the file is missing. Used to
    embed the failure tail directly in the audit event. *)
