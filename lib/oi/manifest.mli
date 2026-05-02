(** Registry-side manifest produced by joining {!Provenance} (per-layer
    content metadata) with {!Audit} events (per-caller event log).

    One layer can have N callers; the manifest records every distinct
    [(overlay, toolchain, project, trigger)] tuple with its first/last seen
    timestamps and outcome counts, instead of picking a single winner. *)

type pkg_id = Provenance.pkg_id = { name : string; version : string }
type method_ = Provenance.method_ = Source | Binary

type caller = {
  overlay : Audit.overlay_ctx option;
  toolchain : string option;
  trigger : string;
  project : string option;
  first_seen : float;
  last_seen : float;
  count : int;
  outcomes : (string * int) list;
      (** Distinct outcome kinds seen from this caller, with counts.
          Sorted by descending count then by name for stable output. *)
}

type entry = {
  layer_hash : string;
  pkg : pkg_id;
  os_key : string;
  method_ : method_;
  headline_outcome : string;
      (** Best-known outcome over all callers. ["ok"] when a [Provenance]
          file exists; otherwise picked from the audit events
          (failure outcomes win over success). *)
  built_at : float option;
  duration_s : float option;
  phases : Provenance.phases option;
  opam : Provenance.opam_info option;
  source : Provenance.source_info option;
  deps : Provenance.dep list;
  depexts_declared : string list;
  build_env : Provenance.build_env option;
  callers : caller list;
  log : Audit.log_pointer option;
      (** Failure-tail pointer for entries with no provenance. *)
}

type summary = {
  ok : int;
  cached : int;
  restored : int;
  build_failed : int;
  install_failed : int;
  fetch_failed : int;
  solve_failed : int;
  dep_failed : int;
  depext_missing : int;
  skipped : int;
}

type t = {
  schema : int;
  os_key : string;
  exported_at : float;
  n_packages : int;
  summary : summary;
  results : entry list;
}

(** {1 Codec} *)

val codec : t Jsont.t

(** {1 Build} *)

val build :
  os_key:string ->
  exported_at:float ->
  Provenance.t list ->
  Audit.event list ->
  t
(** [build ~os_key ~exported_at provs events] joins [provs] with [events]
    by [layer_hash] and returns a manifest:

    - For each provenance, emit one entry with content fields populated
      from the provenance and a [callers[]] list aggregated from matching
      events.
    - For each event whose [layer_hash] has no matching provenance (the
      typical failed-build case), emit a separate entry whose content
      fields are absent and whose [callers[]] contains a single entry
      synthesised from that event.
*)
