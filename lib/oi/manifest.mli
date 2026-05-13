(** Registry-side manifest produced by joining {!Provenance} (per-layer content
    metadata) with {!Audit} events (per-caller event log).

    One layer can have N callers; the manifest records every distinct
    [(overlay, toolchain, project, trigger)] tuple with its first/last seen
    timestamps and outcome counts, instead of picking a single winner. *)

type caller = {
  overlay : D10.Overlay.t option;
  toolchain : string option;
  trigger : string;
  project : string option;
  first_seen : float;
  last_seen : float;
  count : int;
  outcomes : (Outcome.kind * int) list;
      (** Distinct outcome kinds seen from this caller, with counts. Sorted by
          descending count then by name for stable output. *)
}

type entry = {
  layer_hash : string;
  pkg : Identity.t;
  os_key : string;
  method_ : Identity.method_;
  headline_outcome : Outcome.kind;
      (** Best-known outcome over all callers. [K_ok] when a [Provenance] file
          exists; otherwise picked from the audit events (failure outcomes win
          over success). *)
  built_at : float option;
  duration_s : float option;
  phases : Provenance.phases option;
  opam : Provenance.opam_info option;
  source : Provenance.source_info option;
  deps : Identity.dep list;
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

val pp : t Fmt.t
(** [pp ppf t] renders a one-line summary: schema, OS key and package count. *)

(** {1 Codec} *)

val codec : t Jsont.t

(** {1 Build} *)

val from_logs :
  os_key:string ->
  exported_at:float ->
  Provenance.t list ->
  Audit.event list ->
  t
(** [from_logs ~os_key ~exported_at provs events] joins [provs] with [events] by
    layer hash. Audit events tagged [Solve_key] are bucketed under their own
    digest and produce failure-only entries (no provenance, headline =
    [K_solve_failed]). Audit events tagged [Layer] join the provenance for that
    hash; layers with only failure events get a synthesised failure-only entry.
*)
