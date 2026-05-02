(* -- Types --------------------------------------------------------------- *)

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
}

type entry = {
  layer_hash : string;
  pkg : pkg_id;
  os_key : string;
  method_ : method_;
  headline_outcome : string;
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

let empty_summary =
  {
    ok = 0;
    cached = 0;
    restored = 0;
    build_failed = 0;
    install_failed = 0;
    fetch_failed = 0;
    solve_failed = 0;
    dep_failed = 0;
    depext_missing = 0;
    skipped = 0;
  }

(* -- Outcome ↔ string ---------------------------------------------------- *)

let outcome_name (o : Audit.outcome) =
  match o with
  | Ok -> "ok"
  | Cached -> "cached"
  | Restored -> "restored"
  | Build_failed _ -> "build_failed"
  | Install_failed _ -> "install_failed"
  | Dep_failed _ -> "dep_failed"
  | Fetch_failed _ -> "fetch_failed"
  | Depext_missing _ -> "depext_missing"
  | Solve_failed _ -> "solve_failed"
  | Skipped _ -> "skipped"

(* Failure outcomes outrank success when picking a [headline_outcome] from
   a list of audit events. Provenance presence still wins overall (handled
   in [build]); this only tie-breaks when there's no provenance. *)
let outcome_priority = function
  | "build_failed" | "install_failed" | "fetch_failed" | "solve_failed" -> 0
  | "dep_failed" | "depext_missing" | "skipped" -> 1
  | "ok" -> 2
  | "cached" | "restored" -> 3
  | _ -> 9

let bump_summary s name =
  match name with
  | "ok" -> { s with ok = s.ok + 1 }
  | "cached" -> { s with cached = s.cached + 1 }
  | "restored" -> { s with restored = s.restored + 1 }
  | "build_failed" -> { s with build_failed = s.build_failed + 1 }
  | "install_failed" -> { s with install_failed = s.install_failed + 1 }
  | "fetch_failed" -> { s with fetch_failed = s.fetch_failed + 1 }
  | "solve_failed" -> { s with solve_failed = s.solve_failed + 1 }
  | "dep_failed" -> { s with dep_failed = s.dep_failed + 1 }
  | "depext_missing" -> { s with depext_missing = s.depext_missing + 1 }
  | "skipped" -> { s with skipped = s.skipped + 1 }
  | _ -> s

(* -- Caller aggregation -------------------------------------------------- *)

(* Group key. Collapse identical [(overlay, toolchain, project, trigger)]
   tuples into one caller row. The host field is dropped from the key on
   purpose: the same caller running on two CI workers should aggregate.
   Encoded as a tuple rather than a record so Hashtbl's structural equality
   works without a synthetic [@@deriving] step. *)
type caller_key =
  (string * string) option * string option * string * string option

let key_of_event (e : Audit.event) : caller_key =
  let overlay =
    Option.map
      (fun (o : Audit.overlay_ctx) -> (o.handle, o.version))
      e.context.overlay
  in
  (overlay, e.context.toolchain, e.context.trigger, e.context.project)

(* Increment the count for [name] in an outcome histogram, preserving the
   relative order of earlier entries. Pushes a new entry if [name] hasn't
   been seen before — caller sorts the result before returning. *)
let bump_outcome name = function
  | [] -> [ (name, 1) ]
  | os ->
      let rec walk = function
        | [] -> [ (name, 1) ]
        | (n, c) :: rest when n = name -> (n, c + 1) :: rest
        | head :: rest -> head :: walk rest
      in
      walk os

let aggregate_callers (events : Audit.event list) : caller list =
  let tbl : (caller_key, caller) Hashtbl.t = Hashtbl.create 4 in
  List.iter
    (fun (e : Audit.event) ->
      let k = key_of_event e in
      let nm = outcome_name e.outcome in
      let updated =
        match Hashtbl.find_opt tbl k with
        | None ->
            {
              overlay = e.context.overlay;
              toolchain = e.context.toolchain;
              trigger = e.context.trigger;
              project = e.context.project;
              first_seen = e.ts;
              last_seen = e.ts;
              count = 1;
              outcomes = [ (nm, 1) ];
            }
        | Some c ->
            {
              c with
              first_seen = Float.min c.first_seen e.ts;
              last_seen = Float.max c.last_seen e.ts;
              count = c.count + 1;
              outcomes = bump_outcome nm c.outcomes;
            }
      in
      Hashtbl.replace tbl k updated)
    events;
  let sort_outcomes os =
    List.sort
      (fun (n1, c1) (n2, c2) ->
        if c1 <> c2 then compare c2 c1 else compare n1 n2)
      os
  in
  Hashtbl.fold (fun _ v acc -> v :: acc) tbl []
  |> List.map (fun c -> { c with outcomes = sort_outcomes c.outcomes })
  |> List.sort (fun a b ->
      (* Stable order: oldest invocation first. *)
      compare a.first_seen b.first_seen)

(* -- Failure synthesis from audit-only entries --------------------------- *)

let log_of_event (e : Audit.event) = e.log

let method_of_outcome (o : Audit.outcome) =
  (* Audit-only entries (no Provenance) are typically Source builds that
     failed before the layer could be committed. Use [Source] as a sane
     default; cached/restored shouldn't reach this path in practice. *)
  match o with
  | Ok | Cached | Restored -> Binary
  | _ -> Source

(* Pick the most informative outcome name across [events] using
   [outcome_priority]: failure outcomes outrank success/cached, and we
   tie-break alphabetically for stability. Caller guarantees [events]
   is non-empty. *)
let pick_headline events =
  events
  |> List.map (fun (e : Audit.event) -> outcome_name e.outcome)
  |> List.sort (fun a b ->
      let pa = outcome_priority a and pb = outcome_priority b in
      if pa <> pb then compare pa pb else compare a b)
  |> List.hd

let entry_for_failure_only = function
  | [] ->
      (* [build] only calls this with a non-empty cluster from the events
         hashtable, so the empty case is unreachable. *)
      invalid_arg "Manifest.entry_for_failure_only: no events"
  | (e : Audit.event) :: _ as events ->
      {
        layer_hash = e.layer_hash;
        pkg = { name = e.pkg.name; version = e.pkg.version };
        os_key = e.os_key;
        method_ = method_of_outcome e.outcome;
        headline_outcome = pick_headline events;
        built_at = None;
        duration_s = None;
        phases = None;
        opam = None;
        source = None;
        deps = [];
        depexts_declared = [];
        build_env = None;
        callers = aggregate_callers events;
        log = List.find_map log_of_event events;
      }

(* -- Build --------------------------------------------------------------- *)

let entry_of_provenance (p : Provenance.t) (events : Audit.event list) : entry =
  let callers = aggregate_callers events in
  {
    layer_hash = p.layer_hash;
    pkg = { name = p.pkg.name; version = p.pkg.version };
    os_key = p.os_key;
    method_ = p.method_;
    headline_outcome = "ok";
    built_at = Some p.built_at;
    duration_s = Some p.duration_s;
    phases = Some p.phases;
    opam = Some p.opam;
    source = p.source;
    deps = p.deps;
    depexts_declared = p.depexts_declared;
    build_env = Some p.build_env;
    callers;
    log = None;
  }

let build ~os_key ~exported_at provs events =
  (* Bucket [events] by layer hash for the join. Filtering by [os_key] is
     belt-and-braces: callers should already pass a slice for the os, but
     a stray cross-os event would otherwise show up as a phantom entry. *)
  let events_by_hash : (string, Audit.event list) Hashtbl.t =
    Hashtbl.create 256
  in
  let push tbl key v =
    let prev =
      match Hashtbl.find_opt tbl key with Some xs -> xs | None -> []
    in
    Hashtbl.replace tbl key (v :: prev)
  in
  List.iter
    (fun (e : Audit.event) ->
      if e.os_key = os_key then push events_by_hash e.layer_hash e)
    events;
  let take_events hash =
    match Hashtbl.find_opt events_by_hash hash with
    | Some xs ->
        Hashtbl.remove events_by_hash hash;
        xs
    | None -> []
  in
  let from_provs =
    List.map
      (fun (p : Provenance.t) ->
        entry_of_provenance p (take_events p.layer_hash))
      provs
  in
  (* Anything left in [events_by_hash] now is a layer-less audit cluster —
     typically solve/build failures that never produced a [Provenance.t]. *)
  let from_failures =
    Hashtbl.fold
      (fun _ evs acc -> entry_for_failure_only evs :: acc)
      events_by_hash []
  in
  let results = from_provs @ from_failures in
  let summary =
    List.fold_left
      (fun s e -> bump_summary s e.headline_outcome)
      empty_summary results
  in
  {
    schema = 1;
    os_key;
    exported_at;
    n_packages = List.length results;
    summary;
    results;
  }

(* -- Codec --------------------------------------------------------------- *)

(* Reuse leaf codecs from the producers — same shapes, no point restating.
   [pkg_id] / [method_] are exposed as type aliases of [Provenance]'s, so
   their codecs round-trip through the same JSON. *)
let pkg_id_codec = Provenance.pkg_id_codec
let method_codec = Provenance.method_codec
let dep_codec = Provenance.dep_codec
let phases_codec = Provenance.phases_codec
let opam_info_codec = Provenance.opam_info_codec
let source_info_codec = Provenance.source_info_codec
let build_env_codec = Provenance.build_env_codec
let overlay_ctx_codec = Audit.overlay_ctx_codec
let log_pointer_codec = Audit.log_pointer_codec

let outcome_count_codec : (string * int) Jsont.t =
  let open Jsont in
  Object.map ~kind:"outcome_count" (fun kind count -> (kind, count))
  |> Object.mem "kind" string ~enc:(fun (k, _) -> k)
  |> Object.mem "count" int ~enc:(fun (_, c) -> c)
  |> Object.finish

let caller_codec : caller Jsont.t =
  let open Jsont in
  Object.map ~kind:"caller"
    (fun overlay toolchain trigger project first_seen last_seen count outcomes ->
      {
        overlay;
        toolchain;
        trigger;
        project;
        first_seen;
        last_seen;
        count;
        outcomes;
      })
  |> Object.opt_mem "overlay" overlay_ctx_codec ~enc:(fun c -> c.overlay)
  |> Object.opt_mem "toolchain" string ~enc:(fun c -> c.toolchain)
  |> Object.mem "trigger" string ~enc:(fun c -> c.trigger)
  |> Object.opt_mem "project" string ~enc:(fun c -> c.project)
  |> Object.mem "first_seen" number ~enc:(fun c -> c.first_seen)
  |> Object.mem "last_seen" number ~enc:(fun c -> c.last_seen)
  |> Object.mem "count" int ~enc:(fun c -> c.count)
  |> Object.mem "outcomes" (list outcome_count_codec) ~dec_absent:[]
       ~enc:(fun c -> c.outcomes)
       ~enc_omit:(( = ) [])
  |> Object.finish

let entry_codec : entry Jsont.t =
  let open Jsont in
  Object.map ~kind:"entry"
    (fun
      layer_hash
      pkg
      os_key
      method_
      headline_outcome
      built_at
      duration_s
      phases
      opam
      source
      deps
      depexts_declared
      build_env
      callers
      log
    : entry ->
      {
        layer_hash;
        pkg;
        os_key;
        method_;
        headline_outcome;
        built_at;
        duration_s;
        phases;
        opam;
        source;
        deps;
        depexts_declared;
        build_env;
        callers;
        log;
      })
  |> Object.mem "layer_hash" string ~enc:(fun (e : entry) -> e.layer_hash)
  |> Object.mem "pkg" pkg_id_codec ~enc:(fun (e : entry) -> e.pkg)
  |> Object.mem "os_key" string ~enc:(fun (e : entry) -> e.os_key)
  |> Object.mem "method" method_codec ~enc:(fun (e : entry) -> e.method_)
  |> Object.mem "headline_outcome" string
       ~enc:(fun (e : entry) -> e.headline_outcome)
  |> Object.opt_mem "built_at" number ~enc:(fun (e : entry) -> e.built_at)
  |> Object.opt_mem "duration_s" number ~enc:(fun (e : entry) -> e.duration_s)
  |> Object.opt_mem "phases" phases_codec ~enc:(fun (e : entry) -> e.phases)
  |> Object.opt_mem "opam" opam_info_codec ~enc:(fun (e : entry) -> e.opam)
  |> Object.opt_mem "source" source_info_codec
       ~enc:(fun (e : entry) -> e.source)
  |> Object.mem "deps" (list dep_codec) ~dec_absent:[]
       ~enc:(fun (e : entry) -> e.deps)
       ~enc_omit:(( = ) [])
  |> Object.mem "depexts_declared" (list string) ~dec_absent:[]
       ~enc:(fun (e : entry) -> e.depexts_declared)
       ~enc_omit:(( = ) [])
  |> Object.opt_mem "build_env" build_env_codec
       ~enc:(fun (e : entry) -> e.build_env)
  |> Object.mem "callers" (list caller_codec) ~dec_absent:[]
       ~enc:(fun (e : entry) -> e.callers)
       ~enc_omit:(( = ) [])
  |> Object.opt_mem "log" log_pointer_codec ~enc:(fun (e : entry) -> e.log)
  |> Object.finish

let summary_codec : summary Jsont.t =
  let open Jsont in
  Object.map ~kind:"summary"
    (fun
      ok
      cached
      restored
      build_failed
      install_failed
      fetch_failed
      solve_failed
      dep_failed
      depext_missing
      skipped
    ->
      {
        ok;
        cached;
        restored;
        build_failed;
        install_failed;
        fetch_failed;
        solve_failed;
        dep_failed;
        depext_missing;
        skipped;
      })
  |> Object.mem "ok" int ~enc:(fun s -> s.ok)
  |> Object.mem "cached" int ~enc:(fun s -> s.cached)
  |> Object.mem "restored" int ~enc:(fun s -> s.restored)
  |> Object.mem "build_failed" int ~enc:(fun s -> s.build_failed)
  |> Object.mem "install_failed" int ~enc:(fun s -> s.install_failed)
  |> Object.mem "fetch_failed" int ~enc:(fun s -> s.fetch_failed)
  |> Object.mem "solve_failed" int ~enc:(fun s -> s.solve_failed)
  |> Object.mem "dep_failed" int ~enc:(fun s -> s.dep_failed)
  |> Object.mem "depext_missing" int ~enc:(fun s -> s.depext_missing)
  |> Object.mem "skipped" int ~enc:(fun s -> s.skipped)
  |> Object.finish

let codec : t Jsont.t =
  let open Jsont in
  Object.map ~kind:"manifest"
    (fun schema os_key exported_at n_packages summary results ->
      { schema; os_key; exported_at; n_packages; summary; results })
  |> Object.mem "schema" int ~enc:(fun m -> m.schema)
  |> Object.mem "os_key" string ~enc:(fun m -> m.os_key)
  |> Object.mem "exported_at" number ~enc:(fun m -> m.exported_at)
  |> Object.mem "n_packages" int ~enc:(fun m -> m.n_packages)
  |> Object.mem "summary" summary_codec ~enc:(fun m -> m.summary)
  |> Object.mem "results" (list entry_codec) ~enc:(fun m -> m.results)
  |> Object.finish
