let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.build_log"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Types --------------------------------------------------------------- *)

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
type log_pointer = { text_path : string; tail : string option }

type fetch_kind =
  | Http_status of int
  | Checksum_mismatch
  | Network_timeout
  | Git_failed
  | Other of string

type outcome =
  | Ok_built
  | Cached
  | Skipped of { reason : string }
  | Build_failed of { command : string; exit_code : int option }
  | Install_failed of { command : string; exit_code : int option }
  | Dep_failed of { upstream : dep }
  | Fetch_failed of { url : string; kind : fetch_kind }
  | Depext_missing of { missing : string list; not_found : string list }
  | Solve_failed of { reason : string }

type phase_durations = {
  fetch : float option;
  build : float option;
  install : float option;
  restore : float option;
}

type t = {
  schema : int;
  pkg : pkg_id;
  layer_hash : string;
  os_key : string;
  method_ : method_;
  started_at : float;
  duration_s : float;
  outcome : outcome;
  deps : dep list;
  depexts : depexts;
  source : source_info option;
  log : log_pointer option;
  overlay : overlay option;
  phases : phase_durations;
}

let n_tail_lines = 150

let empty_phases =
  { fetch = None; build = None; install = None; restore = None }

let empty_depexts =
  { declared = []; installed = []; missing = []; not_found = [] }

(* -- Parsing helpers ------------------------------------------------------ *)

(* Use OpamPackage's parser so package names with dots
   (e.g. [base-bigarray.base], [dune-build-info]) and versions with dots
   (e.g. [5.4.0.20250914.2]) split correctly. Falls back to [s] as a
   bare name if opam can't parse it. *)
let parse_pkg s =
  match OpamPackage.of_string_opt s with
  | Some p ->
      {
        name = OpamPackage.Name.to_string (OpamPackage.name p);
        version = OpamPackage.Version.to_string (OpamPackage.version p);
      }
  | None -> { name = s; version = "" }

(* -- Fetch error classification ------------------------------------------ *)

(* OpamRepository raises [Failure msg] (via [Fmt.failwith]) with messages
   that quote curl/git stderr. Match a few common signatures so we can
   slot them into a small enum. The [Other] bucket carries the original
   message verbatim so the JSON is never lossy. *)
let substring_index ~needle haystack =
  let nl = String.length needle in
  let hl = String.length haystack in
  if nl = 0 || nl > hl then None
  else
    let rec loop i =
      if i + nl > hl then None
      else if String.sub haystack i nl = needle then Some i
      else loop (i + 1)
    in
    loop 0

let contains_substring ~needle haystack =
  substring_index ~needle haystack <> None

(* Extract a 3-digit HTTP status code (4xx or 5xx) from anywhere in a
   message. Returns the first match. Walks the string once; no regex. *)
let extract_http_status msg =
  let n = String.length msg in
  let rec loop i =
    if i + 2 >= n then None
    else
      let c0 = msg.[i] and c1 = msg.[i + 1] and c2 = msg.[i + 2] in
      let is_digit c = c >= '0' && c <= '9' in
      let word_boundary i = i = 0 || not (is_digit msg.[i - 1]) in
      let next_boundary = i + 3 = n || not (is_digit msg.[i + 3]) in
      if
        (c0 = '4' || c0 = '5')
        && is_digit c1 && is_digit c2 && word_boundary i && next_boundary
      then Some (int_of_string (String.sub msg i 3))
      else loop (i + 1)
  in
  loop 0

let declared_depexts ~env opam =
  List.fold_left
    (fun acc (pkgs, filter) ->
      if OpamFilter.eval_to_bool ~default:false env filter then
        OpamSysPkg.Set.union acc pkgs
      else acc)
    OpamSysPkg.Set.empty
    (OpamFile.OPAM.depexts opam)
  |> OpamSysPkg.Set.elements
  |> List.map OpamSysPkg.to_string

let classify_fetch_msg msg =
  let lc = String.lowercase_ascii msg in
  let has s = contains_substring ~needle:s lc in
  if has "checksum" || has "hash mismatch" then Checksum_mismatch
  else if
    has "timed out" || has "timeout"
    || has "could not resolve host"
    || has "name or service not known"
  then Network_timeout
  else if (has "git " && (has "failed" || has "error")) || has "fatal: " then
    Git_failed
  else
    match extract_http_status msg with
    | Some n -> Http_status n
    | None -> Other msg

(* -- Codec --------------------------------------------------------------- *)

(* Each sum type is encoded as an object with a tag member ("kind" /
   "type") that selects which other members are present. *)

let pkg_id_codec : pkg_id Jsont.t =
  let open Jsont in
  Object.map ~kind:"pkg_id" (fun name version : pkg_id -> { name; version })
  |> Object.mem "name" string ~enc:(fun (p : pkg_id) -> p.name)
  |> Object.mem "version" string ~enc:(fun (p : pkg_id) -> p.version)
  |> Object.finish

let method_codec =
  Jsont.enum ~kind:"method" [ ("source", Source); ("binary", Binary) ]

let dep_codec : dep Jsont.t =
  let open Jsont in
  Object.map ~kind:"dep" (fun name version hash : dep ->
      { name; version; hash })
  |> Object.mem "name" string ~enc:(fun (d : dep) -> d.name)
  |> Object.mem "version" string ~enc:(fun (d : dep) -> d.version)
  |> Object.mem "hash" string ~enc:(fun (d : dep) -> d.hash)
  |> Object.finish

let depexts_codec =
  let open Jsont in
  Object.map ~kind:"depexts" (fun declared installed missing not_found ->
      { declared; installed; missing; not_found })
  |> Object.mem "declared" (list string) ~dec_absent:[]
       ~enc:(fun d -> d.declared)
       ~enc_omit:(( = ) [])
  |> Object.mem "installed" (list string) ~dec_absent:[]
       ~enc:(fun d -> d.installed)
       ~enc_omit:(( = ) [])
  |> Object.mem "missing" (list string) ~dec_absent:[]
       ~enc:(fun d -> d.missing)
       ~enc_omit:(( = ) [])
  |> Object.mem "not_found" (list string) ~dec_absent:[]
       ~enc:(fun d -> d.not_found)
       ~enc_omit:(( = ) [])
  |> Object.finish

let source_info_codec =
  let open Jsont in
  Object.map ~kind:"source" (fun url checksums -> { url; checksums })
  |> Object.mem "url" string ~enc:(fun s -> s.url)
  |> Object.mem "checksums" (list string) ~dec_absent:[]
       ~enc:(fun s -> s.checksums)
       ~enc_omit:(( = ) [])
  |> Object.finish

let overlay_codec : overlay Jsont.t =
  let open Jsont in
  Object.map ~kind:"overlay" (fun handle version : overlay ->
      { handle; version })
  |> Object.mem "handle" string ~enc:(fun (o : overlay) -> o.handle)
  |> Object.mem "version" string ~enc:(fun (o : overlay) -> o.version)
  |> Object.finish

let log_pointer_codec =
  let open Jsont in
  Object.map ~kind:"log" (fun text_path tail -> { text_path; tail })
  |> Object.mem "text_path" string ~enc:(fun l -> l.text_path)
  |> Object.opt_mem "tail" string ~enc:(fun l -> l.tail)
  |> Object.finish

let phase_durations_codec =
  let open Jsont in
  Object.map ~kind:"phases" (fun fetch build install restore ->
      { fetch; build; install; restore })
  |> Object.opt_mem "fetch" number ~enc:(fun p -> p.fetch)
  |> Object.opt_mem "build" number ~enc:(fun p -> p.build)
  |> Object.opt_mem "install" number ~enc:(fun p -> p.install)
  |> Object.opt_mem "restore" number ~enc:(fun p -> p.restore)
  |> Object.finish

(* fetch_kind sub-codec — discriminated by "type" *)

let fetch_kind_codec =
  let open Jsont in
  let case_http =
    Object.Case.map "http_status"
      (Object.map ~kind:"http_status" (fun code -> Http_status code)
      |> Object.mem "code" int ~enc:(function Http_status n -> n | _ -> 0)
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_checksum =
    Object.Case.map "checksum_mismatch"
      (Object.map ~kind:"checksum_mismatch" Checksum_mismatch |> Object.finish)
      ~dec:Fun.id
  in
  let case_timeout =
    Object.Case.map "network_timeout"
      (Object.map ~kind:"network_timeout" Network_timeout |> Object.finish)
      ~dec:Fun.id
  in
  let case_git =
    Object.Case.map "git_failed"
      (Object.map ~kind:"git_failed" Git_failed |> Object.finish)
      ~dec:Fun.id
  in
  let case_other =
    Object.Case.map "other"
      (Object.map ~kind:"other" (fun message -> Other message)
      |> Object.mem "message" string ~enc:(function Other s -> s | _ -> "")
      |> Object.finish)
      ~dec:Fun.id
  in
  let cases =
    [
      Object.Case.make case_http;
      Object.Case.make case_checksum;
      Object.Case.make case_timeout;
      Object.Case.make case_git;
      Object.Case.make case_other;
    ]
  in
  Object.map ~kind:"fetch_kind" Fun.id
  |> Object.case_mem "type" string cases ~enc:Fun.id ~enc_case:(fun k ->
      match k with
      | Http_status _ -> Object.Case.value case_http k
      | Checksum_mismatch -> Object.Case.value case_checksum k
      | Network_timeout -> Object.Case.value case_timeout k
      | Git_failed -> Object.Case.value case_git k
      | Other _ -> Object.Case.value case_other k)
  |> Object.finish

(* Outcome — discriminated by "kind" *)

let outcome_codec =
  let open Jsont in
  let case_ok =
    Object.Case.map "ok"
      (Object.map ~kind:"ok" Ok_built |> Object.finish)
      ~dec:Fun.id
  in
  let case_cached =
    Object.Case.map "cached"
      (Object.map ~kind:"cached" Cached |> Object.finish)
      ~dec:Fun.id
  in
  let case_skipped =
    Object.Case.map "skipped"
      (Object.map ~kind:"skipped" (fun reason -> Skipped { reason })
      |> Object.mem "reason" string ~enc:(function
        | Skipped { reason } -> reason
        | _ -> "")
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_build_failed =
    Object.Case.map "build_failed"
      (Object.map ~kind:"build_failed" (fun command exit_code ->
           Build_failed { command; exit_code })
      |> Object.mem "command" string ~enc:(function
        | Build_failed { command; _ } -> command
        | _ -> "")
      |> Object.opt_mem "exit_code" int ~enc:(function
        | Build_failed { exit_code; _ } -> exit_code
        | _ -> None)
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_install_failed =
    Object.Case.map "install_failed"
      (Object.map ~kind:"install_failed" (fun command exit_code ->
           Install_failed { command; exit_code })
      |> Object.mem "command" string ~enc:(function
        | Install_failed { command; _ } -> command
        | _ -> "")
      |> Object.opt_mem "exit_code" int ~enc:(function
        | Install_failed { exit_code; _ } -> exit_code
        | _ -> None)
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_dep_failed =
    Object.Case.map "dep_failed"
      (Object.map ~kind:"dep_failed" (fun upstream -> Dep_failed { upstream })
      |> Object.mem "upstream" dep_codec ~enc:(function
        | Dep_failed { upstream } -> upstream
        | _ -> { name = ""; version = ""; hash = "" })
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_fetch_failed =
    Object.Case.map "fetch_failed"
      (Object.map ~kind:"fetch_failed" (fun url kind ->
           Fetch_failed { url; kind })
      |> Object.mem "url" string ~enc:(function
        | Fetch_failed { url; _ } -> url
        | _ -> "")
      |> Object.mem "fetch_kind" fetch_kind_codec ~enc:(function
        | Fetch_failed { kind; _ } -> kind
        | _ -> Other "")
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_depext_missing =
    Object.Case.map "depext_missing"
      (Object.map ~kind:"depext_missing" (fun missing not_found ->
           Depext_missing { missing; not_found })
      |> Object.mem "missing" (list string) ~enc:(function
        | Depext_missing { missing; _ } -> missing
        | _ -> [])
      |> Object.mem "not_found" (list string) ~enc:(function
        | Depext_missing { not_found; _ } -> not_found
        | _ -> [])
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_solve_failed =
    Object.Case.map "solve_failed"
      (Object.map ~kind:"solve_failed" (fun reason -> Solve_failed { reason })
      |> Object.mem "reason" string ~enc:(function
        | Solve_failed { reason } -> reason
        | _ -> "")
      |> Object.finish)
      ~dec:Fun.id
  in
  let cases =
    [
      Object.Case.make case_ok;
      Object.Case.make case_cached;
      Object.Case.make case_skipped;
      Object.Case.make case_build_failed;
      Object.Case.make case_install_failed;
      Object.Case.make case_dep_failed;
      Object.Case.make case_fetch_failed;
      Object.Case.make case_depext_missing;
      Object.Case.make case_solve_failed;
    ]
  in
  Object.map ~kind:"outcome" Fun.id
  |> Object.case_mem "kind" string cases ~enc:Fun.id ~enc_case:(fun o ->
      match o with
      | Ok_built -> Object.Case.value case_ok o
      | Cached -> Object.Case.value case_cached o
      | Skipped _ -> Object.Case.value case_skipped o
      | Build_failed _ -> Object.Case.value case_build_failed o
      | Install_failed _ -> Object.Case.value case_install_failed o
      | Dep_failed _ -> Object.Case.value case_dep_failed o
      | Fetch_failed _ -> Object.Case.value case_fetch_failed o
      | Depext_missing _ -> Object.Case.value case_depext_missing o
      | Solve_failed _ -> Object.Case.value case_solve_failed o)
  |> Object.finish

(* The top-level record. Optional sub-objects are encoded with
   [opt_mem]; lists with [enc_omit] when empty so the JSON stays
   readable. Bound twice: [codec] is the public name; [record_codec]
   is the internal alias used inside the [Manifest] sub-module where
   [codec] would shadow to [Manifest.codec]. *)
let codec : t Jsont.t =
  let open Jsont in
  Object.map ~kind:"build_log"
    (fun
      schema
      pkg
      layer_hash
      os_key
      method_
      started_at
      duration_s
      outcome
      deps
      depexts
      source
      log
      overlay
      phases
    ->
      {
        schema;
        pkg;
        layer_hash;
        os_key;
        method_;
        started_at;
        duration_s;
        outcome;
        deps;
        depexts;
        source;
        log;
        overlay;
        phases;
      })
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "pkg" pkg_id_codec ~enc:(fun r -> r.pkg)
  |> Object.mem "layer_hash" string ~enc:(fun r -> r.layer_hash)
  |> Object.mem "os_key" string ~enc:(fun r -> r.os_key)
  |> Object.mem "method" method_codec ~enc:(fun r -> r.method_)
  |> Object.mem "started_at" number ~enc:(fun r -> r.started_at)
  |> Object.mem "duration_s" number ~enc:(fun r -> r.duration_s)
  |> Object.mem "outcome" outcome_codec ~enc:(fun r -> r.outcome)
  |> Object.mem "deps" (list dep_codec) ~dec_absent:[]
       ~enc:(fun r -> r.deps)
       ~enc_omit:(( = ) [])
  |> Object.mem "depexts" depexts_codec ~dec_absent:empty_depexts
       ~enc:(fun r -> r.depexts)
       ~enc_omit:(fun d -> d = empty_depexts)
  |> Object.opt_mem "source" source_info_codec ~enc:(fun r -> r.source)
  |> Object.opt_mem "log" log_pointer_codec ~enc:(fun r -> r.log)
  |> Object.opt_mem "overlay" overlay_codec ~enc:(fun r -> r.overlay)
  |> Object.mem "phases" phase_durations_codec ~dec_absent:empty_phases
       ~enc:(fun r -> r.phases)
       ~enc_omit:(fun p -> p = empty_phases)
  |> Object.finish

let record_codec = codec

(* -- Storage ------------------------------------------------------------- *)

let short_hash h = String.sub h 0 (min 12 (String.length h))

let sidecar_path ~cache_root ~name ~version ~hash =
  let safe_pkg = if version = "" then name else Fmt.str "%s.%s" name version in
  cache_root / "build" / "logs"
  / Fmt.str "%s-%s.json" safe_pkg (short_hash hash)

let encode_to ~fs ~path ~ctx r =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec r with
  | Ok s -> (
      try Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) s
      with exn ->
        Log.warn (fun m ->
            m "build_log: %s %s failed: %s" ctx path (Printexc.to_string exn)))
  | Error e -> Log.warn (fun m -> m "build_log: %s %s encode: %s" ctx path e)

let write ~fs ~cache_root r =
  let path =
    sidecar_path ~cache_root ~name:r.pkg.name ~version:r.pkg.version
      ~hash:r.layer_hash
  in
  Cache.Logs.ensure ~fs ~cache_root;
  encode_to ~fs ~path ~ctx:"write_sidecar" r

let layer_log_path ~cache_root ~os_key ~hash =
  cache_root / "layers" / os_key / hash / "build_log.json"

(* Only writes when the layer dir already exists. The layer is
   committed by [D10.Layer.store]; calling [write_layer] before that
   would attach the proof to a non-existent layer (which a future
   [D10.Layer.exists] check would treat as the absence of a
   build). *)
let write_layer ~fs ~cache_root ~os_key r =
  let dir = cache_root / "layers" / os_key / r.layer_hash in
  if Sys.file_exists dir then
    let path = layer_log_path ~cache_root ~os_key ~hash:r.layer_hash in
    encode_to ~fs ~path ~ctx:"write_layer" r

(* -- Tail extraction ------------------------------------------------------ *)

let tail_of_file ~path =
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let len = in_channel_length ic in
          let buf_size = min len 64_000 in
          seek_in ic (len - buf_size);
          let s = really_input_string ic buf_size in
          let lines = String.split_on_char '\n' s in
          let tail =
            match lines with
            | [] | [ _ ] -> lines
            | _ :: rest when String.length s = buf_size && len > buf_size ->
                rest
            | _ -> lines
          in
          let n = List.length tail in
          let drop = max 0 (n - n_tail_lines) in
          let kept = tail |> List.to_seq |> Seq.drop drop |> List.of_seq in
          Some (String.concat "\n" kept))
    with _ -> None

(* -- Aggregate manifest --------------------------------------------------- *)

module Manifest = struct
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

  let empty_summary =
    {
      ok = 0;
      cached = 0;
      build_failed = 0;
      install_failed = 0;
      dep_failed = 0;
      fetch_failed = 0;
      depext_missing = 0;
      solve_failed = 0;
      skipped = 0;
    }

  let bump s = function
    | Ok_built -> { s with ok = s.ok + 1 }
    | Cached -> { s with cached = s.cached + 1 }
    | Build_failed _ -> { s with build_failed = s.build_failed + 1 }
    | Install_failed _ -> { s with install_failed = s.install_failed + 1 }
    | Dep_failed _ -> { s with dep_failed = s.dep_failed + 1 }
    | Fetch_failed _ -> { s with fetch_failed = s.fetch_failed + 1 }
    | Depext_missing _ -> { s with depext_missing = s.depext_missing + 1 }
    | Solve_failed _ -> { s with solve_failed = s.solve_failed + 1 }
    | Skipped _ -> { s with skipped = s.skipped + 1 }

  let summary_codec =
    let open Jsont in
    Object.map ~kind:"summary"
      (fun
        ok
        cached
        build_failed
        install_failed
        dep_failed
        fetch_failed
        depext_missing
        solve_failed
        skipped
      ->
        {
          ok;
          cached;
          build_failed;
          install_failed;
          dep_failed;
          fetch_failed;
          depext_missing;
          solve_failed;
          skipped;
        })
    |> Object.mem "ok" int ~enc:(fun s -> s.ok)
    |> Object.mem "cached" int ~enc:(fun s -> s.cached)
    |> Object.mem "build_failed" int ~enc:(fun s -> s.build_failed)
    |> Object.mem "install_failed" int ~enc:(fun s -> s.install_failed)
    |> Object.mem "dep_failed" int ~enc:(fun s -> s.dep_failed)
    |> Object.mem "fetch_failed" int ~enc:(fun s -> s.fetch_failed)
    |> Object.mem "depext_missing" int ~enc:(fun s -> s.depext_missing)
    |> Object.mem "solve_failed" int ~enc:(fun s -> s.solve_failed)
    |> Object.mem "skipped" int ~enc:(fun s -> s.skipped)
    |> Object.finish

  let codec =
    let open Jsont in
    Object.map ~kind:"manifest"
      (fun schema os_key exported_at n_packages summary results ->
        { schema; os_key; exported_at; n_packages; summary; results })
    |> Object.mem "schema" int ~enc:(fun m -> m.schema)
    |> Object.mem "os_key" string ~enc:(fun m -> m.os_key)
    |> Object.mem "exported_at" number ~enc:(fun m -> m.exported_at)
    |> Object.mem "n_packages" int ~enc:(fun m -> m.n_packages)
    |> Object.mem "summary" summary_codec ~enc:(fun m -> m.summary)
    |> Object.mem "results" (list codec) ~enc:(fun m -> m.results)
    |> Object.finish

  let of_records ~os_key ~exported_at results =
    let summary =
      List.fold_left (fun s r -> bump s r.outcome) empty_summary results
    in
    {
      schema = 1;
      os_key;
      exported_at;
      n_packages = List.length results;
      summary;
      results;
    }

  let try_decode ~fs ~path : t option =
    try
      let s = Eio.Path.load Eio.Path.(fs / path) in
      match
        Jsont_bytesrw.decode_string ~locs:true ~file:path record_codec s
      with
      | Ok (r : t) -> Some r
      | Error e ->
          Log.debug (fun m -> m "build_log: bad %s: %s" path e);
          None
    with exn ->
      Log.debug (fun m ->
          m "build_log: read %s: %s" path (Printexc.to_string exn));
      None

  let read_sidecars ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~cache_root : t list =
    let dir = Cache.Logs.dir ~cache_root in
    let dir_p = Eio.Path.(fs / dir) in
    match Eio.Path.read_dir dir_p with
    | exception Eio.Exn.Io _ -> []
    | entries ->
        entries
        |> List.filter (fun f -> Filename.check_suffix f ".json")
        |> List.filter_map (fun f -> try_decode ~fs ~path:(dir / f))

  (* Walk every [<cache>/layers/<os_key>/<hash>/build_log.json]. Layer
     dirs without a [build_log.json] (built by an older oi, or
     restored from the registry without a proof) are silently skipped
     — the export will fall back to the transient sidecar if any. *)
  let read_layer_logs ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~cache_root ~os_key :
      t list =
    let layers_dir = cache_root / "layers" / os_key in
    match Eio.Path.read_dir Eio.Path.(fs / layers_dir) with
    | exception Eio.Exn.Io _ -> []
    | hashes ->
        List.filter_map
          (fun hash ->
            if String.contains hash '.' then None
            else
              let path = layer_log_path ~cache_root ~os_key ~hash in
              if Eio.Path.is_file Eio.Path.(fs / path) then try_decode ~fs ~path
              else None)
          hashes

  let read_all ~fs ~cache_root ~os_key : t list =
    let from_layers = read_layer_logs ~fs ~cache_root ~os_key in
    let from_sidecars =
      read_sidecars ~fs ~cache_root
      |> List.filter (fun (r : t) -> r.os_key = os_key)
    in
    (* Layer-stored wins when the same layer_hash appears in both. The
       sidecars contribute records with no layer (failures, solve
       errors) which the layer scan can't see. *)
    let by_hash = Hashtbl.create 256 in
    List.iter
      (fun (r : t) -> Hashtbl.replace by_hash r.layer_hash r)
      from_sidecars;
    List.iter
      (fun (r : t) -> Hashtbl.replace by_hash r.layer_hash r)
      from_layers;
    Hashtbl.fold (fun _ v acc -> v :: acc) by_hash []
end
