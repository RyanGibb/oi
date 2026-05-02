let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.audit"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Types --------------------------------------------------------------- *)

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
  | Cached
  | Restored
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
  trigger : string;
  project : string option;
  host : string;
}

type log_pointer = { text_path : string; tail : string option }

type event = {
  schema : int;
  event_id : string;
  invocation_id : string;
  ts : float;
  os_key : string;
  layer_hash : string;
  pkg : pkg_id;
  outcome : outcome;
  duration_s : float;
  context : context;
  log : log_pointer option;
}

let n_tail_lines = 150

(* -- ULID --------------------------------------------------------------- *)

let crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

(* Use [Stdlib.Random.State] seeded from the OS clock so we don't depend on
   global-state initialisation order. ULID's spec asks for crypto-strength
   randomness in the 80-bit suffix; we settle for clock+pid mixing, which is
   plenty for dedup purposes. *)
let rand_state =
  lazy
    (let s = Random.State.make_self_init () in
     (* Mix in pid so two oi processes started in the same wall-clock
        millisecond don't collide on the random suffix. *)
     for _ = 1 to Unix.getpid () mod 64 do
       ignore (Random.State.bits s)
     done;
     s)

let ulid () =
  let st = Lazy.force rand_state in
  let b = Bytes.create 26 in
  let ts_ms = Int64.of_float (Unix.gettimeofday () *. 1000.) in
  let t = ref ts_ms in
  for i = 9 downto 0 do
    Bytes.set b i crockford.[Int64.to_int (Int64.logand !t 31L)];
    t := Int64.shift_right_logical !t 5
  done;
  for i = 10 to 25 do
    Bytes.set b i crockford.[Random.State.int st 32]
  done;
  Bytes.unsafe_to_string b

let invocation_id_cell = lazy (ulid ())
let invocation_id () = Lazy.force invocation_id_cell

let default_context () =
  {
    overlay = None;
    toolchain = None;
    trigger = String.concat " " (Array.to_list Sys.argv);
    project = None;
    host = (try Unix.gethostname () with _ -> "");
  }

(* -- Fetch error classification ------------------------------------------ *)

(* Best-effort matching against curl/git stderr surfaced by OpamRepository
   as a [Failure msg]. Producers attach the resulting [fetch_kind] to the
   audit event so the manifest UI can group fetch failures by cause. *)
let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  if nl = 0 then true
  else if nl > hl then false
  else
    let rec loop i =
      if i + nl > hl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in
    loop 0

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

let classify_fetch_msg msg =
  let lc = String.lowercase_ascii msg in
  let has s = contains ~needle:s lc in
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

(* -- Codec --------------------------------------------------------------- *)

let pkg_id_codec : pkg_id Jsont.t =
  let open Jsont in
  Object.map ~kind:"pkg_id" (fun name version : pkg_id -> { name; version })
  |> Object.mem "name" string ~enc:(fun (p : pkg_id) -> p.name)
  |> Object.mem "version" string ~enc:(fun (p : pkg_id) -> p.version)
  |> Object.finish

let dep_codec =
  let open Jsont in
  Object.map ~kind:"dep" (fun name version hash : dep ->
      { name; version; hash })
  |> Object.mem "name" string ~enc:(fun (d : dep) -> d.name)
  |> Object.mem "version" string ~enc:(fun (d : dep) -> d.version)
  |> Object.mem "hash" string ~enc:(fun (d : dep) -> d.hash)
  |> Object.finish

let overlay_ctx_codec =
  let open Jsont in
  Object.map ~kind:"overlay" (fun handle version : overlay_ctx ->
      { handle; version })
  |> Object.mem "handle" string ~enc:(fun (o : overlay_ctx) -> o.handle)
  |> Object.mem "version" string ~enc:(fun (o : overlay_ctx) -> o.version)
  |> Object.finish

let context_codec =
  let open Jsont in
  Object.map ~kind:"context"
    (fun overlay toolchain trigger project host ->
      { overlay; toolchain; trigger; project; host })
  |> Object.opt_mem "overlay" overlay_ctx_codec ~enc:(fun c -> c.overlay)
  |> Object.opt_mem "toolchain" string ~enc:(fun c -> c.toolchain)
  |> Object.mem "trigger" string ~enc:(fun c -> c.trigger)
  |> Object.opt_mem "project" string ~enc:(fun c -> c.project)
  |> Object.mem "host" string ~enc:(fun c -> c.host)
  |> Object.finish

let log_pointer_codec =
  let open Jsont in
  Object.map ~kind:"log" (fun text_path tail : log_pointer ->
      { text_path; tail })
  |> Object.mem "text_path" string ~enc:(fun l -> l.text_path)
  |> Object.opt_mem "tail" string ~enc:(fun l -> l.tail)
  |> Object.finish

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

let outcome_codec =
  let open Jsont in
  let case_ok =
    Object.Case.map "ok" (Object.map ~kind:"ok" Ok |> Object.finish) ~dec:Fun.id
  in
  let case_cached =
    Object.Case.map "cached"
      (Object.map ~kind:"cached" Cached |> Object.finish)
      ~dec:Fun.id
  in
  let case_restored =
    Object.Case.map "restored"
      (Object.map ~kind:"restored" Restored |> Object.finish)
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
      Object.Case.make case_restored;
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
      | Ok -> Object.Case.value case_ok o
      | Cached -> Object.Case.value case_cached o
      | Restored -> Object.Case.value case_restored o
      | Skipped _ -> Object.Case.value case_skipped o
      | Build_failed _ -> Object.Case.value case_build_failed o
      | Install_failed _ -> Object.Case.value case_install_failed o
      | Dep_failed _ -> Object.Case.value case_dep_failed o
      | Fetch_failed _ -> Object.Case.value case_fetch_failed o
      | Depext_missing _ -> Object.Case.value case_depext_missing o
      | Solve_failed _ -> Object.Case.value case_solve_failed o)
  |> Object.finish

let event_codec =
  let open Jsont in
  Object.map ~kind:"audit_event"
    (fun
      schema
      event_id
      invocation_id
      ts
      os_key
      layer_hash
      pkg
      outcome
      duration_s
      context
      log
    ->
      {
        schema;
        event_id;
        invocation_id;
        ts;
        os_key;
        layer_hash;
        pkg;
        outcome;
        duration_s;
        context;
        log;
      })
  |> Object.mem "schema" int ~enc:(fun e -> e.schema)
  |> Object.mem "event_id" string ~enc:(fun e -> e.event_id)
  |> Object.mem "invocation_id" string ~enc:(fun e -> e.invocation_id)
  |> Object.mem "ts" number ~enc:(fun e -> e.ts)
  |> Object.mem "os_key" string ~enc:(fun e -> e.os_key)
  |> Object.mem "layer_hash" string ~enc:(fun e -> e.layer_hash)
  |> Object.mem "pkg" pkg_id_codec ~enc:(fun e -> e.pkg)
  |> Object.mem "outcome" outcome_codec ~enc:(fun e -> e.outcome)
  |> Object.mem "duration_s" number ~enc:(fun e -> e.duration_s)
  |> Object.mem "context" context_codec ~enc:(fun e -> e.context)
  |> Object.opt_mem "log" log_pointer_codec ~enc:(fun e -> e.log)
  |> Object.finish

(* -- Storage ------------------------------------------------------------- *)

let path ~cache_root = cache_root / "build" / "audit.jsonl"

let ensure_dir ~fs ~cache_root =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / cache_root / "build")
  with _ -> ()

(* Encode the event as a single line of JSON (no pretty-printing) plus a
   trailing newline, then append it to the audit file. POSIX guarantees
   atomic writes for sizes below [PIPE_BUF] when the file is opened with
   [O_APPEND]; a single event is well under 4 KiB, so concurrent appenders
   from multiple [oi build] processes don't need explicit locking. *)
let append ~fs ~cache_root e =
  let dst = path ~cache_root in
  match Jsont_bytesrw.encode_string event_codec e with
  | Ok line ->
      ensure_dir ~fs ~cache_root;
      let body = line ^ "\n" in
      (try
         Eio.Path.save ~append:true ~create:(`If_missing 0o644)
           Eio.Path.(fs / dst) body
       with exn ->
         Log.warn (fun m ->
             m "audit append %s: %s" dst (Printexc.to_string exn)))
  | Error msg -> Log.warn (fun m -> m "audit encode: %s" msg)

(* -- Read --------------------------------------------------------------- *)

let split_lines s =
  String.split_on_char '\n' s |> List.filter (fun l -> l <> "")

let read_all ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~cache_root ~os_key =
  let p = path ~cache_root in
  match
    try Some (Eio.Path.load Eio.Path.(fs / p)) with Eio.Exn.Io _ -> None
  with
  | None -> []
  | Some content ->
      split_lines content
      |> List.filter_map (fun line ->
          match
            Jsont_bytesrw.decode_string ~locs:false ~file:p event_codec line
          with
          | Ok e when e.os_key = os_key -> Some e
          | Ok _ -> None
          | Error msg ->
              Log.debug (fun m -> m "audit bad line: %s" msg);
              None)

(* -- Per-os export ------------------------------------------------------- *)

let per_os_path ~output_dir ~os_key = output_dir / os_key / "audit.jsonl"

let write_per_os ~fs ~output_dir ~os_key events =
  let dst = per_os_path ~output_dir ~os_key in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(fs / output_dir / os_key);
  (* Sort by event_id (ULIDs are timestamp-prefixed so this also sorts by
     time) for deterministic output across cross-host registry merges. *)
  let body =
    events
    |> List.sort (fun a b -> String.compare a.event_id b.event_id)
    |> List.filter_map (fun e ->
        match Jsont_bytesrw.encode_string event_codec e with
        | Ok line -> Some (line ^ "\n")
        | Error msg ->
            Log.debug (fun m -> m "per-os audit encode: %s" msg);
            None)
    |> String.concat ""
  in
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / dst) body
