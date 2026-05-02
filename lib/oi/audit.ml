let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.audit"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Types --------------------------------------------------------------- *)

type context = {
  overlay : D10.Overlay.t option;
  toolchain : string option;
  trigger : string;
  project : string option;
  host : string;
}

type log_pointer = { text_path : string; tail : string option }
type event_target = Layer of string | Solve_key of string

let target_hash = function Layer h | Solve_key h -> h

type event = {
  schema : int;
  event_id : string;
  invocation_id : string;
  ts : float;
  os_key : string;
  target : event_target;
  pkg : Identity.t;
  outcome : Outcome.t;
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

let context_codec =
  let open Jsont in
  Object.map ~kind:"context" (fun overlay toolchain trigger project host ->
      { overlay; toolchain; trigger; project; host })
  |> Object.opt_mem "overlay" D10.Overlay.codec ~enc:(fun c -> c.overlay)
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

(* [event_target] is encoded as a tagged object so a future variant (e.g.
   a synthetic registry export key) doesn't force a schema break:
     {"kind": "layer", "hash": "..."}
     {"kind": "solve_key", "hash": "..."} *)
let event_target_codec : event_target Jsont.t =
  let open Jsont in
  let case_layer =
    Object.Case.map "layer"
      (Object.map ~kind:"layer" (fun hash -> Layer hash)
      |> Object.mem "hash" string ~enc:(function
        | Layer h -> h
        | Solve_key h -> h)
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_solve =
    Object.Case.map "solve_key"
      (Object.map ~kind:"solve_key" (fun hash -> Solve_key hash)
      |> Object.mem "hash" string ~enc:(function
        | Solve_key h -> h
        | Layer h -> h)
      |> Object.finish)
      ~dec:Fun.id
  in
  let cases = [ Object.Case.make case_layer; Object.Case.make case_solve ] in
  Object.map ~kind:"event_target" Fun.id
  |> Object.case_mem "kind" string cases ~enc:Fun.id ~enc_case:(function
    | Layer _ as t -> Object.Case.value case_layer t
    | Solve_key _ as t -> Object.Case.value case_solve t)
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
      target
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
        target;
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
  |> Object.mem "target" event_target_codec ~enc:(fun e -> e.target)
  |> Object.mem "pkg" Identity.codec ~enc:(fun e -> e.pkg)
  |> Object.mem "outcome" Outcome.codec ~enc:(fun e -> e.outcome)
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
  | Ok line -> (
      ensure_dir ~fs ~cache_root;
      let body = line ^ "\n" in
      try
        Eio.Path.save ~append:true ~create:(`If_missing 0o644)
          Eio.Path.(fs / dst)
          body
      with exn ->
        Log.warn (fun m -> m "audit append %s: %s" dst (Printexc.to_string exn))
      )
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
