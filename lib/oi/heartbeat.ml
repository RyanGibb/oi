(* Periodic in-flight reporter for long-running phases.

   When [oi build --all] hangs, the standard logging only emits per-event
   lines (Started / Built / Failed / ...), so a wedged fiber leaves the
   user staring at a stale spinner. A heartbeat tracker keeps a live set
   of named tasks plus their start time and periodically logs the lot,
   converting "the build hung 8 minutes ago" into "build is still
   waiting on [foo, bar (5m)] at [phase]".

   Used by the source-mirror prefetch and the build executor; the
   tracker is allocated fresh per phase and runs as an Eio daemon
   under the caller's switch. *)

let log_src = Logs.Src.create "oi.heartbeat"

module Log = (val Logs.src_log log_src : Logs.LOG)

type entry = { name : string; started : float }

type t = {
  label : string;  (** Phase tag, e.g. ["mirror-fetch"], used in log lines. *)
  tasks : (int, entry) Hashtbl.t;
  mutable next_id : int;
  mutex : Eio.Mutex.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  interval_s : float;
}

(* Default interval comes from [OI_HEARTBEAT_INTERVAL] (seconds; [0]
   disables). 30s strikes a balance between catching a hang quickly and
   not spamming a healthy long-running build. *)
let default_interval_s = 30.0

let interval_s () =
  match Sys.getenv_opt "OI_HEARTBEAT_INTERVAL" with
  | Some v -> (
      match float_of_string_opt v with
      | Some n when n >= 0.0 -> n
      | _ -> default_interval_s)
  | None -> default_interval_s

let now t = Eio.Time.now t.clock

let format_duration secs =
  if secs < 60.0 then Fmt.str "%.0fs" secs
  else if secs < 3600.0 then Fmt.str "%.1fmin" (secs /. 60.0)
  else Fmt.str "%.1fh" (secs /. 3600.0)

let snapshot t =
  Eio.Mutex.use_ro t.mutex @@ fun () ->
  Hashtbl.fold (fun _ e acc -> e :: acc) t.tasks []
  |> List.sort (fun a b -> compare a.started b.started)

let format_entries t entries =
  let now_t = now t in
  entries
  |> List.map (fun e ->
      Fmt.str "%s (%s)" e.name (format_duration (now_t -. e.started)))
  |> String.concat ", "

(* The heartbeat fiber runs as a daemon: it loops [Eio.Time.sleep] +
   [Log.warn], and exits when the parent switch closes. We use [warn]
   rather than [info] because heartbeats are intended to surface stalls
   even at the default verbosity. The log line is silent when nothing
   is in-flight. *)
let run_daemon t =
  let rec loop () =
    Eio.Time.sleep t.clock t.interval_s;
    let entries = snapshot t in
    (match entries with
    | [] -> ()
    | _ ->
        Log.warn (fun m ->
            m "[%s] %d task(s) in flight: %s" t.label (List.length entries)
              (format_entries t entries)));
    loop ()
  in
  try loop () with Eio.Cancel.Cancelled _ -> ()

let v ?(interval_s = interval_s ()) ~sw ~clock label =
  let t =
    {
      label;
      tasks = Hashtbl.create 16;
      next_id = 0;
      mutex = Eio.Mutex.create ();
      clock;
      interval_s;
    }
  in
  if interval_s > 0.0 then
    Eio.Fiber.fork_daemon ~sw (fun () ->
        run_daemon t;
        `Stop_daemon);
  t

let pp ppf t = Fmt.pf ppf "<heartbeat %s>" t.label

let track t name f =
  if t.interval_s <= 0.0 then f ()
  else begin
    let id =
      Eio.Mutex.use_rw ~protect:false t.mutex @@ fun () ->
      let id = t.next_id in
      t.next_id <- id + 1;
      Hashtbl.replace t.tasks id { name; started = now t };
      id
    in
    Fun.protect
      ~finally:(fun () ->
        Eio.Mutex.use_rw ~protect:false t.mutex @@ fun () ->
        Hashtbl.remove t.tasks id)
      f
  end

let in_flight t = snapshot t |> List.map (fun e -> e.name)
