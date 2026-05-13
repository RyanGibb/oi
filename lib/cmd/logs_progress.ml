(* Single-slot active-display registry. Guarded by [mutex]: the bar
   is updated from one Eio fiber and Logs.report can fire from any
   fiber, so we serialise the pause/resume across both producers. *)

type display = (unit, unit) Progress.Display.t

let active : display option ref = ref None
let mutex = Mutex.create ()
let with_lock f = Mutex.protect mutex f
let set_active d = with_lock (fun () -> active := Some d)
let clear_active () = with_lock (fun () -> active := None)

(* Make sure the active display is finalised even when callers
   reach for [Stdlib.exit] mid-flow. [Stdlib.exit] runs at-exit
   handlers but doesn't unwind [Fun.protect] cleanups, so without
   this hook the bar stays on screen / leaves the cursor hidden
   when an early-exit code path fires. *)
let () =
  at_exit (fun () ->
      match !active with
      | None -> ()
      | Some d -> (
          active := None;
          try Progress.Display.finalise d with Sys_error _ | Failure _ -> ()))

let pause_display d =
  try Progress.Display.pause d with Sys_error _ | Failure _ -> ()

let resume_display d =
  try Progress.Display.resume d with Sys_error _ | Failure _ -> ()

let with_paused_display d f =
  pause_display d;
  Fun.protect f ~finally:(fun () -> resume_display d)

let interject f =
  match !active with
  | None -> f ()
  | Some d -> with_lock (fun () -> with_paused_display d f)

let wrap_reporter (r : Logs.reporter) : Logs.reporter =
  let report src level ~over k msgf =
    r.Logs.report src level ~over k (fun construction ->
        interject (fun () -> msgf construction))
  in
  { Logs.report }

let start_display_compact ~ppf:_ ~config =
  Progress.Display.start ~config Progress.Multi.blank
