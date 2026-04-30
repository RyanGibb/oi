(* Unified progress display built on nox-tty.

   Single bar with optional scrolling status zone for in-flight items.
   Animation runs in a daemon fiber on the caller's switch. When stdout
   is not a TTY the bar is silently disabled and tick/log/suspend
   degrade to no-ops. *)

type t = Tty.Progress.t

let run ?(status_lines = 4) ~sw ~clock ~total ~title f =
  let bar =
    Tty.Progress.v ~enabled:(Tty.is_tty ()) ~status_lines ~total title
  in
  Tty_eio.Progress.animate ~sw ~clock bar;
  Fun.protect ~finally:(fun () -> Tty.Progress.clear bar) (fun () -> f bar)

let with_msg = Tty.Progress.message

let tick ?msg t =
  (match msg with Some m -> Tty.Progress.message t m | None -> ());
  Tty.Progress.tick t

let log = Tty.Progress.log
let suspend _ f = Tty.Progress.suspend f
let finish ?msg t = Tty.Progress.finish ?message:msg t
