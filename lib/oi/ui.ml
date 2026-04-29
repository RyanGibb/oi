(* Unified progress display built on nox-tty.

   Single bar with optional scrolling status zone for in-flight items.
   Animation runs in a daemon fiber on the caller's switch. When stdout
   is not a TTY the bar is silently disabled and tick/log/suspend
   degrade to no-ops. *)

type t = { bar : Tty.Progress.t; total : int; title : string }

let run ?(status_lines = 4) ~sw ~clock ~total ~title f =
  let bar =
    Tty.Progress.v ~enabled:(Tty.is_tty ()) ~status_lines ~total title
  in
  Tty_eio.Progress.animate ~sw ~clock bar;
  let t = { bar; total; title } in
  let r =
    Fun.protect ~finally:(fun () -> Tty.Progress.clear bar) (fun () -> f t)
  in
  r

let with_msg t msg = Tty.Progress.message t.bar msg

let tick ?msg t =
  (match msg with Some m -> Tty.Progress.message t.bar m | None -> ());
  Tty.Progress.tick t.bar

let log t msg = Tty.Progress.log t.bar msg
let suspend _ f = Tty.Progress.suspend f

let finish ?msg t =
  match msg with
  | Some m -> Tty.Progress.finish ~message:m t.bar
  | None -> Tty.Progress.finish t.bar

let position t = Tty.Progress.position t.bar
let total t = t.total
let title t = t.title
