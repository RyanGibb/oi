(** Unified progress display.

    A single progress bar with an optional dimmed status zone above it for
    in-flight items. Animation is driven by an Eio daemon fiber. When stdout is
    not a TTY the bar is silently disabled. *)

type t

val run :
  ?status_lines:int ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  total:int ->
  title:string ->
  (t -> 'a) ->
  'a
(** [run ~sw ~clock ~total ~title f] opens an animated progress bar, runs [f t],
    and clears the bar on return. [status_lines] reserves rows above the bar for
    {!log} entries (default 4). *)

val with_msg : t -> string -> unit
(** Set the bar message without advancing position. Visible on the next tick or
    redraw. *)

val tick : ?msg:string -> t -> unit
(** Advance the position by one. If [msg] is given, also sets the message. *)

val log : t -> string -> unit
(** Push a line into the dimmed status zone above the bar. Older lines fall off
    once the zone is full. *)

val suspend : t -> (unit -> 'a) -> 'a
(** Clear the bar, run [f ()], then redraw. Use when emitting permanent output
    (errors, warnings) that must not be overwritten by the next redraw. *)

val finish : ?msg:string -> t -> unit
(** Render at 100% and emit a newline. Optionally replaces the message with a
    summary. *)

val position : t -> int
val total : t -> int
val title : t -> string
