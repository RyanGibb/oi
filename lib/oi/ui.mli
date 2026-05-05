[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Unified terminal UI for [oi run] / [oi build] / [oi build --all].

    Three layers, used together:

    - {!Theme} — OCaml-themed [Progress] line styling (camel-orange filled
      bars + spinners, dim-brown empty bar segments). Use these helpers
      whenever a new [Progress.Line.bar] / [spinner] is constructed so
      [Pipeline]'s pre-built fetch UI and [Execute]'s build UI look
      visually identical.

    - {!Preflight} — overall-progress UI that owns the single
      [Progress.Display] active during [oi run] / [oi build]. Subsystems
      ([Pipeline.fetch_remote_layers], [Execute.run]) attach their
      multi-bar lines to the same display via [add_line] / [remove_line],
      so the overall bar above stays visible across phase transitions.

    - The top-level legacy single-bar (built on [nox-tty]) — retained for
      [oi build --all]'s cross-invocation counter; not used by new code. *)

(** {1 Shared layout}

    Column budgets used across the pipeline UI. Pin these on every per-row
    line ([Pipeline.fetch_with_display], [Execute.with_default_reporter])
    and the overall {!Preflight} row so every [│bar│] column lands on the
    same screen column. *)

val row_label_width : int
(** Leading prefix on every row (overall, aggregate, per-layer,
    per-package). Picked to fit the longest [pkg.version] strings we see
    in practice without truncation. *)

val row_bar_width : int
(** Bar width inside [│…│] delimiters on every row. *)

val fit_label : int -> string -> string
(** [fit_label n s] pads [s] with spaces to width [n], or truncates with a
    trailing [".."] if [s] is longer. Use for per-row label cells so
    package names line up regardless of length. *)

(** {1 Theme} *)

module Theme : sig
  val camel : Progress.Color.t
  (** Filled-bar / active-spinner colour ([#EF7D00], OCaml.org camel orange). *)

  val dim_camel : Progress.Color.t
  (** Empty-bar colour ([#5C4632], desaturated warm brown). *)

  val bar_style : Progress.Line.Bar_style.t
  (** UTF-8 sub-cell stages with the camel / dim-camel palette. *)

  val bar :
    width:[ `Fixed of int | `Expand ] -> int -> int Progress.Line.t
  (** [bar ~width total] is a [Progress.Line.bar] with {!bar_style}. *)

  val spinner : unit -> _ Progress.Line.t
  (** Camel-orange spinner using the default braille frames. *)

  val pct_pp : total:int -> int Progress.Printer.t
  (** [(N%)] printer: no internal padding, externally lpad'd to width 6
      so column alignment holds for any 0..100 value. *)
end

(** {1 Preflight overall progress} *)

module Preflight : sig
  val with_bar :
    clock:_ Eio.Time.clock ->
    ?total_steps:int ->
    (on_phase:(string -> unit) ->
     on_text:(string -> unit) ->
     preflight_done:(unit -> unit) ->
     shared_display:(unit, unit) Progress.Display.t option ->
     'a) ->
    'a
  (** [with_bar ~clock ?total_steps f] runs [f] under the shared overall
      progress UI.

      Owns one [Progress.Display] for the whole pipeline. Renders one
      persistent overall-bar line at the top in the form
      [⠼ <phase>                    │██████░░░░░░░░░░░░░░│ 6/18 (33%)];
      inner multi-bar phases ([Pipeline.fetch_remote_layers],
      [Execute.run]) attach their aggregate + per-row lines below it
      via [shared_display] / [Progress.Display.add_line] and remove
      them on completion, so the overall bar stays on screen the
      whole way through.

      The callback receives:

      - [on_phase msg] — swap the trailing phase text and (after the first
        call) advance the overall bar by one step against [total_steps]
        (default 12).
      - [on_text msg] — swap the phase text {i without} advancing the step
        bar. Use for high-frequency progress strings (per-byte fetch
        counts) where treating each update as a phase boundary would race
        the bar to 100% in seconds.
      - [preflight_done ()] — finalise the display, freeing the screen
        for subsequent subprocess output (e.g. [dune build]).
      - [shared_display] — the live [Progress.Display] handle to thread
        through to [Pipeline.build]'s [?shared_display]. [None] on non-TTY.

      On non-TTY all hooks are no-ops; the callback still runs with the
      same plumbing so caller code stays unconditional. *)
end

(** {1 Legacy single-bar (oi build --all)} *)

type t

val run :
  ?status_lines:int ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  total:int ->
  title:string ->
  (t -> 'a) ->
  'a
(** [run ~sw ~clock ~total ~title f] opens an animated single-bar progress
    display, runs [f t], and clears the bar on return. [status_lines]
    reserves rows above the bar for {!log} entries (default 4). Used by
    [oi build --all]; new code should reach for {!Preflight} instead. *)

val with_msg : t -> string -> unit
(** Set the bar message without advancing position. Visible on the next
    tick or redraw. *)

val tick : ?msg:string -> t -> unit
(** Advance the position by one. If [msg] is given, also sets the message. *)

val log : t -> string -> unit
(** Push a line into the dimmed status zone above the bar. Older lines
    fall off once the zone is full. *)

val suspend : t -> (unit -> 'a) -> 'a
(** Clear the bar, run [f ()], then redraw. Use when emitting permanent
    output (errors, warnings) that must not be overwritten by the next
    redraw. *)

val finish : ?msg:string -> t -> unit
(** Render at 100% and emit a newline. Optionally replaces the message
    with a summary. *)
