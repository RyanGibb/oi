(** Overall-progress UI used by [oi run].

    Wraps a single [Progress.Display] line that stays visible while [oi run]
    works through its phases (toolchain resolve → solve → fetch → build → exec).
    Subsystems attach their multi-bar lines to the same display via
    [shared_display] / [Progress.Display.add_line] / [remove_line], so the
    overall bar above stays visible across phase transitions. *)

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

      Owns one [Progress.Display] for the whole pipeline. Renders one persistent
      overall-bar line at the top in the form
      [⠼ <phase>                    │██████░░░░░░░░░░░░░░│ 6/18 (33%)]; inner
      multi-bar phases attach their aggregate + per-row lines below it via
      [shared_display] and remove them on completion, so the overall bar stays
      on screen the whole way through.

      The callback receives:

      - [on_phase msg] — swap the trailing phase text and (after the first call)
        advance the overall bar by one step against [total_steps] (default 12).
      - [on_text msg] — swap the phase text {i without} advancing the step bar.
        Use for high-frequency progress strings (per-byte fetch counts) where
        treating each update as a phase boundary would race the bar to 100% in
        seconds.
      - [preflight_done ()] — finalise the display, freeing the screen for
        subsequent subprocess output (e.g. [dune build]).
      - [shared_display] — the live [Progress.Display] handle to thread through
        to inner phases' [?shared_display]. [None] on non-TTY.

      On non-TTY all hooks are no-ops; the callback still runs with the same
      plumbing so caller code stays unconditional. *)
end
