(** Consistent narration primitives for CLI commands.

    Every command's "I'm doing X" output should go through one of these helpers
    so colour, indentation, and the [▸] / [✓] / [⚠] / [✗] markers stay
    consistent across the codebase. Plain [Fmt.pr] calls in command bodies tend
    to drift in style (different prefixes, ad-hoc colours, inconsistent
    capitalization); routing through [Say.*] keeps them uniform.

    Output goes to stdout for {!step}, {!info}, {!field}, {!header}, {!ok}, and
    to stderr for {!warn} and {!error}. None of these helpers interact with the
    progress bar — callers inside an {!Ui.run} body should use {!Ui.log} or
    {!Ui.suspend} instead. *)

val step : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [step "%s" msg] prints ["▸ msg"] in accent style — used for top-level action
    steps ("Solving", "Assembling prefix", "Wrote .envrc"). *)

val info : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [info "%s" msg] prints ["  msg"] in default style — secondary lines indented
    under a {!step}. *)

val field : string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [field "deps" "%s" v] prints ["  deps:    v"] with the label dim and a fixed
    alignment column for the value. *)

val header : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [header "%s" h] prints ["h"] in bold — used for section headers in
    multi-block reports ([oi show], [oi config]). *)

val ok : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [ok "%s" msg] prints ["  ✓ msg"] in success-green. *)

val warn : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [warn "%s" msg] prints ["warning: msg"] in yellow on stderr, then flushes —
    protects against the message being eaten by a subsequent progress bar
    redraw. *)

val error : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [error "%s" msg] prints ["error: msg"] in red on stderr, then flushes. *)

val newline : unit -> unit
(** Emit a blank line on stdout — used to vertically separate logical sections.
*)
