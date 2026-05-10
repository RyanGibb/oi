(** Project-wide terminal style palette.

    Semantic style names so call sites read intent rather than colour. All
    helpers honour [Fmt.style_renderer] / [NO_COLOR] / [--color]. *)

val error : Tty.Style.t
val warn : Tty.Style.t
val ok : Tty.Style.t
val info : Tty.Style.t
val header : Tty.Style.t
val dim : Tty.Style.t
val accent : Tty.Style.t

val error_string : string Fmt.t
val warn_string : string Fmt.t
val ok_string : string Fmt.t
val info_string : string Fmt.t
val header_string : string Fmt.t
val dim_string : string Fmt.t
val accent_string : string Fmt.t
val strong_ok_string : string Fmt.t
val source_string : string Fmt.t

val pp : Tty.Style.t -> 'a Fmt.t -> 'a Fmt.t
(** [pp style pp] wraps a formatter with [style]. Use for non-string formatters
    or one-off styles not in the palette. *)

val pp_table : Format.formatter -> Tty.Table.t -> unit
(** [pp_table ppf t] renders [t] through [ppf]. Honours the formatter's style
    rendering setting: when ANSI is off, escapes are stripped before output
    lands. Prefer this over {!Tty.Table.pp} at every call site. *)
