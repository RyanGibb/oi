(** Project-wide terminal style palette.

    Semantic style names so call sites read intent rather than colour. All
    helpers honour [Fmt.style_renderer] / [NO_COLOR] / [--color]. *)

(** {1 Raw styles}

    The {!Tty.Style.t} palette. Pair with {!pp} to apply to arbitrary formatters
    or with {!Tty.Style.styled} when you need fine-grained control. *)

val error : Tty.Style.t
(** [error] is bold red — failed builds, raised errors, unrecoverable state. *)

val warn : Tty.Style.t
(** [warn] is yellow — non-fatal anomalies that the user should notice. *)

val ok : Tty.Style.t
(** [ok] is green — completed action confirmations. *)

val info : Tty.Style.t
(** [info] is cyan — incidental status that is neither success nor failure. *)

val header : Tty.Style.t
(** [header] is bold — section headers in multi-block reports. *)

val dim : Tty.Style.t
(** [dim] is faint — secondary metadata that should fade into the background. *)

val accent : Tty.Style.t
(** [accent] is bold cyan — proper nouns (commands, handles, package names)
    inside a sentence. *)

(** {1 String formatters}

    Convenience [string Fmt.t] for the common case of "stringify with this
    style". Each helper checks the formatter's {!Fmt.style_renderer} and falls
    back to plain text when ANSI is off. *)

val pp_error_string : string Fmt.t
(** [pp_error_string] renders a string in {!error} style. *)

val pp_warn_string : string Fmt.t
(** [pp_warn_string] renders a string in {!warn} style. *)

val pp_ok_string : string Fmt.t
(** [pp_ok_string] renders a string in {!ok} style. *)

val pp_info_string : string Fmt.t
(** [pp_info_string] renders a string in {!info} style. *)

val pp_header_string : string Fmt.t
(** [pp_header_string] renders a string in {!header} style. *)

val pp_dim_string : string Fmt.t
(** [pp_dim_string] renders a string in {!dim} style. *)

val pp_accent_string : string Fmt.t
(** [pp_accent_string] renders a string in {!accent} style. *)

val pp_strong_ok_string : string Fmt.t
(** [pp_strong_ok_string] renders a string in bold {!ok} (strong success). *)

val pp : Tty.Style.t -> 'a Fmt.t -> 'a Fmt.t
(** [pp style pp] wraps a formatter with [style]. Use for non-string formatters
    or one-off styles not in the palette. *)

val pp_table : Format.formatter -> Tty.Table.t -> unit
(** [pp_table ppf t] renders [t] through [ppf]. Honours the formatter's style
    rendering setting: when ANSI is off, escapes are stripped before output
    lands. Prefer this over {!Tty.Table.pp} at every call site. *)
