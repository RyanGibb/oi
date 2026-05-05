(* Project-wide terminal style palette built on nox-tty.

   One place to tweak colours. Call sites use semantic names
   ([Style.error], [Style.header]) rather than hard-coded ANSI roles
   so the meaning is obvious from the diff. *)

open Tty

let error = Style.(bold + fg Color.red)
let warn = Style.(fg Color.yellow)
let ok = Style.(fg Color.green)
let info = Style.(fg Color.cyan)
let header = Style.bold
let dim = Style.faint
let accent = Style.(bold + fg Color.cyan)
let strong_ok = Style.(bold + fg Color.green)
let source = Style.(fg Color.blue)

(* OCaml.org camel orange — used by progress spinners / bars across the
   pipeline so cache preload, registry fetch, and build all look like one
   tool. See {!Ui.Theme.camel} for the matching Progress library colour. *)
let camel = Style.(bold + fg (Color.hex "#EF7D00"))
let phase_fetch = info
let phase_build = source
let phase_install = ok
let phase_binary = Style.(fg Color.magenta)
let phase_clone = info
let phase_configure = warn
let phase_check = warn
let error_string ppf s = Style.styled error Fmt.string ppf s
let warn_string ppf s = Style.styled warn Fmt.string ppf s
let ok_string ppf s = Style.styled ok Fmt.string ppf s
let info_string ppf s = Style.styled info Fmt.string ppf s
let header_string ppf s = Style.styled header Fmt.string ppf s
let dim_string ppf s = Style.styled dim Fmt.string ppf s
let accent_string ppf s = Style.styled accent Fmt.string ppf s
let strong_ok_string ppf s = Style.styled strong_ok Fmt.string ppf s
let source_string ppf s = Style.styled source Fmt.string ppf s
let camel_string ppf s = Style.styled camel Fmt.string ppf s
let pp s pp = Style.styled s pp
