(* Project-wide terminal style palette built on nox-tty.

   One place to tweak colours. Call sites use semantic names
   ([Style.error], [Style.header]) rather than hard-coded ANSI roles
   so the meaning is obvious from the diff.

   Every emission helper consults [Fmt.style_renderer ppf] before
   writing ANSI. [Fmt_tty.setup_std_outputs] (called from
   [Terms.setup_log]) seeds [Fmt.stdout] / [Fmt.stderr] from the
   [--color] flag, [NO_COLOR], and an [isatty]/[$TERM] probe — so a
   redirect to a file or [TERM=dumb] disables colour automatically. *)

open Tty

let error = Style.(bold + fg Color.red)
let warn = Style.(fg Color.yellow)
let ok = Style.(fg Color.green)
let info = Style.(fg Color.cyan)
let header = Style.bold
let dim = Style.faint
let accent = Style.(bold + fg Color.cyan)
let strong_ok = Style.(bold + fg Color.green)

(* True when [ppf] is configured for ANSI output. Reads the per-formatter
   renderer set by [Fmt_tty.setup_std_outputs]; an unconfigured formatter
   (e.g. one wrapping a Buffer) returns [false] and gets plain text. *)
let color_enabled ppf =
  match Fmt.style_renderer ppf with `Ansi_tty -> true | `None -> false

let styled style pp ppf v =
  if color_enabled ppf then Style.styled style pp ppf v else pp ppf v

let error_string ppf s = styled error Fmt.string ppf s
let warn_string ppf s = styled warn Fmt.string ppf s
let ok_string ppf s = styled ok Fmt.string ppf s
let info_string ppf s = styled info Fmt.string ppf s
let header_string ppf s = styled header Fmt.string ppf s
let dim_string ppf s = styled dim Fmt.string ppf s
let accent_string ppf s = styled accent Fmt.string ppf s
let strong_ok_string ppf s = styled strong_ok Fmt.string ppf s
let pp s pp = styled s pp

(* Strip SGR ANSI escape sequences ([\027[...m]) from [s]. Used as a
   post-pass for output that goes through libraries (e.g. nox-tty's
   table renderer) which emit ANSI unconditionally; we render to a
   buffer first, strip, then write through the real ppf. *)
let strip_ansi s =
  let n = String.length s in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if !i + 1 < n && s.[!i] = '\027' && s.[!i + 1] = '[' then begin
      i := !i + 2;
      while !i < n && s.[!i] <> 'm' do
        incr i
      done;
      if !i < n then incr i (* skip the terminating 'm' *)
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let with_pre_render ~render ppf =
  if color_enabled ppf then render ppf
  else
    let buf = Buffer.create 256 in
    let bppf = Format.formatter_of_buffer buf in
    render bppf;
    Format.pp_print_flush bppf ();
    Fmt.string ppf (strip_ansi (Buffer.contents buf))

let pp_table ppf table =
  with_pre_render ~render:(fun ppf -> Tty.Table.pp ppf table) ppf
