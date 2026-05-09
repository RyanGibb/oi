(* Unified progress UI.

   Layout:
       ⠼ build doi2bib   [████████------]  12/86  (14%)  running 3
         ⠼ ppxlib.0.38.0       [build       ]    3.2s
         ⠼ bos.0.3.0           [build       ]    0.7s
         ⠼ x509.1.0.6          [install     ]    0.2s

   - Top: spinner + phase header + bar + absolute count + pct +
     running-count.
   - Below: up to [build_parallelism] dynamic per-running rows
     added on [Node_started], removed on [Node_built/Cached/Failed/Skipped].
     Each row has its own spinner, a roomy pkg column, the current
     d10ir phase, and elapsed seconds.

   Implementation note: every dynamic column is composed via
   [Progress.Line.using f (rpad N string)] — i.e. a single sized
   string segment that takes the latest projection of the payload.
   We deliberately avoid [Theme.bar] / [count_to] / [sum] for the
   aggregate row's count and bar, since those are delta-accumulator
   segments — feeding them an absolute "current" value on every push
   makes them sum the absolute on every report (so the count
   eventually shows e.g. [779/86] when [current] was reported 9
   times). The hand-rolled string-rendered bar / count below take
   the absolute count directly. *)

(* Per-row reporter payload: (elapsed_seconds, phase_label) *)
type row_payload = float * string

(* Aggregate-row payload, all fields pre-rendered to fixed cell &
   byte budgets so [Progress.Printer.create]'s buffer is fully
   written every frame:

     (label, sparkline, bar_pct, progress, pct)

   - [label]: phase tag + optional Status detail. Truncated/padded
     to [agg_phase_w].
   - [sparkline]: pre-rendered (per-cell coloured) sparkline of
     in-flight job counts. Exactly [agg_spark_bytes] bytes.
   - [bar_pct]: 0..100 — the bar's fill ratio.
   - [progress]: right-column text. During fetch this is bytes
     done/total; during build it's task count. Padded to
     [agg_progress_w].
   - [pct]: parenthesised pct string. Padded to [agg_pct_w]. *)
type agg_payload = string * string * int * string * string

(* ANSI true-color escapes used to dither the bar in OCaml-camel
   shades. Each escape is a fixed-width byte string so the bar's
   total byte count stays stable independent of fill ratio — that
   stability is load-bearing because [Progress.Printer.create]
   reserves a fixed slot for [string_len] bytes; a short write
   leaves trailing garbage in the slot.

   The four colours form a leading-edge gradient: bright "glow"
   tip, mid-tone, full body, dim background. The lengths are
   counted by-byte and folded into [bar_with_color_bytes]. *)
let ansi_camel_full = "\027[38;2;239;125;0m"  (* #EF7D00 OCaml camel *)
let ansi_camel_glow = "\027[38;2;255;176;71m" (* #FFB047 leading flame tip *)
let ansi_camel_mid = "\027[38;2;229;141;38m"  (* #E58D26 transition shade *)
let ansi_camel_dim = "\027[38;2;92;70;50m"    (* #5C4632 unfilled bg *)
let ansi_reset = "\027[0m"

let _ansi_byte_overhead =
  String.length ansi_camel_full
  + String.length ansi_camel_glow
  + String.length ansi_camel_mid
  + String.length ansi_camel_dim
  + String.length ansi_reset

(* [bar_with_color_bytes ~inner] is the exact byte count [render_bar]
   produces for a bar with [inner] interior cells. The two delimiter
   "│" chars are 3 bytes each, the ANSI escapes have known byte
   lengths, and every interior cell is a 3-byte UTF-8 box-drawing
   char regardless of which "shade" is chosen — so the total stays
   stable across fill ratios. *)
let bar_with_color_bytes ~inner =
  3 + _ansi_byte_overhead + (inner * 3) + 3

(* Render a fixed-width box-drawing bar in OCaml-camel shades for a
   [(current, total)] pair. We do this ourselves rather than via
   [Theme.bar] because [Theme.bar] is a delta-accumulator.

   The bar grows from the left, with the leading edge displayed as
   a 2-cell dithered gradient: a brighter "▒" tip, a "▓" mid-shade
   one cell behind it. The body is solid "█" in full camel; the
   empty trailing portion is "░" in dim camel. At [filled = inner]
   the gradient is suppressed for a clean 100% block. *)
let render_bar ~width (current : int) (total : int) =
  if width <= 2 then String.make width ' '
  else
    let inner = width - 2 in
    let filled =
      if total <= 0 then 0
      else
        let f = current * inner / max 1 total in
        if f < 0 then 0 else if f > inner then inner else f
    in
    let block = "\226\150\136" (* "█" U+2588 *) in
    let dark = "\226\150\147" (* "▓" U+2593 *) in
    let mid = "\226\150\146" (* "▒" U+2592 *) in
    let shade = "\226\150\145" (* "░" U+2591 *) in
    let n_solid, n_dark, n_mid, n_empty =
      if filled = 0 then (0, 0, 0, inner)
      else if filled = inner then (inner, 0, 0, 0)
      else if filled = 1 then (0, 0, 1, inner - 1)
      else (filled - 2, 1, 1, inner - filled)
    in
    let buf = Buffer.create (bar_with_color_bytes ~inner) in
    Buffer.add_string buf "│";
    Buffer.add_string buf ansi_camel_full;
    for _ = 1 to n_solid do Buffer.add_string buf block done;
    Buffer.add_string buf ansi_camel_mid;
    for _ = 1 to n_dark do Buffer.add_string buf dark done;
    Buffer.add_string buf ansi_camel_glow;
    for _ = 1 to n_mid do Buffer.add_string buf mid done;
    Buffer.add_string buf ansi_camel_dim;
    for _ = 1 to n_empty do Buffer.add_string buf shade done;
    Buffer.add_string buf ansi_reset;
    Buffer.add_string buf "│";
    Buffer.contents buf

let _render_pct ~total c =
  if total <= 0 then "0%"
  else
    let p = c * 100 / max 1 total in
    let p = if p < 0 then 0 else if p > 100 then 100 else p in
    Fmt.str "%d%%" p

(* OCaml.org camel orange. Used for both per-row and aggregate
   spinners so the bar reads as one consistent piece. The same
   colour is exported by [Preflight_bar.Theme.camel] for legacy
   bars; we keep a private copy here to drop the cross-module
   dependency. *)
let camel = Progress.Color.hex "#EF7D00"

(* Spinner styles. Different visuals so the eye distinguishes the
   aggregate row's "overall pulse" from each per-build row's "this
   one is alive" tick. *)
let row_spinner () =
  Progress.Line.spinner ~color:camel ()

(* The rendered string MUST have stable byte length and stable cell
   width across every report — [Progress.Printer.create] writes the
   string into a fixed-size [Line_buffer] slot, and a short write
   leaves the remaining bytes initialised to whatever was in the
   buffer previously (often zeroed but sometimes prior render
   leftover, which prints as e.g. [\wu\wuu]). We compute a precise
   fixed-width layout below and pad with spaces. *)

(* Cell widths the line layout commits to. The bar (20 cells) is
   placed AFTER a 5-cell sparkline of the recent in-flight count so
   the user sees "fetch ▂▄▇▅▃ │bar│" as one glance — phase tag,
   activity trend, and progress all on the left. The right-side
   "running N" column is gone; sparkline replaces it.

   Layout:
     spinner | phase[26] | sparkline[5] | bar[20] | count[11] | pct[6]
*)
(* Phase is shrunk to 14 cells so the sparkline can claim the
   previously-empty space between the phase tag and the bar. Status
   messages ("Resolving toolchain", "Loaded N packages") truncate
   aggressively but are short-lived, while the sparkline shows
   ~10s of recent activity in 20 cells. *)
let agg_phase_w = 14
let agg_spark_w = 20
let agg_bar_w = 20  (* 18 inner + 2 │ delimiters *)
let agg_progress_w = 14
let agg_pct_w = 6

(* Print width = sum of column widths + 4 separator spaces. *)
let agg_total_cells =
  agg_phase_w + 1 + agg_spark_w + 1 + agg_bar_w + 1 + agg_progress_w + 1
  + agg_pct_w

(* The bar uses 3-byte UTF-8 box-drawing chars in EVERY cell, plus
   a fixed bundle of ANSI true-color escapes (one per shade region
   plus a final reset). [bar_with_color_bytes] returns the exact
   byte count of the colored bar — stable across fill ratios. *)
(* Sparkline: each of [agg_spark_w] cells carries its own ANSI
   true-color prefix (whose palette is keyed off the in-flight job
   count for that history slot), with one final reset escape after
   the last cell. We zero-pad each RGB component to 3 digits so the
   ANSI prefix is a constant 19 bytes regardless of the colour
   chosen — that stability lets us pin [agg_spark_bytes] without
   conditioning on which slot maps to which level. *)
let _spark_ansi_per_cell = 19  (* "\x1b[38;2;XXX;YYY;ZZZm" *)
let _spark_char_bytes = 3      (* every block-fill char is 3 bytes UTF-8 *)
let _spark_reset_bytes = String.length ansi_reset

let agg_spark_bytes =
  (agg_spark_w * (_spark_ansi_per_cell + _spark_char_bytes))
  + _spark_reset_bytes

let agg_total_bytes =
  agg_phase_w + 1 + agg_spark_bytes + 1
  + bar_with_color_bytes ~inner:(agg_bar_w - 2) + 1
  + agg_progress_w + 1 + agg_pct_w

let render_agg (lab, sparkline, bar_pct, progress, pct : agg_payload) =
  let phase = Fmt.str "%-*s" agg_phase_w lab in
  let phase =
    if String.length phase > agg_phase_w then String.sub phase 0 agg_phase_w
    else phase
  in
  let bar = render_bar ~width:agg_bar_w bar_pct 100 in
  Fmt.str "%s %s %s %s %s" phase sparkline bar progress pct

(* Sparkline (history of in-flight task count). Every cell uses a
   3-byte UTF-8 block-fill char prefixed by its OWN ANSI true-color
   escape — the colour reflects the running-count level at that
   slot, on a cool→hot palette (idle = grey, low = blues, mid =
   greens, high = yellows/reds). This visualises load: a cluster
   of red cells means the run is parallelism-bound, a tail of grey
   means the queue is draining.

   The 9 levels run from [⠀] (braille blank, "0 jobs") through
   [▁▂▃▄▅▆▇] up to [█] (8+ jobs). Every cell's ANSI prefix is
   exactly 19 bytes (RGB components zero-padded to 3 digits) so
   the total byte count of the sparkline is stable regardless of
   what mix of levels it contains. *)
let spark_chars = [|
  "\226\160\128"; (* ⠀ U+2800 braille blank *)
  "\226\150\129"; (* ▁ *)
  "\226\150\130"; (* ▂ *)
  "\226\150\131"; (* ▃ *)
  "\226\150\132"; (* ▄ *)
  "\226\150\133"; (* ▅ *)
  "\226\150\134"; (* ▆ *)
  "\226\150\135"; (* ▇ *)
  "\226\150\136"; (* █ *)
|]

let spark_max_level = 8

(* Palette keyed by level. Desaturated cool→warm gradient: dim
   grey for idle, dusty blues for low parallelism, sage / olive for
   mid load, dusty rose / muted brick for high. Saturation is well
   below the vivid blue/green/red used previously — the sparkline
   reads as a soft activity haze rather than a flashing alarm. *)
let _ansi_rgb_padded r g b =
  Fmt.str "\027[38;2;%03d;%03d;%03dm" r g b

let spark_palette = [|
  _ansi_rgb_padded 080 080 080; (* 0  idle grey *)
  _ansi_rgb_padded 100 130 165; (* 1  dusty slate-blue *)
  _ansi_rgb_padded 110 145 165; (* 2  muted blue *)
  _ansi_rgb_padded 115 155 155; (* 3  muted teal *)
  _ansi_rgb_padded 120 160 130; (* 4  sage green *)
  _ansi_rgb_padded 150 160 110; (* 5  pale olive *)
  _ansi_rgb_padded 175 155 105; (* 6  warm tan *)
  _ansi_rgb_padded 175 130 130; (* 7  dusty rose *)
  _ansi_rgb_padded 180 110 110; (* 8+ muted brick *)
|]

let spark_level_of_count n =
  if n <= 0 then 0
  else if n >= spark_max_level then spark_max_level
  else n

let render_sparkline ~samples =
  (* Render newest-on-left: leftmost cell is the most recent sample,
     rightmost is the oldest. Visually the trail "ages out" to the
     right, so a freshly-spawned burst shows up on the left edge
     where the eye lands first. *)
  let buf = Buffer.create agg_spark_bytes in
  let n = Array.length samples in
  for i = n - 1 downto 0 do
    let count = samples.(i) in
    let lvl = spark_level_of_count count in
    Buffer.add_string buf spark_palette.(lvl);
    Buffer.add_string buf spark_chars.(lvl)
  done;
  Buffer.add_string buf ansi_reset;
  Buffer.contents buf

let blank_sparkline () =
  render_sparkline ~samples:(Array.make agg_spark_w 0)

(* Use [of_printer] with an [~init] value so the FIRST render
   (triggered by [add_line]'s built-in initial-render) shows real
   content, not the empty default of a [string] segment. *)
let agg_line ~target =
  let open Progress.Line in
  let printer =
    Progress.Printer.create ~to_string:render_agg
      ~string_len:agg_total_bytes ~width:agg_total_cells ()
  in
  let agg_spinner =
    spinner ~color:camel
      ~frames:[ "→"; "↘"; "↓"; "↙"; "←"; "↖"; "↑"; "↗" ]
      ()
  in
  (* Initial label = the user's target so the very first paint says
     "doi2bib" rather than a generic "preparing". Once a phase
     starts firing it overwrites this. *)
  let blank_progress = String.make agg_progress_w ' ' in
  let blank_pct = String.make agg_pct_w ' ' in
  list ~sep:(const " ")
    [ agg_spinner;
      of_printer
        ~init:(target, blank_sparkline (), 0, blank_progress, blank_pct)
        printer ]

(* Truncate-or-pad to a precise cell width. Long names get a [..]
   marker in the middle so both the package name and version tail
   stay legible. *)
let fit_label ~width s =
  let n = String.length s in
  if n = width then s
  else if n < width then Fmt.str "%-*s" width s
  else if width < 5 then String.sub s 0 width
  else
    let head = (width - 2) / 2 in
    let tail = width - 2 - head in
    Fmt.str "%s..%s"
      (String.sub s 0 head)
      (String.sub s (n - tail) tail)

(* Per-running-build row line. Layout matches the per-fetch row
   (column-for-column) so build and fetch rows nest beneath the
   aggregate with identical alignment:

     "  " | sep | spinner | sep | <pkg>[29] | sep | bar[20] | sep | status[33]

   The bar fills as the package walks through the 8 d10ir build
   phases (Stage_deps … Store_layer). [status] is rendered tightly
   as ["[phase] elapsed.s"] without the wide bracket padding that
   read as wasted whitespace in the older [%-44s [%-12s] %7.1fs]
   layout. *)

let row_pkg_w = 32
let row_bar_w = 20
let row_status_w = 21
let row_cells = row_pkg_w + 1 + row_bar_w + 1 + row_status_w
let row_bytes =
  row_pkg_w + 1
  + bar_with_color_bytes ~inner:(row_bar_w - 2)
  + 1 + row_status_w

(* Map a d10ir-phase short tag to a 1..8 progress index so
   [render_bar] can fill the row's bar as the package advances. *)
let phase_idx_of_string = function
  | "stage-deps" -> 1
  | "unpack" -> 2
  | "subst" -> 3
  | "snapshot" -> 4
  | "build" -> 5
  | "install" -> 6
  | "diff" -> 7
  | "store" -> 8
  | _ -> 0
let phase_total = 8

let render_row_payload pkg ((elapsed, phase) : row_payload) =
  let pkg_cell = fit_label ~width:row_pkg_w pkg in
  let bar =
    let idx = phase_idx_of_string phase in
    render_bar ~width:row_bar_w idx phase_total
  in
  let status_raw = Fmt.str "[%s] %.1fs" phase elapsed in
  let status =
    if String.length status_raw > row_status_w then
      String.sub status_raw 0 row_status_w
    else
      Fmt.str "%-*s" row_status_w status_raw
  in
  let s = Fmt.str "%s %s %s" pkg_cell bar status in
  let n = String.length s in
  if n = row_bytes then s
  else if n > row_bytes then String.sub s 0 row_bytes
  else s ^ String.make (row_bytes - n) ' '

let row_line ~pkg =
  let open Progress.Line in
  let printer =
    Progress.Printer.create
      ~to_string:(render_row_payload pkg)
      ~string_len:row_bytes ~width:row_cells ()
  in
  list ~sep:(const " ")
    [ const "  "; row_spinner (); of_printer ~init:(0., "stage-deps") printer ]

(* Per-running-fetch row payload: (bytes_done, bytes_total).
   [bytes_total = 0L] means the size is unknown (typical for source
   archives prewarmed via OpamHash without size metadata) — the row
   then renders just the running byte total without a bar. *)
type fetch_row_payload = int64 * int64

let format_bytes_short (n : int64) =
  let f = Int64.to_float n in
  if f < 1024. then Fmt.str "%.0fB" f
  else if f < 1024. *. 1024. then Fmt.str "%.1fKB" (f /. 1024.)
  else if f < 1024. *. 1024. *. 1024. then
    Fmt.str "%.1fMB" (f /. 1024. /. 1024.)
  else Fmt.str "%.2fGB" (f /. 1024. /. 1024. /. 1024.)

(* Scale int64 byte count to int for [render_bar]'s width-and-total
   model. Avoids overflow on >2GB fetches by mapping to a fixed
   1000-resolution fraction. *)
let scale_int64 (b : int64) (t : int64) =
  let scale = 1000 in
  if Int64.compare t 0L <= 0 then (0, 0)
  else
    let bf = Int64.to_float b in
    let tf = Int64.to_float t in
    let r = bf *. float_of_int scale /. tf in
    let r = int_of_float r in
    let r = if r < 0 then 0 else if r > scale then scale else r in
    (r, scale)

(* Per-fetch row layout. Same "  " indent + spinner prefix as the
   per-build row so all sub-status rows visually nest beneath the
   aggregate row. Column widths are tuned so the bar's left and
   right [│] delimiters land on exactly the same columns as the
   aggregate row's bar (cols 36 and 55), and the right edge of the
   row matches the aggregate's right edge (col 89):

     "  " | sep | spinner | sep | <pkg>[29] | sep | bar[20] | sep | bytes[33]

   The bar carries ANSI true-color shades, so the line's byte
   length exceeds its cell width. We use [Progress.Printer.create]
   directly (rather than [rpad N string]) so [string_len] /
   [width] can be declared explicitly; the default [string] segment
   caps at the cell-width budget in bytes, which would truncate the
   colored bar mid-ANSI escape. *)
(* pkg_w = 32 (not 31) so the per-row bar's left [│] sits on
   exactly the same column as the agg bar's left [│]. The previous
   off-by-one came from miscounting the prefix: the row prefix
   from [list ~sep:(const " ") [const "  "; row_spinner; …]] is
   "  " + sep + spinner + sep = 5 cells, plus pkg + sep = pkg + 1,
   which must equal the agg bar's offset of 39 cells from the
   start. 5 + 32 + 1 + 1 = 39. *)
let fetch_row_pkg_w = 32
let fetch_row_bar_w = 20
let fetch_row_bytes_w = 21
let fetch_row_cells =
  fetch_row_pkg_w + 1 + fetch_row_bar_w + 1 + fetch_row_bytes_w

let fetch_row_bytes =
  fetch_row_pkg_w + 1
  + bar_with_color_bytes ~inner:(fetch_row_bar_w - 2)
  + 1 + fetch_row_bytes_w

let render_fetch_row_payload pkg ((b, t) : fetch_row_payload) =
  let pkg_cell = fit_label ~width:fetch_row_pkg_w pkg in
  let cur, total = scale_int64 b t in
  let bar = render_bar ~width:fetch_row_bar_w cur total in
  let bytes_str =
    if Int64.compare t 0L <= 0 then
      Fmt.str "%-*s" fetch_row_bytes_w (format_bytes_short b)
    else
      let raw =
        Fmt.str "%6s/%-6s" (format_bytes_short b) (format_bytes_short t)
      in
      (* Cap to exactly [fetch_row_bytes_w] cells so the buffer
         length stays stable. *)
      if String.length raw > fetch_row_bytes_w then
        String.sub raw 0 fetch_row_bytes_w
      else
        Fmt.str "%-*s" fetch_row_bytes_w raw
  in
  let s = Fmt.str "%s %s %s" pkg_cell bar bytes_str in
  (* Stable byte length: pad/truncate to [fetch_row_bytes] so
     [Progress.Printer.create]'s buffer is fully written. *)
  let n = String.length s in
  if n = fetch_row_bytes then s
  else if n > fetch_row_bytes then String.sub s 0 fetch_row_bytes
  else s ^ String.make (fetch_row_bytes - n) ' '

let fetch_row_line ~pkg =
  let open Progress.Line in
  let printer =
    Progress.Printer.create
      ~to_string:(render_fetch_row_payload pkg)
      ~string_len:fetch_row_bytes ~width:fetch_row_cells ()
  in
  (* "  " indent matches the per-build row prefix so all sub-status
     rows nest visually beneath the aggregate row. *)
  list ~sep:(const " ")
    [ const "  "; row_spinner (); of_printer ~init:(0L, 0L) printer ]

(* -- State ------------------------------------------------------------- *)

type row = {
  started_at : float;
  mutable phase : string;
  reporter : row_payload Progress.Reporter.t;
}

type fetch_row = {
  fetch_size : int64;
  fetch_reporter : fetch_row_payload Progress.Reporter.t;
}

type state = {
  display : (unit, unit) Progress.Display.t;
  clock : [ `Clock of float ] Eio.Resource.t;
  agg : agg_payload Progress.Reporter.t;
  mutex : Mutex.t;
  rows : (string, row) Hashtbl.t;  (* keyed by layer hash *)
  fetches : (string, int64) Hashtbl.t;  (* keyed by Fetch_* key, value=bytes *)
  fetch_sizes : (string, int64) Hashtbl.t;  (* declared total per Fetch_* key *)
  fetch_rows : (string, fetch_row) Hashtbl.t;  (* keyed by Fetch_* key *)
  (* Per-phase counters for the in-progress group. The displayed
     [N/T] is the sum of these plus the [*_base] committed totals
     from any groups that already finished (committed at
     [Plan_done] for [oi build @overlay] / [oi build --all] which
     run multiple solve groups back-to-back). The bar therefore
     advances monotonically across both phase boundaries within a
     group AND group boundaries — total only ever grows. *)
  mutable solve_total : int;
  mutable solve_done : int;
  mutable fetch_total : int;
  mutable fetch_done : int;
  mutable build_total : int;
  mutable build_done : int;
  mutable solve_total_base : int;
  mutable solve_done_base : int;
  mutable fetch_total_base : int;
  mutable fetch_done_base : int;
  mutable build_total_base : int;
  mutable build_done_base : int;
  mutable running : int;  (* in-flight fetches + in-flight builds *)
  mutable phase_label : string;
  mutable phase_id : string;  (* short phase tag, used to detect transitions *)
  mutable status_msg : string;
  mutable phase_bytes_total : int64;
      (* sum of declared sizes for Fetch_starteds in this phase *)
  mutable phase_bytes_done : int64;
      (* total bytes considered transferred (live; recomputed) *)
  mutable phase_bytes_done_base : int64;
      (* bytes accumulated from finished fetches; in-flight fetches
         add their current bytes on top in [recompute_phase_bytes_done] *)
  running_hist : int array;
      (* ring buffer of recent in-flight counts. Width =
         [agg_spark_w]; element 0 is the oldest sample, last
         element is the newest. Updated by the tick fiber. *)
  target_label : string;  (* user's target, set once at with_ui time *)
}

let total_of s =
  s.solve_total_base + s.solve_total
  + s.fetch_total_base + s.fetch_total
  + s.build_total_base + s.build_total
let current_of s =
  s.solve_done_base + s.solve_done
  + s.fetch_done_base + s.fetch_done
  + s.build_done_base + s.build_done

(* Commit the in-progress group's per-phase counts into the [*_base]
   accumulators and zero the working slots. Called at the end of a
   solve group (D10ir.Plan_done) so subsequent groups in [oi build
   --all] / [oi build @overlay] don't replace the previous group's
   numbers — they ADD on top. *)
let commit_group s =
  s.solve_total_base <- s.solve_total_base + s.solve_total;
  s.solve_done_base <- s.solve_done_base + s.solve_done;
  s.fetch_total_base <- s.fetch_total_base + s.fetch_total;
  s.fetch_done_base <- s.fetch_done_base + s.fetch_done;
  s.build_total_base <- s.build_total_base + s.build_total;
  s.build_done_base <- s.build_done_base + s.build_done;
  s.solve_total <- 0;
  s.solve_done <- 0;
  s.fetch_total <- 0;
  s.fetch_done <- 0;
  s.build_total <- 0;
  s.build_done <- 0

let with_lock s f = Mutex.protect s.mutex f

(* Phase column composition. The phase short tag (solve/fetch/build)
   is at the head so the user can see the major-stage transition at
   a glance; the in-flight fetch summary or free-form [Status] is
   appended after a colon, with fetch winning over plain [Status]
   (so "fetching 3 (12.4 MB)" stays visible during the Fetching
   phase regardless of the most recent Status). The target name
   trails when set.

   Before any [Phase_started] fires the bar shows just the target
   name (e.g. "doi2bib") rather than a generic "preparing" — the
   target is always the most informative bit at zero seconds. *)
let composed_label s =
  (* The byte progress that used to surface here moved to the right
     column. The left column is now strictly: phase tag, optionally
     elaborated by a free-form Status message ("Loaded N packages",
     "Resolving toolchain", …). *)
  if s.phase_label = "" then s.target_label
  else if s.status_msg = "" then s.phase_label
  else Fmt.str "%s: %s" s.phase_label s.status_msg

(* Pad/truncate a string to exactly [w] cells (assuming ASCII). *)
let pad_exact ~w s =
  let n = String.length s in
  if n = w then s
  else if n > w then String.sub s 0 w
  else Fmt.str "%-*s" w s

let _pad_right ~w s =
  let n = String.length s in
  if n = w then s
  else if n > w then String.sub s 0 w
  else Fmt.str "%*s" w s

(* The agg bar's fill ratio in [0..100]. Combined task progress
   across the whole pipeline (fetch + build): the bar grows in
   proportion to (fetch_done + build_done) / (fetch_total +
   build_total), so the user sees one smoothly-advancing bar
   instead of separate per-phase ones. *)
let bar_pct_of s =
  let t = total_of s in
  if t <= 0 then 0
  else
    let p = current_of s * 100 / t in
    if p < 0 then 0 else if p > 100 then 100 else p

(* Right-column text: bytes during fetch (matching the per-fetch
   row totals), task counts during build. Left-aligned in the
   column so the value sits flush against the bar's right [│]
   instead of drifting to the screen edge — visually the
   right-column anchors to the bar, like the per-row [bytes]
   column does. *)
let progress_text_of s =
  let raw =
    if s.phase_id = "fetch"
       && Int64.compare s.phase_bytes_total 0L > 0
    then
      Fmt.str "%s/%s"
        (format_bytes_short s.phase_bytes_done)
        (format_bytes_short s.phase_bytes_total)
    else if total_of s > 0 then
      Fmt.str "%d/%d" (current_of s) (total_of s)
    else ""
  in
  pad_exact ~w:agg_progress_w raw

let pct_text_of s =
  let pct =
    if s.phase_id = "fetch"
       && Int64.compare s.phase_bytes_total 0L > 0
    then
      let p =
        Int64.to_float s.phase_bytes_done *. 100.
        /. Int64.to_float s.phase_bytes_total
      in
      let p = if p < 0. then 0. else if p > 100. then 100. else p in
      int_of_float p
    else
      let t = total_of s in
      if t <= 0 then 0
      else
        let p = current_of s * 100 / t in
        if p < 0 then 0 else if p > 100 then 100 else p
  in
  pad_exact ~w:agg_pct_w (Fmt.str "(%d%%)" pct)

let push_agg s =
  try
    Progress.Reporter.report s.agg
      ( pad_exact ~w:agg_phase_w (composed_label s),
        render_sparkline ~samples:s.running_hist,
        bar_pct_of s,
        progress_text_of s,
        pct_text_of s )
  with _ -> ()

let push_row r ~now phase =
  let elapsed = max 0. (now -. r.started_at) in
  try Progress.Reporter.report r.reporter (elapsed, phase) with _ -> ()

let pkg_label_of (n : D10ir.Plan.node) =
  Fmt.str "%s.%s" n.package.name n.package.version

let key_of (n : D10ir.Plan.node) =
  D10ir.Layer_hash.to_string n.layer_hash

let add_row s ~now node =
  let k = key_of node in
  if not (Hashtbl.mem s.rows k) then begin
    let pkg = pkg_label_of node in
    let r = Progress.Display.add_line s.display (row_line ~pkg) in
    let phase = D10ir.Direct.phase_to_string Stage_deps in
    let row = { started_at = now; phase; reporter = r } in
    Hashtbl.add s.rows k row;
    push_row row ~now phase;
    s.running <- s.running + 1;
    push_agg s
  end

let drop_row s n =
  let k = key_of n in
  match Hashtbl.find_opt s.rows k with
  | None -> ()
  | Some r ->
      Hashtbl.remove s.rows k;
      (try Progress.Reporter.finalise r.reporter with _ -> ());
      (try Progress.Display.remove_line s.display r.reporter with _ -> ());
      s.running <- max 0 (s.running - 1)

let current_fetch_size s key fallback =
  match Hashtbl.find_opt s.fetch_sizes key with
  | Some t when Int64.compare t 0L > 0 -> t
  | _ -> fallback

let push_fetch_row s ~key r ~bytes =
  let size = current_fetch_size s key r.fetch_size in
  try
    Progress.Reporter.report r.fetch_reporter (bytes, size)
  with _ -> ()

let add_fetch_row s ~now:_ ~key ~pkg ~size =
  if not (Hashtbl.mem s.fetch_rows key) then begin
    let label = if pkg = "" then key else pkg in
    let r =
      Progress.Display.add_line s.display (fetch_row_line ~pkg:label)
    in
    let row = { fetch_size = size; fetch_reporter = r } in
    Hashtbl.add s.fetch_rows key row;
    push_fetch_row s ~key row ~bytes:0L
  end

let drop_fetch_row s ~key =
  match Hashtbl.find_opt s.fetch_rows key with
  | None -> ()
  | Some r ->
      Hashtbl.remove s.fetch_rows key;
      (try Progress.Reporter.finalise r.fetch_reporter with _ -> ());
      try Progress.Display.remove_line s.display r.fetch_reporter
      with _ -> ()

let update_fetch_row s ~key ~bytes =
  match Hashtbl.find_opt s.fetch_rows key with
  | None -> ()
  | Some r -> push_fetch_row s ~key r ~bytes

let bump_build_done s =
  s.build_done <- s.build_done + 1;
  push_agg s

let update_phase s ~now n phase =
  let k = key_of n in
  match Hashtbl.find_opt s.rows k with
  | None -> ()
  | Some r ->
      r.phase <- phase;
      push_row r ~now phase

let tick_rows s =
  let now = Eio.Time.now s.clock in
  Hashtbl.iter (fun _ r -> push_row r ~now r.phase) s.rows

(* Shift the running-count history one slot left and append the
   current value. Drives the agg row's sparkline, sampled by the
   100ms tick fiber every ~5 ticks (~500ms) so the trail covers
   ~2.5s of recent activity. *)
let sample_running s =
  let n = Array.length s.running_hist in
  if n > 0 then begin
    for i = 0 to n - 2 do
      s.running_hist.(i) <- s.running_hist.(i + 1)
    done;
    s.running_hist.(n - 1) <- s.running
  end

(* -- Event dispatch ---------------------------------------------------- *)

let handle_build_event s (e : D10ir.Direct.event) =
  with_lock s @@ fun () ->
  let now = Eio.Time.now s.clock in
  match e with
  | Plan_started { total } ->
      (* Refine the build denominator if it wasn't seeded by an
         earlier [Plan_ready]. We do not reset [build_done] — by
         design the count carries across the fetch → build boundary
         so the user sees a single combined progress bar. *)
      if s.build_total = 0 then s.build_total <- total;
      push_agg s
  | Plan_done _ ->
      (* End of this solve group's d10ir build. Commit per-phase
         counters into the [*_base] accumulators so the bar's
         denominator stays monotonic when the next group begins
         under [oi build @overlay] / [oi build --all]. *)
      commit_group s;
      push_agg s
  | Node_queued _ -> ()
  | Node_started { node } -> add_row s ~now node
  | Node_phase { node; phase } ->
      update_phase s ~now node (D10ir.Direct.phase_to_string phase)
  | Node_cached { node } ->
      drop_row s node;
      bump_build_done s
  | Node_built { node; _ } ->
      drop_row s node;
      bump_build_done s
  | Node_failed { node; _ } ->
      drop_row s node;
      bump_build_done s
  | Node_skipped { node; _ } ->
      drop_row s node;
      bump_build_done s

(* User-friendly phase label. The [Phase_started.label] coming from
   the library carries detail that's nice to surface in the bar
   (e.g. "Solving for 3 roots"); the [phase] enum gives the
   short-tag that fits in a column. *)
let phase_short = function
  | Oi.Build_progress.Solving -> "solve"
  | Fetching -> "fetch"
  | Baking -> "bake"
  | Building -> "build"
  | Assembling -> "assemble"

let _format_bytes (n : int64) =
  let f = Int64.to_float n in
  if f < 1024. then Fmt.str "%.0f B" f
  else if f < 1024. *. 1024. then Fmt.str "%.1f KB" (f /. 1024.)
  else if f < 1024. *. 1024. *. 1024. then
    Fmt.str "%.1f MB" (f /. 1024. /. 1024.)
  else Fmt.str "%.2f GB" (f /. 1024. /. 1024. /. 1024.)

let recompute_phase_bytes_done s =
  (* Sum bytes_done from completed fetches plus current bytes from
     in-flight ones. We store completed bytes implicitly: when a
     [Fetch_finished] fires, its bytes are folded into a separate
     baseline, and we drop the in-flight entry. The simpler approach
     used here: completed bytes accumulate into [phase_bytes_done]
     directly at finish-time, and in-flight bytes are added on top
     for live updates. *)
  let in_flight =
    Hashtbl.fold (fun _ b acc -> Int64.add acc b) s.fetches 0L
  in
  s.phase_bytes_done <- Int64.add s.phase_bytes_done_base in_flight

let handle_event s (e : Oi.Build_progress.event) =
  match e with
  | Phase_started { phase; label = _ } ->
      with_lock s (fun () ->
          let new_id = phase_short phase in
          s.phase_label <- new_id;
          (* A new phase clears the per-phase status text and any
             in-flight fetch state, but does NOT reset the count
             fractions: the bar's [N/T] is the sum of fetch + build
             tasks across the whole pipeline, so the user sees one
             smooth progression instead of a denominator jump on
             every phase transition. *)
          if s.phase_id <> new_id then begin
            s.phase_id <- new_id;
            Hashtbl.clear s.fetches;
            Hashtbl.clear s.fetch_sizes;
            Hashtbl.iter
              (fun _ r ->
                (try Progress.Reporter.finalise r.fetch_reporter with _ -> ());
                try Progress.Display.remove_line s.display r.fetch_reporter
                with _ -> ())
              s.fetch_rows;
            Hashtbl.clear s.fetch_rows;
            s.phase_bytes_total <- 0L;
            s.phase_bytes_done <- 0L;
            s.phase_bytes_done_base <- 0L
          end;
          s.status_msg <- "";
          push_agg s)
  | Phase_done _ -> ()
  | Status msg ->
      with_lock s (fun () ->
          s.status_msg <- msg;
          push_agg s)
  | Aggregate { phase; current; total } ->
      with_lock s (fun () ->
          (match phase with
           | Solving ->
               s.solve_total <- total;
               s.solve_done <- current
           | Fetching ->
               s.fetch_total <- total;
               s.fetch_done <- current
           | Building ->
               s.build_total <- total;
               s.build_done <- current
           | Baking | Assembling -> ());
          push_agg s)
  | Fetch_started { key; pkg; size; _ } ->
      with_lock s (fun () ->
          let now = Eio.Time.now s.clock in
          let new_key = not (Hashtbl.mem s.fetches key) in
          Hashtbl.replace s.fetches key 0L;
          if not (Hashtbl.mem s.fetch_sizes key) then begin
            Hashtbl.replace s.fetch_sizes key size;
            if Int64.compare size 0L > 0 then
              s.phase_bytes_total <-
                Int64.add s.phase_bytes_total size
          end;
          if new_key then s.running <- s.running + 1;
          add_fetch_row s ~now ~key ~pkg ~size;
          recompute_phase_bytes_done s;
          push_agg s)
  | Fetch_progress { key; bytes; total; _ } ->
      with_lock s (fun () ->
          if Hashtbl.mem s.fetches key then begin
            Hashtbl.replace s.fetches key bytes;
            (* Refine per-fetch declared size if [total] is now known
               and we previously didn't have one. Counts toward the
               phase byte total too so the agg bar's bytes-based
               fill stays accurate. *)
            (if Int64.compare total 0L > 0 then
               match Hashtbl.find_opt s.fetch_sizes key with
               | Some t when Int64.compare t 0L > 0 -> ()
               | _ ->
                   Hashtbl.replace s.fetch_sizes key total;
                   s.phase_bytes_total <-
                     Int64.add s.phase_bytes_total total);
            update_fetch_row s ~key ~bytes;
              recompute_phase_bytes_done s;
            push_agg s
          end)
  | Fetch_finished { key; _ } ->
      with_lock s (fun () ->
          (* Fold the in-flight bytes into the baseline so subsequent
             [recompute_phase_bytes_done] still includes them after
             the entry is dropped. If the declared size is known and
             larger, prefer that as the baseline contribution. *)
          let final_bytes =
            match Hashtbl.find_opt s.fetches key with
            | Some b -> b
            | None -> 0L
          in
          let declared =
            match Hashtbl.find_opt s.fetch_sizes key with
            | Some s when Int64.compare s 0L > 0 -> s
            | _ -> final_bytes
          in
          let contribution =
            if Int64.compare declared final_bytes > 0 then declared
            else final_bytes
          in
          s.phase_bytes_done_base <-
            Int64.add s.phase_bytes_done_base contribution;
          if Hashtbl.mem s.fetches key then
            s.running <- max 0 (s.running - 1);
          Hashtbl.remove s.fetches key;
          drop_fetch_row s ~key;
          recompute_phase_bytes_done s;
          (* [fetch_done] itself is driven by the [Aggregate] event
             that fetch loops emit alongside each [Fetch_finished],
             so we don't bump it here. *)
          push_agg s)
  | Plan_ready plan ->
      with_lock s (fun () ->
          (* Seed the build denominator early so the agg bar shows
             the combined fetch + build total even before D10ir's
             [Plan_started] arrives. *)
          s.build_total <- List.length plan.nodes;
          push_agg s)
  | Build e -> handle_build_event s e
  | Build_summary _ -> ()

(* -- Lifecycle --------------------------------------------------------- *)

let with_ui ?(target = "") ~clock ~enabled f =
  if not enabled then f Oi.Build_progress.null
  else begin
    let cfg =
      Progress.Config.v ~ppf:Format.err_formatter ~persistent:false ()
    in
    let display =
      Logs_progress.start_display_compact ~ppf:Format.err_formatter
        ~config:cfg
    in
    Logs_progress.set_active display;
    Oi.Say.set_around_emit Logs_progress.interject;
    let agg = Progress.Display.add_line display (agg_line ~target) in
    let s =
      {
        display;
        clock;
        agg;
        mutex = Mutex.create ();
        rows = Hashtbl.create 16;
        fetches = Hashtbl.create 16;
        fetch_sizes = Hashtbl.create 16;
        fetch_rows = Hashtbl.create 16;
        solve_total = 0;
        solve_done = 0;
        fetch_total = 0;
        fetch_done = 0;
        build_total = 0;
        build_done = 0;
        solve_total_base = 0;
        solve_done_base = 0;
        fetch_total_base = 0;
        fetch_done_base = 0;
        build_total_base = 0;
        build_done_base = 0;
        running = 0;
        running_hist = Array.make agg_spark_w 0;
        (* Empty phase_label until the first [Phase_started] fires;
           [composed_label] then renders just the target name. *)
        phase_label = "";
        phase_id = "";
        status_msg = "";
        phase_bytes_total = 0L;
        phase_bytes_done = 0L;
        phase_bytes_done_base = 0L;
        target_label = target;
      }
    in
    push_agg s;
    let reporter : Oi.Build_progress.reporter =
      { event = (fun e -> handle_event s e) }
    in
    let cleanup () =
      with_lock s (fun () ->
          Hashtbl.iter
            (fun _ r ->
              (try Progress.Reporter.finalise r.reporter with _ -> ());
              try Progress.Display.remove_line display r.reporter with _ -> ())
            s.rows;
          Hashtbl.clear s.rows;
          Hashtbl.iter
            (fun _ r ->
              (try Progress.Reporter.finalise r.fetch_reporter with _ -> ());
              try Progress.Display.remove_line display r.fetch_reporter
              with _ -> ())
            s.fetch_rows;
          Hashtbl.clear s.fetch_rows;
          Hashtbl.clear s.fetches;
          Hashtbl.clear s.fetch_sizes;
          (try Progress.Reporter.finalise s.agg with _ -> ());
          try Progress.Display.remove_line display s.agg with _ -> ());
      Logs_progress.clear_active ();
      Oi.Say.set_around_emit (fun f -> f ());
      try Progress.Display.finalise display with _ -> ()
    in
    Fun.protect ~finally:cleanup @@ fun () ->
    let done_p, done_r = Eio.Promise.create () in
    let result = ref None in
    Eio.Fiber.both
      (fun () ->
        let tick_count = ref 0 in
        let rec loop () =
          if Eio.Promise.is_resolved done_p then ()
          else begin
            (try Eio.Time.sleep clock 0.1 with _ -> ());
            incr tick_count;
            (try
               with_lock s (fun () ->
                 tick_rows s;
                 (* Sample sparkline history every ~5 ticks (~500ms);
                    push_agg so the new sparkline frame paints. *)
                 if !tick_count mod 5 = 0 then begin
                   sample_running s;
                   push_agg s
                 end)
             with _ -> ());
            loop ()
          end
        in
        loop ())
      (fun () ->
        (try result := Some (f reporter)
         with exn ->
           Eio.Promise.resolve done_r ();
           raise exn);
        Eio.Promise.resolve done_r ());
    match !result with
    | Some r -> r
    | None -> assert false
  end
