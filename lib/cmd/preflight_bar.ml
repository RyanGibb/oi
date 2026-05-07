[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Unified terminal UI for [oi run] / [oi build] / [oi build --all].
   See [ui.mli] for the public-facing structure. *)

(* -- Shared layout ------------------------------------------------------- *)

(* Column budgets shared across {!Theme}, {!Preflight}, and the per-row
   layouts in [Pipeline.fetch_with_display] / [Execute.with_default_reporter].
   Keeping them in one place means changing the visual width of the
   pipeline UI is a single edit.

   - [row_label_width]: leading prefix on every row (overall, aggregate,
     per-layer, per-package). 32 fits the longest [pkg.version] strings
     we see in practice ([expect_test_helpers_core.v0.17.0] = 32) without
     truncation.
   - [row_bar_width]: bar width inside [│…│] delimiters. 24 cells gives
     enough resolution that even a 4 MiB layer's bar advances visibly. *)

let row_label_width = 40
let row_bar_width = 24

(* Fit [s] into [n] columns: pad with spaces if shorter, truncate with
   a trailing ".." if longer. Used by per-row prefixes so package names
   line up regardless of length. *)
let fit_label n s =
  let len = String.length s in
  if len = n then s
  else if len < n then s ^ String.make (n - len) ' '
  else if n <= 2 then String.sub s 0 n
  else String.sub s 0 (n - 2) ^ ".."

(* -- Theme --------------------------------------------------------------- *)

module Theme = struct
  let camel = Progress.Color.hex "#EF7D00"
  let dim_camel = Progress.Color.hex "#5C4632"

  let bar_style =
    let open Progress.Line.Bar_style in
    utf8 |> with_color camel |> with_empty_color dim_camel

  let bar ~width total =
    Progress.Line.bar ~style:(`Custom bar_style) ~width total

  let spinner () = Progress.Line.spinner ~color:camel ()

  let pct_pp ~(total : int) : int Progress.Printer.t =
    (* [(/)] is shadowed at the top of [pipeline.ml] / [execute.ml] as
       [Filename.concat], so percentage math there has to reach for
       [Stdlib.(/)]. Defined here with no shadow, so callers in those
       files can just use [Theme.pct_pp ~total]. *)
    let render (n : int) : string =
      let pct : int =
        if total <= 0 then 0
        else
          let p = n * 100 / total in
          if p < 0 then 0 else if p > 100 then 100 else p
      in
      let body = Printf.sprintf "(%d%%)" pct in
      let pad = max 0 (6 - String.length body) in
      String.make pad ' ' ^ body
    in
    Progress.Printer.create ~string_len:6 ~to_string:render ()
end

(* -- Preflight overall progress bar ------------------------------------- *)

module Preflight = struct
  (* See [Theme.bar] above for the camel-orange filled / dim-camel
     empty palette. The 32-column prefix matches the per-row prefix
     [Pipeline.fetch_with_display] / [Execute.with_default_reporter]
     use, so every [│bar│] column lines up vertically.

     [Progress.Line.string] intentionally writes [width - 1] cells
     (see the [XXX: why is -1 necessary?] note in Progress's source),
     so [rpad N string] only renders [N - 1] visible chars. We bump
     the [rpad] target by one to compensate, landing the overall row
     on the same column as a per-row [pkg fit_str 32] prefix. *)
  let phase_col_width = row_label_width - 2 + 1

  let overall_line ~total =
    let open Progress.Line in
    let spin = Theme.spinner () in
    let phase_seg = using (fun (_, s) -> s) (rpad phase_col_width string) in
    let bar_seg =
      using (fun (i, _) -> i) (Theme.bar ~width:(`Fixed row_bar_width) total)
    in
    let count_seg = using (fun (i, _) -> i) (count_to total) in
    let pct_seg =
      using (fun (i, _) -> i) (sum ~pp:(Theme.pct_pp ~total) ~width:6 ())
    in
    list ~sep:(const " ") [ spin; phase_seg; bar_seg; count_seg; pct_seg ]

  let with_bar ~clock ?total_steps f =
    let enabled = Tty.is_tty () in
    let stopped = ref false in
    if not enabled then begin
      let on_phase _ = () in
      let on_text _ = () in
      let preflight_done () = stopped := true in
      Fun.protect
        ~finally:(fun () -> stopped := true)
        (fun () -> f ~on_phase ~on_text ~preflight_done ~shared_display:None)
    end
    else
      let total = match total_steps with Some n when n > 0 -> n | _ -> 12 in
      Eio.Switch.run @@ fun sw ->
      let cfg =
        Progress.Config.v ~ppf:Format.err_formatter ~persistent:false ()
      in
      (* Open with [Multi.blank] (no built-in reporters) and attach the
         overall bar via [add_line]. This keeps the [Display.t] typed
         as [(unit, unit) Display.t] regardless of what's added later,
         so subsystems ([Pipeline.fetch_remote_layers], [Execute.run])
         can receive the same display via [?shared_display:(unit, unit)
         Display.t] without OCaml's inference unifying the type
         parameter to whatever the OWNED-display path uses. *)
      let display : (unit, unit) Progress.Display.t =
        Logs_progress.start_display_compact ~ppf:Format.err_formatter
          ~config:cfg
      in
      Logs_progress.set_active display;
      let overall_h = Progress.Display.add_line display (overall_line ~total) in
      let msg = ref "Preparing" in
      let stepped = ref false in
      let push ~advance =
        let delta = if advance then 1 else 0 in
        try Progress.Reporter.report overall_h (delta, !msg) with _ -> ()
      in
      push ~advance:false;
      Eio.Fiber.fork_daemon ~sw (fun () ->
          let rec loop () =
            Eio.Time.sleep clock 0.1;
            if !stopped then `Stop_daemon
            else begin
              (try Progress.Display.tick display with _ -> ());
              loop ()
            end
          in
          loop ());
      let on_phase m =
        if not !stopped then begin
          msg := m;
          push ~advance:!stepped;
          stepped := true
        end
      in
      let on_text m =
        if not !stopped then begin
          msg := m;
          push ~advance:false
        end
      in
      let preflight_done () =
        if not !stopped then begin
          stopped := true;
          Logs_progress.clear_active ();
          try Progress.Display.finalise display with _ -> ()
        end
      in
      Fun.protect
        ~finally:(fun () ->
          if not !stopped then begin
            stopped := true;
            Logs_progress.clear_active ();
            try Progress.Display.finalise display with _ -> ()
          end)
        (fun () ->
          f ~on_phase ~on_text ~preflight_done ~shared_display:(Some display))
end

(* -- Legacy single-bar UI (oi build --all) ------------------------------ *)

(* The single-bar [Tty.Progress] interface, retained for [oi build --all]'s
   cross-invocation counter. New code should reach for {!Preflight} or its
   [shared_display] handle instead. *)

type t = Tty.Progress.t

let run ?(status_lines = 4) ~sw ~clock ~total ~title f =
  let bar =
    Tty.Progress.v ~enabled:(Tty.is_tty ()) ~status_lines ~total title
  in
  Tty_eio.Progress.animate ~sw ~clock bar;
  (* On exit:
       1. [Tty.Progress.finish] renders the bar one last time at
          100% and sets [finished := true] so the animate fiber's
          next tick becomes a no-op. Without this the daemon could
          redraw the bar back on top of whatever we print next.
       2. We then erase that final 100% line — Tty.Progress is
          designed to leave a "done" footprint visible, but oi's
          callers always print their own summary block immediately
          after, so the persistent bar is just clutter above the
          summary.

     The escape is: cursor up one line, erase that line, carriage
     return. ASCII fallback: do nothing on non-tty (the bar wasn't
     rendered anyway). *)
  Fun.protect
    ~finally:(fun () ->
      Tty.Progress.finish bar;
      if Tty.is_tty () then begin
        Format.eprintf "\x1b[1A\x1b[2K\r%!"
      end)
    (fun () -> f bar)

let with_msg = Tty.Progress.message

let tick ?msg t =
  (match msg with Some m -> Tty.Progress.message t m | None -> ());
  Tty.Progress.tick t

let log = Tty.Progress.log

(* Suspend BOTH bar systems: nox-tty's [Tty.Progress] (the legacy
   [oi build --all] bar) and the [progress] library's [Display]
   (the [Pipeline.fetch] / [D10ir_bar] multi-line UI). [oi build --all]
   only uses the former, but a caller that overlaps both shouldn't
   need to know which is active. *)
let suspend _ f = Tty.Progress.suspend (fun () -> Logs_progress.interject f)
