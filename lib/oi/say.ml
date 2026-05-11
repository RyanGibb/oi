(* Width chosen so the most common labels (deps, with-deps, overlays,
   extra-repos, prefix, tools, toolchain) all align to the same column.
   Longer labels overflow gracefully — they push the value over rather
   than truncating. *)
let label_width = 12

(* Flush stdout after every visible line so [oi run]'s preflight steps
   appear as soon as they're printed rather than at the OS buffer's
   discretion. The caller often immediately enters a slow phase
   (toolchain solve, opam-file parsing) where the next output won't come
   for hundreds of ms or more — without an explicit flush, line-buffered
   stdout silently swallows the marker. *)
let flush_out () = try Stdlib.flush stdout with _ -> ()

(* Every Say function calls a "pause around emission" hook before
   writing. The default is identity (no-op); the cmdliner layer
   installs a real hook that pauses the active progress bar(s)
   while a print happens, then resumes them. Keeps lib/oi free of
   any progress-bar machinery. *)
let around_emit_hook : ((unit -> unit) -> unit) ref = ref (fun f -> f ())
let set_around_emit f = around_emit_hook := f

let interject f =
  let result = ref None in
  !around_emit_hook (fun () -> result := Some (f ()));
  match !result with Some r -> r | None -> assert false

let step fmt =
  Fmt.kstr
    (fun s ->
      interject (fun () ->
          Fmt.pr "%a %s@." Style.accent_string "▸" s;
          flush_out ()))
    fmt

let info fmt =
  Fmt.kstr
    (fun s ->
      interject (fun () ->
          Fmt.pr "  %s@." s;
          flush_out ()))
    fmt

let field label fmt =
  Fmt.kstr
    (fun v ->
      interject (fun () ->
          Fmt.pr "  %a %s@." Style.dim_string
            (Fmt.str "%-*s" label_width (label ^ ":"))
            v;
          flush_out ()))
    fmt

(* Continuation indent for [field_list] wrapping: 2 leading spaces + the
   label gutter + 1 separator space = 15 columns. Has to match [field]'s
   prefix exactly so wrapped lines line up under the first item. *)
let field_continuation_indent = 2 + label_width + 1

let field_list ?(sep = ", ") label items =
  match items with
  | [] -> ()
  | _ ->
      let term_w = Tty.Width.terminal_width () in
      let body_w = max 20 (term_w - field_continuation_indent) in
      let joined = String.concat sep items in
      let wrapped = Tty.Width.wrap body_w joined in
      let pad = String.make field_continuation_indent ' ' in
      interject (fun () ->
          (match String.split_on_char '\n' wrapped with
          | [] -> ()
          | first :: rest ->
              Fmt.pr "  %a %s@." Style.dim_string
                (Fmt.str "%-*s" label_width (label ^ ":"))
                first;
              List.iter (fun line -> Fmt.pr "%s%s@." pad line) rest);
          flush_out ())

let progress msg =
  if Tty.is_tty () then
    interject (fun () -> Fmt.pr "\r\027[K%a%!" Style.dim_string msg)

let progress_clear () =
  if Tty.is_tty () then interject (fun () -> Fmt.pr "\r\027[K%!")

let header fmt =
  Fmt.kstr
    (fun s ->
      interject (fun () ->
          Fmt.pr "%a@." Style.header_string s;
          flush_out ()))
    fmt

let ok fmt =
  Fmt.kstr
    (fun s ->
      interject (fun () ->
          Fmt.pr "  %a %s@." Style.ok_string "✓" s;
          flush_out ()))
    fmt

let warn fmt =
  Fmt.kstr
    (fun s ->
      interject (fun () ->
          Fmt.epr "%a %s@." Style.warn_string "warning:" s;
          try Stdlib.flush stderr with _ -> ()))
    fmt

let error fmt =
  Fmt.kstr
    (fun s ->
      interject (fun () ->
          Fmt.epr "%a %s@." Style.error_string "error:" s;
          try Stdlib.flush stderr with _ -> ()))
    fmt

let newline () =
  interject (fun () ->
      Fmt.pr "@.";
      flush_out ())
