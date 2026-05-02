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

let step fmt =
  Fmt.kstr
    (fun s ->
      Fmt.pr "%a %s@." Style.accent_string "▸" s;
      flush_out ())
    fmt

let info fmt =
  Fmt.kstr
    (fun s ->
      Fmt.pr "  %s@." s;
      flush_out ())
    fmt

let field label fmt =
  Fmt.kstr
    (fun v ->
      Fmt.pr "  %a %s@." Style.dim_string
        (Fmt.str "%-*s" label_width (label ^ ":"))
        v;
      flush_out ())
    fmt

let header fmt =
  Fmt.kstr
    (fun s ->
      Fmt.pr "%a@." Style.header_string s;
      flush_out ())
    fmt

let ok fmt =
  Fmt.kstr
    (fun s ->
      Fmt.pr "  %a %s@." Style.ok_string "✓" s;
      flush_out ())
    fmt

let warn fmt =
  Fmt.kstr
    (fun s ->
      Fmt.epr "%a %s@." Style.warn_string "warning:" s;
      try Stdlib.flush stderr with _ -> ())
    fmt

let error fmt =
  Fmt.kstr
    (fun s ->
      Fmt.epr "%a %s@." Style.error_string "error:" s;
      try Stdlib.flush stderr with _ -> ())
    fmt

let newline () =
  Fmt.pr "@.";
  flush_out ()
