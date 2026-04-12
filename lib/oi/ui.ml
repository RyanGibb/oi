[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

type phase = Fetch | Build | Install | Binary | Clone | Configure | Check

let phase_label = function
  | Fetch -> "fetch  "
  | Build -> "build  "
  | Install -> "install"
  | Binary -> "binary "
  | Clone -> "clone  "
  | Configure -> "config "
  | Check -> "check  "

let phase_style : phase -> Fmt.style = function
  | Fetch -> `Fg `Cyan
  | Build -> `Fg `Blue
  | Install -> `Fg `Green
  | Binary -> `Fg `Magenta
  | Clone -> `Fg `Cyan
  | Configure -> `Fg `Yellow
  | Check -> `Fg `Yellow

(* -- Display state ------------------------------------------------------- *)

type failure = { name : string; cmd : string; output : string }

(* No mutex needed: Eio fibers on a single domain don't preempt each
   other between yield points. All state updates are quick in-memory
   operations that complete before the next yield. *)
type state = {
  mutable active : (string * phase) list;
  mutable failures : failure list;
  mutable completed : int;
}

type t = { state : state; report : int * string -> unit }

let format_active active =
  match active with
  | [] -> ""
  | items ->
      let show = List.filteri (fun i _ -> i < 4) items in
      let parts =
        List.map
          (fun (name, phase) ->
            Fmt.str "%a %s"
              Fmt.(styled (phase_style phase) string)
              (phase_label phase) name)
          show
      in
      String.concat " │ " parts
      ^
      if List.length items > 4 then Fmt.str " (+%d more)" (List.length items - 4)
      else ""

let make_bar total =
  let open Progress.Line in
  let bar_style =
    Progress.Line.Bar_style.v
      ~color:(Terminal.Color.ansi `green)
      [ "█"; "▉"; "▊"; "▋"; "▌"; "▍"; "▎"; "▏"; " " ]
  in
  pair ~sep:(const "  ")
    (list
       [
         bar ~style:(`Custom bar_style) ~width:(`Fixed 30) total;
         count_to total;
         elapsed ();
       ])
    (rpad 40 string)

let run ~now ~total ~title f =
  let start_time = now () in
  let bar = make_bar total in
  let config = Progress.Config.v ~persistent:false ~hide_cursor:true () in
  let state = { active = []; failures = []; completed = 0 } in
  let result = ref None in
  Progress.with_reporter ~config bar (fun report ->
      report (0, "");
      let t = { state; report } in
      result := Some (f t));
  let elapsed = now () -. start_time in
  let mins = int_of_float (elapsed /. 60.0) in
  let secs = int_of_float (Float.rem elapsed 60.0) in
  let time_str =
    if mins > 0 then Fmt.str "%dm%02ds" mins secs else Fmt.str "%ds" secs
  in
  let n_fail = List.length state.failures in
  List.iter
    (fun f ->
      Fmt.epr "@.%a %s@,  command: %s@,@,%s@."
        Fmt.(styled `Red string)
        "✗" f.name f.cmd f.output)
    (List.rev state.failures);
  if n_fail = 0 then
    Fmt.pr "%a %d %s in %s@."
      Fmt.(styled `Green string)
      "✓" total title time_str
  else begin
    Fmt.pr "%a %d/%d %s, %d failed in %s@."
      Fmt.(styled `Yellow string)
      "!" (total - n_fail) total title n_fail time_str;
    exit 1
  end;
  match !result with Some r -> r | None -> assert false

let start_activity t name phase =
  t.state.active <- t.state.active @ [ (name, phase) ];
  t.report (0, format_active t.state.active)

let update_phase t name phase =
  t.state.active <-
    List.map
      (fun (n, p) -> if n = name then (n, phase) else (n, p))
      t.state.active;
  t.report (0, format_active t.state.active)

let finish_activity t name =
  t.state.active <- List.filter (fun (n, _) -> n <> name) t.state.active;
  t.state.completed <- t.state.completed + 1;
  t.report (1, format_active t.state.active)

let fail_activity t name ~cmd ~output =
  t.state.active <- List.filter (fun (n, _) -> n <> name) t.state.active;
  t.state.completed <- t.state.completed + 1;
  t.state.failures <- { name; cmd; output } :: t.state.failures;
  t.report (1, format_active t.state.active)

let finish _t = ()

(* -- Simple spinner ------------------------------------------------------ *)

let with_spinner msg f =
  let braille = [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ] in
  let line =
    let open Progress.Line in
    list
      [
        spinner ~frames:braille
          ~min_interval:(Some (Progress.Duration.of_int_ms 80))
          ();
        const (" " ^ msg);
      ]
  in
  let config = Progress.Config.v ~persistent:false () in
  let result = ref None in
  Progress.with_reporter ~config line (fun _report -> result := Some (f ()));
  match !result with Some r -> r | None -> assert false
