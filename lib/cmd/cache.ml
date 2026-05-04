(** [oi cache]: inspect the d10 content-addressed layer cache.

    Subsumes the old standalone [d10] CLI: every layer the build pipeline has
    materialised under [<cache>/layers/<os_key>/] is queryable here, plus the
    sqlite index that powers binary lookup. *)

open Cmdliner

let ( / ) = Filename.concat
let short_hash h = if String.length h > 12 then String.sub h 0 12 else h

(* Build a [D10.Config.t] from the shared harness env. The d10 view is a
   read-only handle to [<cache>/layers/<os_key>/]; nothing writes through
   it from these inspection commands except [oi cache index]. *)
let with_d10 (env : Harness.env) f =
  let d10 : D10.Config.t =
    {
      sys = env.sys;
      fs = env.fs;
      clock = (env.clock :> D10.Config.clk);
      root = Eio.Path.(env.fs / Oi.Cache.root_s env.cache);
      os_key = env.os_key;
    }
  in
  f d10

let layers_root_s d10 =
  Eio.Path.native_exn d10.D10.Config.root / "layers" / d10.os_key

let index_path_s d10 =
  Eio.Path.native_exn d10.D10.Config.root / "layers" / "index.db"

(* -- helpers for status rendering --------------------------------------- *)

let status_span = function
  | 0 -> Tty.Span.styled Oi.Style.ok "ok"
  | n -> Tty.Span.styled Oi.Style.error (Fmt.str "fail %d" n)

let pp_time t =
  let t = Unix.gmtime t in
  Fmt.str "%04d-%02d-%02d %02d:%02d UTC" (t.Unix.tm_year + 1900) (t.tm_mon + 1)
    t.tm_mday t.tm_hour t.tm_min

let count_files dir =
  if not (Sys.file_exists dir) then 0
  else
    let rec scan acc dir =
      Array.fold_left
        (fun acc name ->
          let path = dir / name in
          if Sys.is_directory path then scan acc path else acc + 1)
        acc (Sys.readdir dir)
    in
    scan 0 dir

(* -- list ---------------------------------------------------------------- *)

let list_cmd =
  let run () cache_dir =
    Harness.run @@ fun ~sw env ->
    let h = Harness.bootstrap ~sw env cache_dir in
    with_d10 h @@ fun d10 ->
    let layers_dir = layers_root_s d10 in
    Fmt.pr "%a %s@.@." Oi.Style.header_string "Layers" d10.os_key;
    if not (Sys.file_exists layers_dir) then Fmt.pr "  (empty)@."
    else begin
      let entries =
        Sys.readdir layers_dir |> Array.to_list |> List.sort String.compare
      in
      let rows, total =
        List.fold_left
          (fun (rows, total) hash ->
            let json =
              Eio.Path.(d10.root / "layers" / d10.os_key / hash / "layer.json")
            in
            match D10.Layer.load_meta json with
            | Some m ->
                let row =
                  [
                    Tty.Span.styled Oi.Style.dim (short_hash hash);
                    status_span m.exit_status;
                    Tty.Span.text m.package;
                  ]
                in
                (row :: rows, total + 1)
            | None ->
                let row =
                  [
                    Tty.Span.styled Oi.Style.dim (short_hash hash);
                    Tty.Span.styled Oi.Style.warn "no metadata";
                    Tty.Span.text "—";
                  ]
                in
                (row :: rows, total))
          ([], 0) entries
      in
      let table =
        Tty.Table.of_rows ~header_style:Oi.Style.header
          [
            Tty.Table.column "HASH";
            Tty.Table.column "STATUS";
            Tty.Table.column "PACKAGE";
          ]
          (List.rev rows)
      in
      Tty.Table.pp Fmt.stdout table;
      Fmt.pr "@.%a %d layer(s)@." Oi.Style.header_string "Total:" total
    end
  in
  let info = Cmd.info "list" ~doc:"List every cached layer for the host" in
  Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)

(* -- show ---------------------------------------------------------------- *)

(* Render the [Provenance.t] sidecar (when present) below the [layer.json]
   block. Provenance fields cover the inputs that produced the layer's
   content hash — opam origin, source URL+kind, declared depexts, ocaml
   version. Caller context (overlay handle, trigger, etc.) lives in
   {!print_callers} below, sourced from the audit log. *)
let pp_duration s =
  if s < 1.0 then Fmt.str "%.0fms" (s *. 1000.)
  else if s < 60.0 then Fmt.str "%.2fs" s
  else
    let m = int_of_float (s /. 60.0) in
    let r = s -. float_of_int (m * 60) in
    Fmt.str "%dm%.0fs" m r

let print_provenance (p : Oi.Provenance.t) =
  let o = p.opam.origin in
  let origin_label =
    match o.overlay with
    | Some ov -> Fmt.str "%a %a" Oi.Origin.pp_kind o.kind D10.Overlay.pp ov
    | None -> Fmt.str "%a" Oi.Origin.pp_kind o.kind
  in
  Fmt.pr "  method:   %s@," (Oi.Identity.method_to_string p.method_);
  Fmt.pr "  built:    %s (%s)@," (pp_time p.built_at) (pp_duration p.duration_s);
  Fmt.pr "  opam sha: %s@," p.opam.sha256;
  Fmt.pr "  origin:   %s@," origin_label;
  if o.path_in_repo <> "" then Fmt.pr "  path:     %s@," o.path_in_repo;
  (match p.source with
  | None -> ()
  | Some s -> (
      let kind = if s.kind = "" then "" else Fmt.str " (%s)" s.kind in
      Fmt.pr "  source:   %s%s@," s.url kind;
      match s.checksums with [] -> () | c :: _ -> Fmt.pr "  checksum: %s@," c));
  Fmt.pr "  ocaml:    %s@," p.build_env.ocaml_version;
  if p.depexts_declared <> [] then
    Fmt.pr "  depexts:  %s@," (String.concat ", " p.depexts_declared);
  let phases = p.phases in
  let phase_strs =
    List.filter_map
      (fun (name, v) ->
        Stdlib.Option.map (fun s -> Fmt.str "%s=%s" name (pp_duration s)) v)
      [
        ("fetch", phases.fetch);
        ("build", phases.build);
        ("install", phases.install);
        ("restore", phases.restore);
      ]
  in
  if phase_strs <> [] then
    Fmt.pr "  phases:   %s@," (String.concat ", " phase_strs)

(* Group [events] by overlay handle and render one row per distinct handle
   listing its outcome histogram and the most recent timestamp:

       @avsm     ok×1 cached×2  (last 2026-05-02 09:14 UTC)
       @samoht   restored×1     (last 2026-05-02 09:30 UTC)

   Events are pre-filtered to a single layer_hash by the caller. *)
let print_callers events =
  if events = [] then ()
  else begin
    let by_handle :
        (string, (Oi.Outcome.kind * int) list ref * float ref) Hashtbl.t =
      Hashtbl.create 4
    in
    List.iter
      (fun (e : Oi.Audit.event) ->
        let key =
          match e.context.overlay with
          | Some o -> "@" ^ o.handle
          | None -> "(no overlay)"
        in
        let outcomes_ref, ts_ref =
          match Hashtbl.find_opt by_handle key with
          | Some v -> v
          | None ->
              let v = (ref [], ref e.ts) in
              Hashtbl.replace by_handle key v;
              v
        in
        outcomes_ref :=
          Oi.Outcome.bump (Oi.Outcome.kind_of e.outcome) !outcomes_ref;
        ts_ref := Float.max !ts_ref e.ts)
      events;
    let rows =
      Hashtbl.fold
        (fun k (os_ref, ts_ref) acc -> (k, !os_ref, !ts_ref) :: acc)
        by_handle []
      |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)
    in
    Fmt.pr "  callers:@,";
    List.iter
      (fun (handle, outcomes, last_ts) ->
        let outcome_str =
          Oi.Outcome.sort_histogram outcomes
          |> List.map (fun (k, c) ->
              Fmt.str "%s×%d" (Oi.Outcome.kind_to_string k) c)
          |> String.concat " "
        in
        Fmt.pr "    %-16s %s  (last %s)@," handle outcome_str (pp_time last_ts))
      rows
  end

let show_cmd =
  let run () cache_dir package =
    Harness.run @@ fun ~sw env ->
    let h = Harness.bootstrap ~sw env cache_dir in
    with_d10 h @@ fun d10 ->
    let layers_dir = layers_root_s d10 in
    if not (Sys.file_exists layers_dir) then begin
      Fmt.pr "No layers found.@.";
      exit 0
    end;
    let cache_root = Eio.Path.native_exn d10.root in
    (* Pull the entire per-os audit slice once and bucket it by layer
       hash; cheaper than re-scanning the jsonl per matched layer.
       [Solve_key]-tagged events have no real layer to point at and are
       skipped here — they only show up in the registry manifest. *)
    let events_by_hash : (string, Oi.Audit.event list) Hashtbl.t =
      let tbl = Hashtbl.create 256 in
      let all = Oi.Audit.read_all ~fs:h.fs ~cache_root ~os_key:d10.os_key in
      List.iter
        (fun (e : Oi.Audit.event) ->
          match e.target with
          | Solve_key _ -> ()
          | Layer hash ->
              let prev =
                match Hashtbl.find_opt tbl hash with
                | Some xs -> xs
                | None -> []
              in
              Hashtbl.replace tbl hash (e :: prev))
        all;
      tbl
    in
    let events_for hash =
      match Hashtbl.find_opt events_by_hash hash with
      | Some xs -> xs
      | None -> []
    in
    let entries = Sys.readdir layers_dir |> Array.to_list in
    let matched = ref 0 in
    List.iter
      (fun hash ->
        let json =
          Eio.Path.(d10.root / "layers" / d10.os_key / hash / "layer.json")
        in
        match D10.Layer.load_meta json with
        | Some m
          when String.length m.package >= String.length package
               && String.sub m.package 0 (String.length package) = package ->
            incr matched;
            let status =
              if m.exit_status = 0 then Fmt.str "%a" Oi.Style.ok_string "ok"
              else
                Fmt.str "%a (exit %d)" Oi.Style.error_string "failed"
                  m.exit_status
            in
            let files = count_files (layers_dir / hash / "fs") in
            let prov =
              Oi.Provenance.load ~fs:h.fs ~cache_root ~os_key:d10.os_key ~hash
            in
            Fmt.pr "@[<v>%a %s@," Oi.Style.header_string "Layer" hash;
            Fmt.pr "  package:  %s@," m.package;
            Fmt.pr "  status:   %s@," status;
            Fmt.pr "  created:  %s@," (pp_time m.created);
            Fmt.pr "  deps:     %s@,"
              (if m.deps = [] then "(none)" else String.concat ", " m.deps);
            Fmt.pr "  hashes:   %s@,"
              (if m.hashes = [] then "(none)"
               else String.concat ", " (List.map short_hash m.hashes));
            Fmt.pr "  files:    %d@," files;
            (match prov with None -> () | Some p -> print_provenance p);
            print_callers (events_for hash);
            Fmt.pr "@,@]@."
        | _ -> ())
      entries;
    if !matched = 0 then Fmt.pr "No layers found matching %S@." package
  in
  let package =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PACKAGE" ~doc:"Package name (prefix match)." [])
  in
  let info = Cmd.info "show" ~doc:"Show details for a package's layers" in
  Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir $ package)

(* -- binaries ------------------------------------------------------------ *)

let binaries_cmd =
  let run () cache_dir =
    Harness.run @@ fun ~sw env ->
    let h = Harness.bootstrap ~sw env cache_dir in
    with_d10 h @@ fun d10 ->
    let index_path = index_path_s d10 in
    if not (Sys.file_exists index_path) then begin
      Fmt.pr "No index found. Run %a first.@." Oi.Style.accent_string
        "oi cache index";
      exit 0
    end;
    let db = D10.Index.open_ ~path:index_path in
    let bins = D10.Index.all_binaries db ~os_key:d10.os_key in
    Fmt.pr "%a %s@.@." Oi.Style.header_string "Binaries" d10.os_key;
    let rows =
      List.map
        (fun (binary, pkg_name, pkg_ver) ->
          [
            Tty.Span.text binary;
            Tty.Span.text pkg_name;
            Tty.Span.styled Oi.Style.dim pkg_ver;
          ])
        bins
    in
    let table =
      Tty.Table.of_rows ~header_style:Oi.Style.header
        [
          Tty.Table.column "BINARY";
          Tty.Table.column "PACKAGE";
          Tty.Table.column "VERSION";
        ]
        rows
    in
    Tty.Table.pp Fmt.stdout table;
    Fmt.pr "@.%a %d binar%s@." Oi.Style.header_string "Total:"
      (List.length bins)
      (if List.length bins = 1 then "y" else "ies");
    D10.Index.close db
  in
  let info =
    Cmd.info "binaries" ~doc:"List binaries known to the layer index"
  in
  Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)

(* -- index --------------------------------------------------------------- *)

let index_cmd =
  let run () cache_dir =
    Harness.run @@ fun ~sw env ->
    let h = Harness.bootstrap ~sw env cache_dir in
    with_d10 h @@ fun d10 ->
    let layers_root = Eio.Path.native_exn d10.root / "layers" in
    let index_path = layers_root / "index.db" in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(d10.fs / layers_root);
    let db = D10.Index.open_ ~path:index_path in
    let totals = ref (0, 0, 0) in
    let rows = ref [] in
    if Sys.file_exists layers_root then
      Array.iter
        (fun entry ->
          let dir = layers_root / entry in
          let skip =
            (not (Sys.is_directory dir))
            || entry = "." || entry = ".."
            || Filename.check_suffix entry ".db"
            || Filename.check_suffix entry ".db-shm"
            || Filename.check_suffix entry ".db-wal"
          in
          if not skip then begin
            let c : D10.Config.t = { d10 with os_key = entry } in
            let cache_root = Eio.Path.native_exn d10.root in
            let overlay_for ~hash =
              Oi.Provenance.overlay_of_layer ~fs:h.fs ~cache_root ~os_key:entry
                ~hash
            in
            D10.Index.rebuild c ~overlay_for db;
            let nl, nb, nf = D10.Index.stats db ~os_key:entry in
            let l, b, f = !totals in
            totals := (l + nl, b + nb, f + nf);
            rows :=
              [
                Tty.Span.text entry;
                Tty.Span.text (string_of_int nl);
                Tty.Span.text (string_of_int nb);
                Tty.Span.text (string_of_int nf);
              ]
              :: !rows
          end)
        (Sys.readdir layers_root);
    D10.Index.close db;
    if !rows = [] then Fmt.pr "No layers indexed.@."
    else begin
      let table =
        Tty.Table.of_rows ~header_style:Oi.Style.header
          [
            Tty.Table.column "OS_KEY";
            Tty.Table.column ~align:`Right "LAYERS";
            Tty.Table.column ~align:`Right "BINARIES";
            Tty.Table.column ~align:`Right "FILES";
          ]
          (List.rev !rows)
      in
      Tty.Table.pp Fmt.stdout table
    end;
    let l, b, f = !totals in
    Fmt.pr "@.%a %d layers, %d binaries, %d files@." Oi.Style.header_string
      "Total:" l b f;
    Fmt.pr "Index: %s@." index_path
  in
  let info = Cmd.info "index" ~doc:"Rebuild the SQLite layer index" in
  Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)

(* -- stats --------------------------------------------------------------- *)

let stats_cmd =
  let run () cache_dir =
    Harness.run @@ fun ~sw env ->
    let h = Harness.bootstrap ~sw env cache_dir in
    with_d10 h @@ fun d10 ->
    let index_path = index_path_s d10 in
    if not (Sys.file_exists index_path) then begin
      Fmt.pr "No index found. Run %a first.@." Oi.Style.accent_string
        "oi cache index";
      exit 0
    end;
    let db = D10.Index.open_ ~path:index_path in
    let nl, nb, nf = D10.Index.stats db ~os_key:d10.os_key in
    Fmt.pr "@[<v>%a %s@," Oi.Style.header_string "Stats" d10.os_key;
    Fmt.pr "  layers:   %d@," nl;
    Fmt.pr "  binaries: %d@," nb;
    Fmt.pr "  files:    %d@,@]@." nf;
    D10.Index.close db
  in
  let info = Cmd.info "stats" ~doc:"Summarise the layer cache for this host" in
  Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)

(* -- group --------------------------------------------------------------- *)

let cmd =
  let info =
    Cmd.info "cache" ~doc:"Inspect the d10 content-addressed layer cache"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Read-only views into the layer cache that backs $(b,oi build) and \
             $(b,oi run). The cache lives at \
             $(b,\\$OI_CACHE_DIR/layers/<os_key>/), with a SQLite index at \
             $(b,\\$OI_CACHE_DIR/layers/index.db) keyed by binary name and \
             package.";
          `P
            "$(b,oi cache index) rebuilds the index from disk; the other \
             subcommands query it (or read $(b,layer.json) sidecars directly).";
          `S "SEE ALSO";
          `P
            "$(b,oi clean)(1) drops layers; $(b,oi build --export)(1) \
             publishes them to a registry tree.";
        ]
  in
  Cmd.group info [ list_cmd; show_cmd; binaries_cmd; index_cmd; stats_cmd ]
