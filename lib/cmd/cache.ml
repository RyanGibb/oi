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

type layer_summary = {
  layer_hash : string;
  package : string option;
  exit_status : int option;
}

let layer_summary_codec =
  let open Jsont in
  Object.map ~kind:"layer_summary" (fun layer_hash package exit_status ->
      { layer_hash; package; exit_status })
  |> Object.mem "layer_hash" string ~enc:(fun l -> l.layer_hash)
  |> Object.opt_mem "package" string ~enc:(fun l -> l.package)
  |> Object.opt_mem "exit_status" int ~enc:(fun l -> l.exit_status)
  |> Object.finish

let list_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_list" (fun _schema_version os_key layers ->
      (os_key, layers))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "os_key" string ~enc:(fun (k, _) -> k)
  |> Object.mem "layers" (list layer_summary_codec) ~enc:(fun (_, ls) -> ls)
  |> Object.finish

let read_layer_summaries ~layers_dir ~root ~os_key =
  if not (Sys.file_exists layers_dir) then []
  else
    Sys.readdir layers_dir |> Array.to_list |> List.sort String.compare
    |> List.map (fun hash ->
        let json = Eio.Path.(root / "layers" / os_key / hash / "layer.json") in
        match D10.Layer.load_meta json with
        | Some m ->
            {
              layer_hash = hash;
              package = Some m.package;
              exit_status = Some m.exit_status;
            }
        | None -> { layer_hash = hash; package = None; exit_status = None })

let list_cmd =
  let run (c : Terms.common) =
    Harness.run @@ fun ~sw env ->
    let h =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    with_d10 h @@ fun d10 ->
    let layers_dir = layers_root_s d10 in
    let summaries =
      read_layer_summaries ~layers_dir ~root:d10.root ~os_key:d10.os_key
    in
    match c.format with
    | Json -> (
        match
          Jsont_bytesrw.encode_string ~format:Jsont.Indent list_envelope_codec
            (d10.os_key, summaries)
        with
        | Ok s ->
            print_string s;
            print_newline ()
        | Error e -> Oi.Error.config_error "json encode failed: %s" e)
    | Text ->
        Fmt.pr "%a %s@.@." Oi.Style.header_string "Layers" d10.os_key;
        if summaries = [] then Fmt.pr "  (empty)@."
        else begin
          let total = List.length summaries in
          let rows =
            List.map
              (fun l ->
                match (l.package, l.exit_status) with
                | Some pkg, Some st ->
                    [
                      Tty.Span.styled Oi.Style.dim (short_hash l.layer_hash);
                      status_span st;
                      Tty.Span.text pkg;
                    ]
                | _ ->
                    [
                      Tty.Span.styled Oi.Style.dim (short_hash l.layer_hash);
                      Tty.Span.styled Oi.Style.warn "no metadata";
                      Tty.Span.text "—";
                    ])
              summaries
          in
          let table =
            Tty.Table.of_rows ~header_style:Oi.Style.header
              [
                Tty.Table.column "HASH";
                Tty.Table.column "STATUS";
                Tty.Table.column "PACKAGE";
              ]
              rows
          in
          Tty.Table.pp Fmt.stdout table;
          Fmt.pr "@.%a %d layer(s)@." Oi.Style.header_string "Total:" total
        end
  in
  let info = Cmd.info "list" ~doc:"List every cached layer for the host" in
  Cmd.v info Term.(const run $ Terms.common)

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

type show_match = {
  layer_hash : string;
  meta : D10.Layer.meta;
  files_count : int;
  provenance : Oi.Provenance.t option;
  callers : Oi.Audit.event list;
}

let show_match_codec =
  let open Jsont in
  Object.map ~kind:"cache_show_layer"
    (fun layer_hash meta files_count provenance callers ->
      { layer_hash; meta; files_count; provenance; callers })
  |> Object.mem "layer_hash" string ~enc:(fun m -> m.layer_hash)
  |> Object.mem "meta" D10.Layer.meta_codec ~enc:(fun m -> m.meta)
  |> Object.mem "files_count" int ~enc:(fun m -> m.files_count)
  |> Object.opt_mem "provenance" Oi.Provenance.codec ~enc:(fun m ->
      m.provenance)
  |> Object.mem "callers"
       (list Oi.Audit.event_codec)
       ~dec_absent:[]
       ~enc:(fun m -> m.callers)
       ~enc_omit:(( = ) [])
  |> Object.finish

let show_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_show"
    (fun _schema_version os_key package_pattern matches ->
      (os_key, package_pattern, matches))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "os_key" string ~enc:(fun (k, _, _) -> k)
  |> Object.mem "package_pattern" string ~enc:(fun (_, p, _) -> p)
  |> Object.mem "matches" (list show_match_codec) ~enc:(fun (_, _, m) -> m)
  |> Object.finish

let show_cmd =
  let run (c : Terms.common) package =
    Harness.run @@ fun ~sw env ->
    let h =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    with_d10 h @@ fun d10 ->
    let layers_dir = layers_root_s d10 in
    let cache_root = Eio.Path.native_exn d10.root in
    let events_by_hash : (string, Oi.Audit.event list) Hashtbl.t =
      let tbl = Hashtbl.create 256 in
      if Sys.file_exists layers_dir then begin
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
          all
      end;
      tbl
    in
    let events_for hash =
      match Hashtbl.find_opt events_by_hash hash with
      | Some xs -> xs
      | None -> []
    in
    let matches =
      if not (Sys.file_exists layers_dir) then []
      else
        Sys.readdir layers_dir |> Array.to_list
        |> List.filter_map (fun hash ->
            let json =
              Eio.Path.(d10.root / "layers" / d10.os_key / hash / "layer.json")
            in
            match D10.Layer.load_meta json with
            | Some m
              when String.length m.package >= String.length package
                   && String.sub m.package 0 (String.length package) = package
              ->
                let files_count = count_files (layers_dir / hash / "fs") in
                let provenance =
                  Oi.Provenance.read_one ~fs:h.fs ~cache_root ~os_key:d10.os_key
                    ~hash
                in
                Some
                  {
                    layer_hash = hash;
                    meta = m;
                    files_count;
                    provenance;
                    callers = events_for hash;
                  }
            | _ -> None)
    in
    match c.format with
    | Json -> (
        match
          Jsont_bytesrw.encode_string ~format:Jsont.Indent show_envelope_codec
            (d10.os_key, package, matches)
        with
        | Ok s ->
            print_string s;
            print_newline ()
        | Error e -> Oi.Error.config_error "json encode failed: %s" e)
    | Text ->
        if matches = [] then Fmt.pr "No layers found matching %S@." package
        else
          List.iter
            (fun (m : show_match) ->
              let status =
                if m.meta.exit_status = 0 then
                  Fmt.str "%a" Oi.Style.ok_string "ok"
                else
                  Fmt.str "%a (exit %d)" Oi.Style.error_string "failed"
                    m.meta.exit_status
              in
              Fmt.pr "@[<v>%a %s@," Oi.Style.header_string "Layer" m.layer_hash;
              Fmt.pr "  package:  %s@," m.meta.package;
              Fmt.pr "  status:   %s@," status;
              Fmt.pr "  created:  %s@," (pp_time m.meta.created);
              Fmt.pr "  deps:     %s@,"
                (if m.meta.deps = [] then "(none)"
                 else String.concat ", " m.meta.deps);
              Fmt.pr "  hashes:   %s@,"
                (if m.meta.hashes = [] then "(none)"
                 else String.concat ", " (List.map short_hash m.meta.hashes));
              Fmt.pr "  files:    %d@," m.files_count;
              (match m.provenance with
              | None -> ()
              | Some p -> print_provenance p);
              print_callers m.callers;
              Fmt.pr "@,@]@.")
            matches
  in
  let package =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PACKAGE" ~doc:"Package name (prefix match)." [])
  in
  let info = Cmd.info "show" ~doc:"Show details for a package's layers" in
  Cmd.v info Term.(const run $ Terms.common $ package)

(* -- binaries ------------------------------------------------------------ *)

type binary_entry = { binary : string; pkg_name : string; pkg_version : string }

let binary_entry_codec =
  let open Jsont in
  Object.map ~kind:"binary" (fun binary pkg_name pkg_version ->
      { binary; pkg_name; pkg_version })
  |> Object.mem "binary" string ~enc:(fun e -> e.binary)
  |> Object.mem "package" string ~enc:(fun e -> e.pkg_name)
  |> Object.mem "version" string ~enc:(fun e -> e.pkg_version)
  |> Object.finish

let binaries_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_binaries"
    (fun _schema_version os_key index_present binaries ->
      (os_key, index_present, binaries))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "os_key" string ~enc:(fun (k, _, _) -> k)
  |> Object.mem "index_present" bool ~enc:(fun (_, p, _) -> p)
  |> Object.mem "binaries" (list binary_entry_codec) ~enc:(fun (_, _, b) -> b)
  |> Object.finish

let binaries_cmd =
  let run (c : Terms.common) =
    Harness.run @@ fun ~sw env ->
    let h =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    with_d10 h @@ fun d10 ->
    let index_path = index_path_s d10 in
    let index_present = Sys.file_exists index_path in
    let bins =
      if index_present then begin
        let db = D10.Index.open_ ~path:index_path in
        let raw = D10.Index.all_binaries db ~os_key:d10.os_key in
        D10.Index.close db;
        List.map
          (fun (binary, pkg_name, pkg_version) ->
            { binary; pkg_name; pkg_version })
          raw
      end
      else []
    in
    match c.format with
    | Json -> (
        match
          Jsont_bytesrw.encode_string ~format:Jsont.Indent
            binaries_envelope_codec
            (d10.os_key, index_present, bins)
        with
        | Ok s ->
            print_string s;
            print_newline ()
        | Error e -> Oi.Error.config_error "json encode failed: %s" e)
    | Text ->
        if not index_present then
          Fmt.pr "No index found. Run %a first.@." Oi.Style.accent_string
            "oi cache index"
        else begin
          Fmt.pr "%a %s@.@." Oi.Style.header_string "Binaries" d10.os_key;
          let rows =
            List.map
              (fun b ->
                [
                  Tty.Span.text b.binary;
                  Tty.Span.text b.pkg_name;
                  Tty.Span.styled Oi.Style.dim b.pkg_version;
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
            (if List.length bins = 1 then "y" else "ies")
        end
  in
  let info =
    Cmd.info "binaries" ~doc:"List binaries known to the layer index"
  in
  Cmd.v info Term.(const run $ Terms.common)

(* -- index --------------------------------------------------------------- *)

let index_cmd =
  let run (c : Terms.common) =
    Harness.run @@ fun ~sw env ->
    let h =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
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
  Cmd.v info Term.(const run $ Terms.common)

(* -- stats --------------------------------------------------------------- *)

let stats_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_stats"
    (fun _schema_version os_key index_present layers binaries files ->
      (os_key, index_present, layers, binaries, files))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "os_key" string ~enc:(fun (k, _, _, _, _) -> k)
  |> Object.mem "index_present" bool ~enc:(fun (_, p, _, _, _) -> p)
  |> Object.mem "layers" int ~enc:(fun (_, _, l, _, _) -> l)
  |> Object.mem "binaries" int ~enc:(fun (_, _, _, b, _) -> b)
  |> Object.mem "files" int ~enc:(fun (_, _, _, _, f) -> f)
  |> Object.finish

let stats_cmd =
  let run (c : Terms.common) =
    Harness.run @@ fun ~sw env ->
    let h =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    with_d10 h @@ fun d10 ->
    let index_path = index_path_s d10 in
    let index_present = Sys.file_exists index_path in
    let nl, nb, nf =
      if index_present then begin
        let db = D10.Index.open_ ~path:index_path in
        let s = D10.Index.stats db ~os_key:d10.os_key in
        D10.Index.close db;
        s
      end
      else (0, 0, 0)
    in
    match c.format with
    | Json -> (
        match
          Jsont_bytesrw.encode_string ~format:Jsont.Indent stats_envelope_codec
            (d10.os_key, index_present, nl, nb, nf)
        with
        | Ok s ->
            print_string s;
            print_newline ()
        | Error e -> Oi.Error.config_error "json encode failed: %s" e)
    | Text ->
        if not index_present then
          Fmt.pr "No index found. Run %a first.@." Oi.Style.accent_string
            "oi cache index"
        else begin
          Fmt.pr "@[<v>%a %s@," Oi.Style.header_string "Stats" d10.os_key;
          Fmt.pr "  layers:   %d@," nl;
          Fmt.pr "  binaries: %d@," nb;
          Fmt.pr "  files:    %d@,@]@." nf
        end
  in
  let info = Cmd.info "stats" ~doc:"Summarise the layer cache for this host" in
  Cmd.v info Term.(const run $ Terms.common)

(* -- explain ------------------------------------------------------------- *)

(* Tree projection of {!Oi.Provenance.t}. Used as the JSON shape emitted by
   [oi cache explain] — pulls the full transitive dependency closure into
   one document so an agent can ask "why does this layer behave this way?"
   without re-walking [provenance.json] sidecars itself. *)

type explain_node = {
  layer_hash : string;
  provenance : Oi.Provenance.t;
  depends_on : explain_node list;
}

let rec explain_node_codec =
  lazy
    (let open Jsont in
     Object.map ~kind:"layer_explanation"
       (fun layer_hash provenance depends_on ->
         { layer_hash; provenance; depends_on })
     |> Object.mem "layer_hash" string ~enc:(fun n -> n.layer_hash)
     |> Object.mem "provenance" Oi.Provenance.codec ~enc:(fun n -> n.provenance)
     |> Object.mem "depends_on"
          (list (Jsont.rec' explain_node_codec))
          ~dec_absent:[]
          ~enc:(fun n -> n.depends_on)
          ~enc_omit:(( = ) [])
     |> Object.finish)

let explain_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_explain" (fun _schema_version root -> root)
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "root" (Lazy.force explain_node_codec) ~enc:(fun n -> n)
  |> Object.finish

let explain_cmd =
  let run (c : Terms.common) hash =
    Harness.run @@ fun ~sw env ->
    let h =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    with_d10 h @@ fun d10 ->
    let cache_root = Eio.Path.native_exn d10.root in
    let memo : (string, explain_node) Hashtbl.t = Hashtbl.create 64 in
    let rec walk hash =
      match Hashtbl.find_opt memo hash with
      | Some n -> n
      | None ->
          let prov =
            match
              Oi.Provenance.read_one ~fs:h.fs ~cache_root ~os_key:d10.os_key
                ~hash
            with
            | Some p -> p
            | None ->
                Oi.Error.config_error
                  "no provenance.json for layer %s under os %s — layer may be \
                   absent, failed, or pre-provenance"
                  hash d10.os_key
          in
          let depends_on =
            List.map (fun (d : Oi.Identity.dep) -> walk d.hash) prov.deps
          in
          let n = { layer_hash = hash; provenance = prov; depends_on } in
          Hashtbl.add memo hash n;
          n
    in
    let tree = walk hash in
    match c.format with
    | Json -> (
        match
          Jsont_bytesrw.encode_string ~format:Jsont.Indent
            explain_envelope_codec tree
        with
        | Ok s ->
            print_string s;
            print_newline ()
        | Error e -> Oi.Error.config_error "json encode failed: %s" e)
    | Text ->
        let rec pp ~depth (n : explain_node) =
          let indent = String.make (depth * 2) ' ' in
          let pkg = Oi.Identity.to_string n.provenance.pkg in
          Fmt.pr "%s%s %a@." indent pkg Oi.Style.dim_string
            (short_hash n.layer_hash);
          List.iter (pp ~depth:(depth + 1)) n.depends_on
        in
        pp ~depth:0 tree
  in
  let hash =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HASH" ~doc:"Full layer hash (e.g. from $(b,oi cache list))."
          [])
  in
  let info =
    Cmd.info "explain"
      ~doc:"Trace a layer's provenance and dependencies as a tree"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Walk the dependency closure of $(b,HASH), reading each layer's \
             $(b,provenance.json) sidecar. Default output is a compact tree of \
             $(b,package layer-hash) lines.";
          `P
            "$(b,--format=json) emits the closure as one JSON document. Every \
             node carries the full provenance shape (opam origin, source URL + \
             checksums, depexts, build_env) plus a $(b,depends_on) array of \
             child nodes with the same shape.";
          `P
            "Use this to answer 'what exactly went into this build?' without \
             re-walking $(b,provenance.json) sidecars by hand. Failed layers \
             have no provenance and are not explainable here — see $(b,oi \
             cache show) for those.";
        ]
  in
  Cmd.v info Term.(const run $ Terms.common $ hash)

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
          `P
            "$(b,oi cache explain HASH) walks a layer's full dependency \
             closure with provenance — pair with $(b,--format=json) for \
             scripted analysis.";
          `S "SEE ALSO";
          `P
            "$(b,oi clean)(1) drops layers; $(b,oi build --export)(1) \
             publishes them to a registry tree.";
        ]
  in
  Cmd.group info
    [ list_cmd; show_cmd; binaries_cmd; index_cmd; stats_cmd; explain_cmd ]
