[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.pipeline"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Platform / d10 wiring ----------------------------------------------- *)

let make_conf ~platform:(p : Osrel.t) ~ocaml_version : Solver.Ctx.conf =
  {
    arch = Osrel.Arch.to_string p.arch;
    os = Osrel.OS.to_string p.os;
    os_distribution = Osrel.OS.kind_to_string p.os.kind;
    os_version = p.os.version;
    os_family = p.os.family;
    ocaml_version;
    jobs = p.jobs;
  }

let make_d10 ~sys ~fs ~clock ~cache ~os_key : D10.Config.t =
  { sys; fs; clock; root = Cache.root cache; os_key }

let init_opam_root ~fs ~data_dir =
  let opam_root = data_dir / "opam-root" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / opam_root);
  Solver.Ctx.init_opam ~root:opam_root

(* -- Toolchain ----------------------------------------------------------- *)

let toolchain_views info conf =
  let conf = Toolchain.apply_conf info conf in
  let ctx = Option.map Toolchain.opam_ctx_of_info info in
  (conf, ctx)

(* Toolchain names implied by a single handle: the [x-oi-toolchain]
   field on its latest reporepo entry, plus the handle's own name when
   it itself is a toolchain definition (i.e. some entry has matching
   [x-oi-toolchain-name]). Both pickup paths are needed: an overlay
   pinned to a non-default toolchain via [oi repo add --toolchain=oxcaml]
   uses (1); a [@oxcaml/utop] target uses (2). *)
let toolchain_names_of_handle entries handle =
  let from_field =
    match Source.Reporepo.latest entries ~handle with
    | Some (e : Source.Reporepo.entry) -> Stdlib.Option.to_list e.toolchain
    | None -> []
  in
  let from_self =
    if Toolchain.depends_of ~handle <> None then [ handle ] else []
  in
  from_field @ from_self

let resolve_toolchain ~fs ~sys ~data_dir ~conf ~install ~override ~handles () =
  let path = Source.Reporepo.env_path () in
  let entries =
    if Sys.file_exists path then
      try Source.Reporepo.load ~path with Error.E _ -> []
    else []
  in
  (* Pick a toolchain handle by precedence; resolve once at the end. *)
  let pick () =
    match override with
    | Some h ->
        Log.debug (fun m -> m "Using --toolchain=%s" h);
        h
    | None -> (
        let from_scope =
          List.concat_map (toolchain_names_of_handle entries) handles
          |> List.sort_uniq String.compare
        in
        match from_scope with
        | [ n ] ->
            Log.debug (fun m -> m "Using toolchain %s from handle scope" n);
            n
        | many when many <> [] ->
            Error.config_error
              "overlays in scope declare conflicting toolchains: %s — pass \
               --toolchain=NAME to disambiguate"
              (String.concat ", " many)
        | _ -> (
            match Source.Reporepo.default_toolchain entries with
            | Some e ->
                let n =
                  Stdlib.Option.value e.toolchain_name ~default:e.handle
                in
                Log.debug (fun m -> m "Using default toolchain %s" n);
                n
            | None ->
                let known =
                  entries
                  |> List.filter_map (fun (e : Source.Reporepo.entry) ->
                      e.toolchain_name)
                  |> List.sort_uniq String.compare
                in
                let hint =
                  if known = [] then
                    "the reporepo has no toolchain definitions yet"
                  else
                    Fmt.str
                      "mark one with: oi repo bump <handle> --default. Known \
                       toolchains: %s"
                      (String.concat ", " known)
                in
                Error.config_error
                  "no default toolchain set in reporepo at %s — %s. Or pass \
                   --toolchain=NAME explicitly."
                  path hint))
  in
  let info = Toolchain.resolve ~fs ~sys ~data_dir ~conf ~handle:(pick ()) in
  if install then Toolchain.ensure_installed ~fs info;
  Some info

let drop_override_compiler_roots ~override ~toolchain names =
  match (override, (toolchain : Toolchain.info option)) with
  | None, _ | _, None -> names
  | Some _, Some info ->
      List.filter
        (fun n -> not (OpamPackage.Name.Set.mem n info.root_names))
        names

(* -- Sources ------------------------------------------------------------- *)

let materialize_with_deps ~fs ~sys ~cache ?refresh with_deps =
  let urls, pkg_deps = Project.Url.classify_all with_deps in
  let url_project = Project.Url.materialize ~fs ~sys ~cache ?refresh urls in
  (pkg_deps, url_project)

let filter_compatible_overlays ~reporepo_path ~toolchain handles =
  match (toolchain : Toolchain.info option) with
  | None -> handles
  | Some info ->
      let entries =
        try Source.Reporepo.load ~path:reporepo_path with Error.E _ -> []
      in
      List.filter
        (fun h ->
          match Source.Reporepo.latest entries ~handle:h with
          | None -> true
          | Some (e : Source.Reporepo.entry) -> (
              match e.toolchain with
              | None -> true
              | Some t when t = info.handle -> true
              | Some t ->
                  Logs.info (fun m ->
                      m
                        "Dropping overlay @%s: built against toolchain %s, \
                         incompatible with --toolchain=%s"
                        h t info.handle);
                  false))
        handles

(* -- Build helpers ------------------------------------------------------- *)

let cache_urls ~cache ~source_remote =
  let local = Source.Mirror.url ~cache in
  match source_remote with
  | Some (`Http_remote r) -> [ local; Source.Mirror.remote_url ~registry:r ]
  | None | Some _ -> [ local ]

(* HTTP fetches are I/O-bound: a fiber spends ~all of its wall-time
   waiting on the wire, so we want many more concurrent fibers than we
   have CPUs. Default 16 (clamped to the larger of 16 and the recommended
   domain count). Override via [OI_HTTP_PARALLELISM]; [?jobs] (which
   originates from [-j N] and primarily caps build subprocesses) wins
   when explicitly set. *)
let fetch_parallelism ?jobs () =
  match jobs with
  | Some n when n > 0 -> n
  | _ -> (
      match Sys.getenv_opt "OI_HTTP_PARALLELISM" with
      | Some s -> (
          match int_of_string_opt s with Some n when n > 0 -> n | _ -> 16)
      | None -> max (Domain.recommended_domain_count ()) 16)

let fmt_mb n =
  if Int64.compare n 1_048_576L >= 0 then
    Fmt.str "%.1fMB" (Int64.to_float n /. 1_048_576.)
  else if Int64.compare n 1024L >= 0 then
    Fmt.str "%.0fKB" (Int64.to_float n /. 1024.)
  else Fmt.str "%LdB" n

(* -- Multi-bar fetch UI -------------------------------------------------- *)

let to_int = Int64.to_int

(* Layout constants live in [Ui]: the same widths drive the overall
   [Preflight] row above and [Execute]'s per-package rows below, so
   keeping one source of truth makes alignment changes a one-line
   edit. *)

(* Pretty-print bytes using Progress's standard byte unit
   ([245.8 MiB]). Used as the static [total] anchor on every row. *)
let bytes_const_of_int n =
  Progress.Printer.to_to_string Progress.Units.Bytes.of_int n

(* Aggregate row at the top of the multi-bar: bold "Pre-built"
   header rpad'd to [Ui.row_label_width] so its bar lines up with the
   per-layer bars below; [bar(24)] + total bytes + tight (pct).
   No running-byte count or elapsed: the bar position + pct already
   convey progress and elapsed-time stalls (registry slow, host CPU
   saturated) lie about ETA more than they help. *)
let aggregate_line ~total =
  let open Progress.Line in
  let header =
    constf "%a"
      Fmt.(styled `Bold string)
      (Printf.sprintf "%-*s" Ui.row_label_width "Pre-built")
  in
  let bar_seg = Ui.Theme.bar ~width:(`Fixed Ui.row_bar_width) total in
  let total_seg = const (bytes_const_of_int total) in
  let pct_seg = sum ~pp:(Ui.Theme.pct_pp ~total) ~width:6 () in
  list ~sep:(const " ") [ header; bar_seg; total_seg; pct_seg ]

(* Per-layer row: [<pkg.ver col 32> [bar(24)] <total> (<pct>)]. *)
let layer_line ~pkg ~size =
  let open Progress.Line in
  let bar_seg = Ui.Theme.bar ~width:(`Fixed Ui.row_bar_width) size in
  let total_seg = const (bytes_const_of_int size) in
  let pct_seg = sum ~pp:(Ui.Theme.pct_pp ~total:size) ~width:6 () in
  list ~sep:(const " ")
    [ const (Ui.fit_label Ui.row_label_width pkg); bar_seg; total_seg; pct_seg ]

(* Drive the live multi-bar. When [shared_display] is supplied we
   attach to it (overall bar above stays visible); otherwise we open
   our own [Display]. Both paths use [add_line] / [Reporter.t] so the
   [Display.t]'s type parameter stays unconstrained. *)
let fetch_with_display ?jobs ?shared_display ~session ~remote ~d10 ~index
    ~available ~pkg_of ~total_bytes_known ~clock () =
  let n_total = List.length available in
  let bytes_received = ref 0L in
  let done_count = ref 0 in
  let cfg =
    Progress.Config.v ~ppf:Format.err_formatter ~persistent:false ()
  in
  let owns_display, display =
    match shared_display with
    | Some d -> (false, d)
    | None ->
        let d = Progress.Display.start ~config:cfg Progress.Multi.blank in
        (true, d)
  in
  let agg_handle =
    Progress.Display.add_line display
      (aggregate_line ~total:(to_int total_bytes_known))
  in
  let layer_handles : (string, int Progress.Reporter.t) Hashtbl.t =
    Hashtbl.create n_total
  in
  let layer_size : (string, int) Hashtbl.t = Hashtbl.create n_total in
  let lock = Mutex.create () in
  let with_lock f = Mutex.protect lock f in
  let stop_heartbeat = ref false in
  Eio.Switch.run @@ fun hb_sw ->
  if owns_display then
    Eio.Fiber.fork_daemon ~sw:hb_sw (fun () ->
        let rec loop () =
          Eio.Time.sleep clock 0.1;
          if !stop_heartbeat then `Stop_daemon
          else begin
            (try Progress.Display.tick display with _ -> ());
            loop ()
          end
        in
        loop ());
  let ensure_line hash =
    if not (Hashtbl.mem layer_handles hash) then begin
      let my_size =
        Stdlib.Option.value (Hashtbl.find_opt layer_size hash) ~default:0
      in
      (* Sort rows size-descending: biggest layers (which dominate
         fetch time) pin near the top. [~above] counts existing rows
         smaller than us; we land just above them. *)
      let above =
        Hashtbl.fold
          (fun h _ acc ->
            let s =
              Stdlib.Option.value (Hashtbl.find_opt layer_size h) ~default:0
            in
            if s < my_size then acc + 1 else acc)
          layer_handles 0
      in
      let pkg =
        Stdlib.Option.value (Hashtbl.find_opt pkg_of hash) ~default:""
      in
      let r =
        Progress.Display.add_line ~above display
          (layer_line ~pkg ~size:my_size)
      in
      Hashtbl.add layer_handles hash r
    end
  in
  let report_to_layer hash delta =
    match Hashtbl.find_opt layer_handles hash with
    | None -> ()
    | Some r -> (try Progress.Reporter.report r delta with _ -> ())
  in
  let fiber_progress hash hash_ref ~received ~total:_ =
    with_lock @@ fun () ->
    let prev = !hash_ref in
    hash_ref := received;
    let delta = Int64.sub received prev in
    bytes_received := Int64.add !bytes_received delta;
    let delta_i = to_int delta in
    (try Progress.Reporter.report agg_handle delta_i with _ -> ());
    report_to_layer hash delta_i
  in
  let remove_line hash =
    with_lock @@ fun () ->
    match Hashtbl.find_opt layer_handles hash with
    | None -> ()
    | Some r ->
        (try Progress.Reporter.finalise r with _ -> ());
        (try Progress.Display.remove_line display r with _ -> ());
        Hashtbl.remove layer_handles hash
  in
  Fun.protect
    ~finally:(fun () ->
      stop_heartbeat := true;
      (* Always retire the aggregate row; finalise the display only
         when we own it. *)
      (try Progress.Reporter.finalise agg_handle with _ -> ());
      (try Progress.Display.remove_line display agg_handle with _ -> ());
      if owns_display then
        try Progress.Display.finalise display with _ -> ())
    (fun () ->
      Eio.Fiber.List.iter
        ~max_fibers:(fetch_parallelism ?jobs ())
        (fun hash ->
          let sha256 =
            Option.map
              (fun (e : D10.Layer.index_entry) -> e.sha256)
              (Hashtbl.find_opt index hash)
          in
          let size =
            match Hashtbl.find_opt index hash with
            | Some (e : D10.Layer.index_entry) -> e.size
            | None -> 0L
          in
          with_lock (fun () ->
              Hashtbl.replace layer_size hash (to_int size);
              ensure_line hash);
          let received_ref = ref 0L in
          let on_progress = fiber_progress hash received_ref in
          let ok =
            D10.Layer.pull_remote d10 ~remote ~hash ~session ~on_progress
              ?sha256 ()
          in
          remove_line hash;
          if ok then begin
            with_lock (fun () -> incr done_count);
            Logs.info (fun m -> m "Fetched %s from registry" hash)
          end)
        available);
  (!done_count, !bytes_received)

let fetch_remote_layers ?on_phase ?on_progress ?jobs ?shared_display ~session
    ~layer_remote ~d10 ~packages_dirs ~ctx ~pkgs build_plan =
  match layer_remote with
  | None -> build_plan
  | Some r ->
      let source_hashes =
        List.filter_map
          (fun (node : Plan.node) ->
            match node.method_ with
            | Identity.Source -> Some node.layer_hash
            | Binary -> None)
          (Plan.nodes build_plan)
      in
      if source_hashes = [] then build_plan
      else
        let index = D10.Layer.fetch_remote_index d10 ~session ~remote:r in
        let available =
          List.filter (fun h -> Hashtbl.mem index h) source_hashes
        in
        if available = [] then begin
          Logs.info (fun m ->
              m "Registry has none of the %d needed layer(s)"
                (List.length source_hashes));
          build_plan
        end
        else begin
          let n_total = List.length available in
          let total_bytes_known =
            List.fold_left
              (fun acc h ->
                match Hashtbl.find_opt index h with
                | Some (e : D10.Layer.index_entry) -> Int64.add acc e.size
                | None -> acc)
              0L available
          in
          Logs.info (fun m ->
              m "Fetching %d layer(s) from registry (%s, %d needed)..." n_total
                (fmt_mb total_bytes_known) (List.length source_hashes));
          (* Build a hash → "pkg.version" lookup for the per-row labels.
             Layers we don't have a node for fall through to "" and the
             row shows blanks. *)
          let pkg_of : (string, string) Hashtbl.t = Hashtbl.create n_total in
          List.iter
            (fun (n : Plan.node) ->
              Hashtbl.replace pkg_of n.layer_hash
                (OpamPackage.to_string n.pkg))
            (Plan.nodes build_plan);
          let done_count, bytes_received =
            if Tty.is_tty () then
              fetch_with_display ?jobs ?shared_display ~session ~remote:r ~d10
                ~index ~available ~pkg_of ~total_bytes_known ~clock:d10.clock
                ()
            else begin
              (* Non-TTY: single-line text update routed through
                 [on_progress] / [on_phase]. No multi-bar UI to corrupt. *)
              let done_count = ref 0 in
              let bytes_total = ref 0L in
              let progress_sink =
                match (on_progress, on_phase) with
                | Some f, _ | None, Some f -> f
                | None, None -> fun _ -> ()
              in
              let emit () =
                progress_sink
                  (Fmt.str "Fetching layers from registry (%d/%d, %s)"
                     !done_count n_total (fmt_mb !bytes_total))
              in
              let fiber_progress hash_ref ~received ~total:_ =
                let prev = !hash_ref in
                hash_ref := received;
                bytes_total :=
                  Int64.add !bytes_total (Int64.sub received prev);
                emit ()
              in
              Eio.Fiber.List.iter
                ~max_fibers:(fetch_parallelism ?jobs ())
                (fun hash ->
                  let sha256 =
                    Option.map
                      (fun (e : D10.Layer.index_entry) -> e.sha256)
                      (Hashtbl.find_opt index hash)
                  in
                  let received_ref = ref 0L in
                  let on_progress = fiber_progress received_ref in
                  if
                    D10.Layer.pull_remote d10 ~remote:r ~hash ~session
                      ~on_progress ?sha256 ()
                  then begin
                    incr done_count;
                    emit ();
                    Logs.info (fun m -> m "Fetched %s from registry" hash)
                  end)
                available;
              (!done_count, !bytes_total)
            end
          in
          (match on_progress with
          | Some _ -> Say.progress_clear ()
          | None -> ());
          (match on_phase with
          | Some f ->
              f
                (Fmt.str "Fetched %d/%d layers from registry (%s)" done_count
                   n_total (fmt_mb bytes_received))
          | None -> ());
          Plan.build ctx ~d10 ~packages_dirs pkgs
        end

(* -- Central build pipeline (was main.ml's solve_and_ensure_layers) ----- *)

let build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf ~os_key ~session
    ?(dry_run = false) ?(extra_repos = []) ?(pins = []) ?(refresh = false)
    ?layer_remote ?source_remote ?jobs ?toolchain
    ?(constraints = OpamPackage.Name.Map.empty) ?project_root
    ?local_packages_dir ?on_phase ?on_progress ?preflight_done ?shared_display
    names =
  let on_phase =
    match on_phase with
    | Some f -> f
    | None -> fun s -> Logs.info (fun m -> m "%s" s)
  in
  let extra_pkg_dirs =
    Source.Repo.ensure_extra ~fs ~data_dir ~refresh extra_repos
  in
  let pins = Source.Pin.resolve_pins ~fs ~sys ?project_root pins in
  let pin_dir = Source.Pin.materialize ~fs ~sys ~cache ~refresh pins in
  (* When a toolchain is active, its [info.packages_dirs] replaces the
     base set — stacking on top would re-add [relocatable], whose
     [ocaml-base-compiler.5.5.0] competes with the toolchain-pinned
     [ocaml-variants.5.2.0+ox] and the [conflict-class] chain rejects
     both. *)
  let base_pkg_dirs =
    match toolchain with
    | None -> Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ()
    | Some (info : Toolchain.info) -> info.packages_dirs
  in
  let packages_dirs =
    Stdlib.Option.to_list local_packages_dir
    @ Stdlib.Option.to_list pin_dir
    @ extra_pkg_dirs @ base_pkg_dirs
  in
  let conf, toolchain_ctx = toolchain_views toolchain conf in
  Log.debug (fun m ->
      m "solver packages_dirs (first-wins, %d entries):%s"
        (List.length packages_dirs)
        (String.concat ""
           (List.map (fun d -> Fmt.str "\n  %s" d) packages_dirs)));
  let cache_root = Cache.root_s cache in
  let d10 = make_d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key in
  (* Fast-path: if we've seen these exact inputs before AND every layer
     hash we stored is still present in the d10 cache, skip
     [Solver.Ctx.create] + [Solver.solve] + [Plan.build] entirely. The
     ~900ms of opam-file parsing that [Solver.Ctx.create] does swamps
     everything else in the happy-path [oi run] of an already-built
     target, so cutting it out here is the main perf win. Disabled
     when [dry_run] is set since that mode needs the full plan tree to
     print. *)
  let layer_cache_key =
    if dry_run then None
    else
      Solver.Cache.key ~conf ~packages_dirs ~constraints ~names
        ?toolchain:toolchain_ctx ()
  in
  let fast_hashes =
    match layer_cache_key with
    | None -> None
    | Some k -> (
        match Solver.Cache.lookup_layers ~cache_root ~key:k with
        | None -> None
        | Some hashes
          when List.for_all (fun h -> D10.Layer.succeeded d10 ~hash:h) hashes ->
            Some hashes
        | Some _ ->
            Logs.info (fun m ->
                m "layer cache entry stale (layers missing), falling through");
            None)
  in
  (* [preflight_done] used to be invoked here by [Pipeline.build] mid-flow,
     to let the caller clear its own preflight bar before [Execute.run]
     opened a separate [Progress.Display]. With the shared-display
     architecture introduced by {!Ui.Preflight} that's no longer
     necessary — [Pipeline.build]'s subsystems attach their multi-bar
     lines to the caller's display via [add_line], so the display
     stays open the entire time. The parameter is kept on the
     signature for backwards-compatibility with the few callers that
     still pass it; it's only fired at the {i very} end of this
     function (after [Execute.run] returns), so the display isn't
     finalised while inner code is still trying to drive it. *)
  let fire_preflight_done () =
    match preflight_done with Some f -> f () | None -> ()
  in
  match fast_hashes with
  | Some hashes ->
      Logs.info (fun m ->
          m "layer cache hit %s (%d layers), skipping solve"
            (String.sub (Stdlib.Option.get layer_cache_key) 0 12)
            (List.length hashes));
      fire_preflight_done ();
      hashes
  | None ->
      (* Phase narration funneled through [on_phase] so the caller picks
         the visual: [oi run] feeds into a TTY-only spinner that clears
         on exit; [oi sync] / [oi build] feeds into [Say.step] for a
         visible audit trail. *)
      on_phase
        (Fmt.str "Building solver context (%d package dirs)"
           (List.length packages_dirs));
      let build_prefix = cache_root / "build" / "prefix" in
      let ctx =
        Solver.Ctx.create ~prefix:build_prefix ~packages_dirs ~conf
          ?toolchain:toolchain_ctx ()
      in
      on_phase
        (Fmt.str "Solving for %d root%s" (List.length names)
           (if List.length names = 1 then "" else "s"));
      let pkgs =
        match
          Solver.solve ~fs ~cache_root ctx ~packages_dirs ~constraints names
        with
        | Ok pkgs -> pkgs
        | Error msg -> Error.no_solution msg
      in
      on_phase
        (Fmt.str "Planning %d package%s" (List.length pkgs)
           (if List.length pkgs = 1 then "" else "s"));
      let build_plan = Plan.build ctx ~d10 ~packages_dirs pkgs in
      if dry_run then begin
        let remote_has =
          match layer_remote with
          | Some r ->
              let idx = D10.Layer.fetch_remote_index d10 ~session ~remote:r in
              fun h -> Hashtbl.mem idx h
          | None -> fun _ -> false
        in
        Fmt.pr "%a@." (Plan.pp_tree ~remote_has) build_plan;
        exit 0
      end;
      let build_plan =
        match layer_remote with
        | None -> build_plan
        | Some _ ->
            on_phase "Checking registry for prebuilt layers";
            fetch_remote_layers ~on_phase ?on_progress ?jobs ?shared_display
              ~session ~layer_remote ~d10 ~packages_dirs ~ctx ~pkgs build_plan
      in
      let hashes = Plan.layer_hashes build_plan in
      (* Every layer in the plan must be cached (Binary method) to skip
         Execute.run. Checking only the top-level targets is unsafe: a
         missing transitive dep's [fs/] tree means its installed files
         are silently dropped from the assembled prefix, so we rebuild
         anything that isn't Binary. *)
      let all_layers_cached =
        List.for_all
          (fun (n : Plan.node) -> n.method_ = Identity.Binary)
          (Plan.nodes build_plan)
      in
      let persist_layer_cache () =
        match layer_cache_key with
        | None -> ()
        | Some k -> Solver.Cache.store_layers ~fs ~cache_root ~key:k hashes
      in
      if all_layers_cached then begin
        Logs.info (fun m -> m "Layers cached, skipping build");
        persist_layer_cache ();
        fire_preflight_done ();
        hashes
      end
      else begin
        (* Proactive depext check: build-from-source packages need their
           system dependencies present before compile time. Skip the
           check entirely when every node in the plan is [Binary]; the
           restore path only extracts cached layer trees and needs no
           depexts. Failures here are non-fatal warnings. *)
        let source_pkgs =
          Plan.nodes build_plan
          |> List.filter_map (fun (n : Plan.node) ->
              match n.method_ with
              | Identity.Source -> Some n.pkg
              | Identity.Binary -> None)
        in
        Log.info (fun m ->
            m
              "depext check: %d source pkg(s); platform os=%s \
               os-distribution=%s os-family=%s"
              (List.length source_pkgs) conf.os conf.os_distribution
              conf.os_family);
        if source_pkgs <> [] then begin
          let entries = Depexts.compute ctx ~packages_dirs source_pkgs in
          let all =
            List.fold_left
              (fun acc e -> OpamSysPkg.Set.union acc e.Depexts.sys_pkgs)
              OpamSysPkg.Set.empty entries
          in
          Log.info (fun m ->
              m "depext check: %d pkg(s) declare matching depexts, union = %d"
                (List.length entries)
                (OpamSysPkg.Set.cardinal all));
          (* Pure-OCaml packages legitimately have no depexts. Listing them
             here is mostly noise — the actually-actionable case is captured
             by the [missing] warning below, which names the system packages
             we know are absent on this host. The full per-package "no
             depexts declared" list is still available at [--verbosity=debug]
             for depext maintainers. *)
          let with_depexts =
            List.fold_left
              (fun acc e -> OpamPackage.Set.add e.Depexts.pkg acc)
              OpamPackage.Set.empty entries
          in
          let without_depexts =
            List.filter
              (fun p -> not (OpamPackage.Set.mem p with_depexts))
              source_pkgs
          in
          (match without_depexts with
          | [] -> ()
          | pkgs ->
              Log.debug (fun m ->
                  m "no %s depexts declared for %d source pkg(s): %s" conf.os
                    (List.length pkgs)
                    (pkgs
                    |> List.map (fun p ->
                        OpamPackage.Name.to_string (OpamPackage.name p))
                    |> List.sort_uniq String.compare
                    |> String.concat ", ")));
          if not (OpamSysPkg.Set.is_empty all) then (
            let st = Depexts.status all in
            Log.info (fun m ->
                m "depext status: installed=%d missing=%d not_found=%d"
                  (OpamSysPkg.Set.cardinal st.installed)
                  (OpamSysPkg.Set.cardinal st.missing)
                  (OpamSysPkg.Set.cardinal st.not_found));
            if not (OpamSysPkg.Set.is_empty st.missing) then
              Say.warn
                "system packages are not installed. The build may fail at \
                 compile time. Install them with: %s"
                (st.missing |> OpamSysPkg.Set.elements
                |> List.map OpamSysPkg.to_string
                |> String.concat " ");
            if not (OpamSysPkg.Set.is_empty st.not_found) then
              Say.warn
                "system packages are not known to the host package manager: %s"
                (st.not_found |> OpamSysPkg.Set.elements
                |> List.map OpamSysPkg.to_string
                |> String.concat ", "))
        end;
        let exec_plan =
          Plan.resolve ctx ~packages_dirs ~cache_root ~os_key
            ~ocaml_version:conf.ocaml_version build_plan
        in
        (* Prewarm the local source mirror from the registry's [/sources/]
           tree via the shared HTTP session. Skips packages already
           covered by the layer cache (those don't run the source fetch
           path) and archives already locally mirrored. The remaining
           fetches multiplex over the existing HTTP/2 connection
           instead of opam spawning curl/wget per package. *)
        (match source_remote with
        | Some (`Http_remote registry) ->
            let archives =
              exec_plan.packages
              |> List.filter_map (fun (p : Plan.package_plan) ->
                  match p.method_ with
                  | Identity.Binary -> None
                  | Source ->
                      let cks_of (s : Plan.source_info) =
                        List.filter_map
                          (fun s ->
                            try Some (OpamHash.of_string s) with _ -> None)
                          s.checksums
                      in
                      let main =
                        match p.source with
                        | None -> []
                        | Some s -> [ cks_of s ]
                      in
                      let extras =
                        List.map (fun (_, s) -> cks_of s) p.extra_sources
                      in
                      Some (main @ extras))
              |> List.concat
              |> List.filter (fun cks -> cks <> [])
            in
            if archives <> [] then begin
              let s =
                Source.Mirror.prewarm_remote ~session ~fs ~cache_root ~registry
                  ~max_fibers:(fetch_parallelism ?jobs ())
                  archives
              in
              Logs.info (fun m ->
                  m "Mirror prewarm: %d new, %d cached, %d failed" s.fetched
                    s.cached s.failed)
            end
        | _ -> ());
        let urls = cache_urls ~cache ~source_remote in
        Execute.run ?shared_display ~cache_urls:urls ~proc_mgr ~fs ?jobs
          ~clock:(clock :> D10.Config.clk)
          ~sys ~os_key exec_plan;
        let prefix_hash = D10.Prefix.solve_hash hashes in
        let prefix_dir =
          Eio.Path.native_exn Eio.Path.(d10.root / "prefixes" / prefix_hash)
        in
        (try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix_dir)
         with _ -> ());
        persist_layer_cache ();
        fire_preflight_done ();
        hashes
      end

let assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes =
  let d10 = make_d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key in
  D10.Prefix.assemble_cached d10 ~layer_hashes
