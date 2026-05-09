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

let solver_inputs info conf =
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

let pick_toolchain ~fs ~sys ~data_dir ~conf ~install ~override ~handles
    ?(reporter = Build_progress.null) () =
  reporter.event (Status "Resolving toolchain");
  let _ = reporter in
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
  if install then Toolchain.ensure_installed ~reporter ~fs info;
  Some info

let strip_compiler_roots_for_override ~override ~toolchain names =
  match (override, (toolchain : Toolchain.info option)) with
  | None, _ | _, None -> names
  | Some _, Some info ->
      List.filter
        (fun n -> not (OpamPackage.Name.Set.mem n info.root_names))
        names

(* -- Sources ------------------------------------------------------------- *)

let classify_with_args ~fs ~sys ~cache ?refresh
    ?(reporter = Build_progress.null) with_deps =
  if with_deps <> [] then
    reporter.event (Status (Fmt.str "Loading %d --with arg(s)"
      (List.length with_deps)));
  let urls, pkg_deps = Project.Url.classify_all with_deps in
  let url_project =
    Project.Url.materialize ~reporter ~fs ~sys ~cache ?refresh urls
  in
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
   waiting on the wire, so we run more concurrent fibers than CPUs.
   Default 8 — large enough to amortise TLS handshakes against a small
   pool, small enough that one wedged registry connection doesn't
   monopolise the [max_connections_per_host] cap and starve the
   remaining fibers. Override via [OI_HTTP_PARALLELISM]; [?jobs] (which
   originates from [-j N] and primarily caps build subprocesses) wins
   when explicitly set.

   Previously defaulted to [max(domain_count, 16)] which on a 32-core
   build host (oi.ci.dev) opened up to 32 simultaneous registry fetches
   with one HTTPS-keepalive connection each — when a remote stops
   responding mid-stream, the half-closed sockets pile up faster than
   the per-host pool can recycle them. *)
let fetch_parallelism ?jobs () =
  let default = 8 in
  match jobs with
  | Some n when n > 0 -> n
  | _ -> (
      match Sys.getenv_opt "OI_HTTP_PARALLELISM" with
      | Some s -> (
          match int_of_string_opt s with Some n when n > 0 -> n | _ -> default)
      | None -> default)

let fmt_mb n =
  if Int64.compare n 1_048_576L >= 0 then
    Fmt.str "%.1fMB" (Int64.to_float n /. 1_048_576.)
  else if Int64.compare n 1024L >= 0 then
    Fmt.str "%.0fKB" (Int64.to_float n /. 1024.)
  else Fmt.str "%LdB" n

(* -- Multi-bar fetch UI -------------------------------------------------- *)


(* Pull every layer in [available] from [remote]. UI is decoupled —
   we only emit typed events through [reporter]; the cmdliner layer
   renders them as a multi-line bar (or text, or nothing). *)
let fetch_layers ?jobs ~reporter ~session ~remote ~d10 ~index ~available
    ~pkg_of () =
  let bytes_received = ref 0L in
  let done_count = ref 0 in
  let lock = Mutex.create () in
  let with_lock f = Mutex.protect lock f in
  let total = List.length available in
  let progressed = ref 0 in
  (* Initial Aggregate seeds the bar's denominator so the count
     fraction is meaningful from the moment the phase begins. *)
  reporter.Build_progress.event
    (Aggregate { phase = Fetching; total; current = 0 });
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
      let pkg =
        Stdlib.Option.value (Hashtbl.find_opt pkg_of hash) ~default:""
      in
      reporter.Build_progress.event
        (Fetch_started { kind = Layer; key = hash; pkg; size });
      let received_ref = ref 0L in
      let on_progress ~received ~total =
        with_lock (fun () ->
            let prev = !received_ref in
            received_ref := received;
            bytes_received :=
              Int64.add !bytes_received (Int64.sub received prev));
        let total_known =
          match total with Some t -> t | None -> size
        in
        reporter.event
          (Fetch_progress
             { kind = Layer; key = hash; bytes = received; total = total_known })
      in
      let ok =
        D10.Layer.pull_remote d10 ~remote ~hash ~session ~on_progress ?sha256
          ()
      in
      reporter.event (Fetch_finished { kind = Layer; key = hash });
      let now =
        with_lock (fun () ->
            incr progressed;
            !progressed)
      in
      reporter.event
        (Aggregate { phase = Fetching; total; current = now });
      if ok then begin
        with_lock (fun () -> incr done_count);
        Logs.info (fun m -> m "Fetched %s from registry" hash)
      end)
    available;
  (!done_count, !bytes_received)

let fetch_remote_layers ?(reporter = Build_progress.null) ?jobs ~session
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
                (fmt_mb total_bytes_known)
                (List.length source_hashes));
          let pkg_of : (string, string) Hashtbl.t = Hashtbl.create n_total in
          List.iter
            (fun (n : Plan.node) ->
              Hashtbl.replace pkg_of n.layer_hash (OpamPackage.to_string n.pkg))
            (Plan.nodes build_plan);
          reporter.event
            (Phase_started
               {
                 phase = Fetching;
                 label = Fmt.str "Fetching %d layer(s) from registry" n_total;
               });
          let done_count, bytes_received =
            fetch_layers ?jobs ~reporter ~session ~remote:r ~d10 ~index
              ~available ~pkg_of ()
          in
          reporter.event (Phase_done Fetching);
          Logs.info (fun m ->
              m "Fetched %d/%d layers from registry (%s)" done_count n_total
                (fmt_mb bytes_received));
          try Plan.of_solution ctx ~d10 ~packages_dirs pkgs
          with Plan.Cycle cycles ->
            Error.config_error
              "dependency cycle in solved packages:@\n\
               %a@\n\
               This is an upstream metadata bug — the cyclic packages declare \
               mutually-equivalent [{= version}] dependencies. Run [oi build \
               --all] to skip just the offending solve group, or fix the \
               offending overlay's opam files."
              Plan.pp_cycles cycles
        end

(* -- Central build pipeline (was main.ml's solve_and_ensure_layers) ----- *)

let build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf ~os_key ~session
    ?(dry_run = false) ?(extra_repos = []) ?(pins = []) ?(refresh = false)
    ?layer_remote ?source_remote ?jobs ?toolchain
    ?(constraints = OpamPackage.Name.Map.empty) ?project_root
    ?local_packages_dir ?(reporter = Build_progress.null) ?emit_recipe
    ?save_recipe names =
  let phase ph label =
    reporter.Build_progress.event (Phase_started { phase = ph; label });
    Logs.info (fun m -> m "%s" label)
  in
  let phase_done ph = reporter.event (Phase_done ph) in
  let extra_pkg_dirs =
    Source.Repo.ensure_many ~reporter ~fs ~data_dir ~refresh extra_repos
  in
  let pins = Source.Pin.resolve_pins ~fs ~sys ?project_root pins in
  let pin_dir =
    Source.Pin.materialize ~reporter ~fs ~sys ~cache ~refresh pins
  in
  (* When a toolchain is active, its [info.packages_dirs] replaces the
     base set — stacking on top would re-add [relocatable], whose
     [ocaml-base-compiler.5.5.0] competes with the toolchain-pinned
     [ocaml-variants.5.2.0+ox] and the [conflict-class] chain rejects
     both. *)
  let base_pkg_dirs =
    match toolchain with
    | None ->
        Source.Reporepo.ensure_base ~reporter ~fs ~sys ~data_dir ~refresh ()
    | Some (info : Toolchain.info) -> info.packages_dirs
  in
  let packages_dirs =
    Stdlib.Option.to_list local_packages_dir
    @ Stdlib.Option.to_list pin_dir
    @ extra_pkg_dirs @ base_pkg_dirs
  in
  let conf, toolchain_ctx = solver_inputs toolchain conf in
  Log.debug (fun m ->
      m "solver packages_dirs (first-wins, %d entries):%s"
        (List.length packages_dirs)
        (String.concat ""
           (List.map (fun d -> Fmt.str "\n  %s" d) packages_dirs)));
  let cache_root = Cache.root_s cache in
  let d10 = make_d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key in
  (* Fast-path: if we've seen these exact inputs before AND every layer
     hash we stored is still present in the d10 cache, skip
     [Solver.Ctx.create] + [Solver.solve] + [Plan.of_solution] entirely. The
     ~900ms of opam-file parsing that [Solver.Ctx.create] does swamps
     everything else in the happy-path [oi run] of an already-built
     target, so cutting it out here is the main perf win. Disabled
     when [dry_run] is set since that mode needs the full plan tree to
     print. *)
  let layer_cache_key =
    if dry_run then None
    else
      Solver.Memo.key ~conf ~packages_dirs ~constraints ~names
        ?toolchain:toolchain_ctx ()
  in
  let fast_hashes =
    if emit_recipe <> None then
      (* emit_recipe needs the full plan (Plan.elaborate) to produce the
         recipe — the cached fast-path returns layer hashes only. *)
      None
    else
      match layer_cache_key with
      | None -> None
      | Some k -> (
          match Solver.Memo.lookup_layers ~cache_root ~key:k with
          | None -> None
          | Some hashes
            when List.for_all (fun h -> D10.Layer.succeeded d10 ~hash:h) hashes
            ->
              Some hashes
          | Some _ ->
              Logs.info (fun m ->
                  m "layer cache entry stale (layers missing), falling through");
              None)
  in
  match fast_hashes with
  | Some hashes ->
      Logs.info (fun m ->
          m "layer cache hit %s (%d layers), skipping solve"
            (String.sub (Stdlib.Option.get layer_cache_key) 0 12)
            (List.length hashes));
      hashes
  | None ->
      phase Solving
        (Fmt.str "Building solver context (%d package dirs)"
           (List.length packages_dirs));
      let build_prefix = cache_root / "build" / "prefix" in
      let ctx =
        Solver.Ctx.create ~prefix:build_prefix ~packages_dirs ~conf
          ?toolchain:toolchain_ctx ~reporter ()
      in
      phase Solving
        (Fmt.str "Solving for %d root%s" (List.length names)
           (if List.length names = 1 then "" else "s"));
      let pkgs =
        match
          Solver.solve ~reporter ~fs ~cache_root ctx ~packages_dirs ~constraints
            names
        with
        | Ok pkgs -> pkgs
        | Error msg -> Error.no_solution msg
      in
      phase_done Solving;
      phase Building
        (Fmt.str "Planning %d package%s" (List.length pkgs)
           (if List.length pkgs = 1 then "" else "s"));
      let build_plan =
        (* When emit_recipe is set, don't consult the d10 cache during
           planning: every package needs to be classified as [Source]
           so [Recipe_emitter.emit] materialises an archive for it.
           Otherwise bake against a fully-warm cache produces an empty
           recipe and writes nothing. *)
        let plan_d10 = if emit_recipe = None then Some d10 else None in
        try Plan.of_solution ctx ?d10:plan_d10 ~packages_dirs pkgs
        with Plan.Cycle cycles ->
          Error.config_error
            "dependency cycle in solved packages:@\n\
             %a@\n\
             This is an upstream metadata bug — the cyclic packages declare \
             mutually-equivalent [{= version}] dependencies. Run [oi build \
             --all] to skip just the offending solve group, or fix the \
             offending overlay's opam files."
            Plan.pp_cycles cycles
      in
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
        | _ when emit_recipe <> None ->
            (* emit-only path: don't pull pre-built binary layers from
               the registry. The emitter produces archives for every
               Source package; any layer that isn't already locally
               cached as Binary becomes a Source node in the recipe.
               The recipe is then self-contained against whatever is
               already in the local d10 store, with no implicit
               registry dep. *)
            build_plan
        | None -> build_plan
        | Some _ ->
            phase Fetching "Checking registry for prebuilt layers";
            fetch_remote_layers ~reporter ?jobs ~session ~layer_remote ~d10
              ~packages_dirs ~ctx ~pkgs build_plan
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
        | Some k -> Solver.Memo.store_layers ~fs ~cache_root ~key:k hashes
      in
      if all_layers_cached && emit_recipe = None then begin
        Logs.info (fun m -> m "Layers cached, skipping build");
        persist_layer_cache ();
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
          Plan.elaborate ctx ~packages_dirs ~cache_root ~os_key
            ~ocaml_version:conf.ocaml_version ~reporter build_plan
        in
        (* Emit-recipe path: serialise a d10ir recipe and stop, no
           Execute.run, no source-mirror prewarm — archives are already
           materialised inline by [Recipe_emitter.emit] via
           [Archive_builder.build] which goes through the standard
           [Source.Mirror] cache. *)
        (match emit_recipe with
        | Some out_dir ->
            let d10 =
              make_d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key
            in
            let toolchain_name =
              match toolchain with
              | Some (i : Toolchain.info) -> i.handle
              | None -> "system"
            in
            let toolchain_layer =
              match exec_plan.packages with
              | p :: _ -> p.layer_hash
              | [] -> ""
            in
            let cache_urls = cache_urls ~cache ~source_remote in
            let recipe =
              Recipe_emitter.emit ~proc_mgr ~fs ~d10 ~cache_urls
                ~cli_invocation:(Array.to_list Sys.argv)
                ~toolchain_name ~toolchain_layer exec_plan
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
              Eio.Path.(fs / out_dir);
            let out_recipe = Filename.concat out_dir "recipe.json" in
            D10ir.Plan.save Eio.Path.(fs / out_recipe) recipe;
            Say.ok "emitted recipe to %s" out_recipe;
            Say.info "%d nodes, %d archives at %s/d10ir/archives/"
              (List.length recipe.nodes)
              (List.length recipe.nodes)
              (Eio.Path.native_exn d10.root);
            persist_layer_cache ();
            hashes
        | None ->
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
                Source.Mirror.fetch_from_registry ~reporter
                  ~clock:(clock :> _ Eio.Time.clock_ty Eio.Resource.t)
                  ~session ~fs ~cache_root ~registry
                  ~max_fibers:(fetch_parallelism ?jobs ())
                  archives
              in
              Logs.info (fun m ->
                  m "Mirror prewarm: %d new, %d cached, %d failed" s.fetched
                    s.cached s.failed)
            end
        | _ -> ());
        let urls = cache_urls ~cache ~source_remote in
        let _ = urls in
        (* d10ir Direct executor:
           - Recipe_emitter.emit walks exec_plan, materialises a
             consolidated source archive per Source package via
             Archive_builder, and produces a D10ir.Plan.t.
           - Direct.run schedules every node on an Eio fiber pool,
             waiting on dep promises, restoring transitive dep layers
             into a per-node staging dir, running the (sh-escaped,
             prefix-rebased) build script, and storing the result back
             into d10. Cached layers (Binary in plan, restored from
             registry above) short-circuit. *)
        let toolchain_name =
          match toolchain with Some i -> i.handle | None -> "system"
        in
        let toolchain_layer =
          match exec_plan.packages with
          | p :: _ -> p.layer_hash
          | [] -> ""
        in
        let recipe =
          Recipe_emitter.emit ~proc_mgr ~fs ~d10 ~cache_urls:urls
            ~cli_invocation:(Array.to_list Sys.argv)
            ~toolchain_name ~toolchain_layer exec_plan
        in
        (match save_recipe with
        | None -> ()
        | Some dir ->
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
              Eio.Path.(fs / dir);
            let stem =
              match names with
              | [] -> "recipe"
              | n :: _ -> OpamPackage.Name.to_string n
            in
            let dst = Filename.concat dir (stem ^ ".d10ir.json") in
            D10ir.Plan.save Eio.Path.(fs / dst) recipe;
            Logs.info (fun m -> m "Saved d10ir recipe to %s" dst));
        reporter.event (Plan_ready recipe);
        (* Pre-fetch every consolidated source archive the recipe
           references. By the time [D10ir.Direct.run] takes over, all
           archives must be present locally — Direct itself does no
           network I/O. Resolution order per archive:

           1. Already in [<cache>/d10ir/archives/<sha>.tar.zst]? Done.
           2. Configured archive registry has it
              (HTTP GET [<registry>/d10ir-archives/<sha>.tar.zst])?
              Pull, verify sha, install.
           3. Fall back to inline bake — find the package_plan that
              produced this sha, run [Archive_builder.build_no_solve]
              against its opam file. This makes fresh clones work
              without an explicit pre-bump as long as upstream URLs
              are reachable. *)
        let cache_root_str = Cache.root_s cache in
        let archive_dir_for sha =
          Filename.concat
            (Filename.concat
               (Filename.concat cache_root_str "d10ir")
               "archives")
            (sha ^ ".tar.zst")
        in
        let missing_locally =
          List.filter
            (fun (n : D10ir.Plan.node) ->
              not (Sys.file_exists (archive_dir_for n.archive.sha256)))
            recipe.nodes
        in
        if missing_locally <> [] then begin
          (* Step 2: try the registry for whatever it has. *)
          let after_registry =
            match source_remote with
            | Some (`Http_remote registry) ->
                Logs.info (fun m ->
                    m "fetching %d archive(s) from %s/d10ir-archives/"
                      (List.length missing_locally) registry);
                let shas =
                  List.map
                    (fun (n : D10ir.Plan.node) -> n.archive.sha256)
                    missing_locally
                in
                let s =
                  D10ir.Registry.prefetch
                    ~clock:(clock :> _ Eio.Time.clock_ty Eio.Resource.t)
                    ~fs ~session ~cache_root:cache_root_str
                    ~remote:(`Http_remote registry)
                    ~max_fibers:(fetch_parallelism ?jobs ()) shas
                in
                Logs.info (fun m ->
                    m "archive prefetch: %d new, %d cached, %d failed"
                      s.fetched s.cached s.failed);
                let still_missing = ref [] in
                List.iter
                  (fun (n : D10ir.Plan.node) ->
                    if not (Sys.file_exists (archive_dir_for n.archive.sha256))
                    then still_missing := n :: !still_missing)
                  missing_locally;
                List.rev !still_missing
            | None -> missing_locally
          in
          (* Step 3: inline-bake any archive the registry doesn't
             have. We look up the [package_plan] on [exec_plan] (a
             [Plan.t]) by name, since [Recipe_emitter.emit] consumed
             the same exec_plan and the [archive.sha256] in the
             recipe came from its [d10_archive] field. *)
          if after_registry <> [] then begin
            let plan_by_pkg = Hashtbl.create 32 in
            List.iter
              (fun (p : Plan.package_plan) -> Hashtbl.replace plan_by_pkg p.pkg p)
              exec_plan.packages;
            let baked = ref 0 in
            let bake_failed = ref [] in
            List.iter
              (fun (n : D10ir.Plan.node) ->
                let pkg_str =
                  Fmt.str "%s.%s" n.package.name n.package.version
                in
                match Hashtbl.find_opt plan_by_pkg pkg_str with
                | None ->
                    bake_failed :=
                      (pkg_str, "no matching package_plan") :: !bake_failed
                | Some p -> (
                    try
                      Logs.info (fun m ->
                          m "inline bake fallback for %s" pkg_str);
                      let _ : Archive_builder.built =
                        Archive_builder.build ~reporter ~proc_mgr ~fs ~d10
                          ~cache_root:cache_root_str ~cache_urls:urls p
                      in
                      incr baked
                    with exn ->
                      bake_failed :=
                        (pkg_str, Printexc.to_string exn) :: !bake_failed))
              after_registry;
            if !bake_failed <> [] then
              Error.config_error
                "d10ir build needs %d archive(s) the registry doesn't \
                 have and inline bake failed for them:@\n  %s"
                (List.length !bake_failed)
                (String.concat "\n  "
                   (List.map (fun (p, e) -> Fmt.str "%s: %s" p e)
                      !bake_failed));
            Logs.info (fun m -> m "inline bake completed: %d archive(s)" !baked)
          end
        end;
        let direct_cfg =
          let base = D10ir.Config.default in
          let base = D10ir.Config.with_env_overrides base in
          {
            base with
            build_parallelism =
              (match jobs with Some j -> j | None -> base.build_parallelism);
          }
        in
        let plan_dir = Eio.Path.native_exn d10.root in
        (* d10ir.Direct emits typed [Direct.event]s; we wrap them as
           [Build_progress.Build _] events through the caller's
           reporter. No bar UI here — the cmdliner layer renders. *)
        phase Building "Running d10ir build";
        let direct_reporter : D10ir.Direct.reporter =
          { event = (fun e -> reporter.event (Build e)) }
        in
        let result =
          D10ir.Direct.run ~config:direct_cfg ~d10 ~fs ~proc_mgr
            ~clock:(clock :> D10.Config.clk)
            ~reporter:direct_reporter ~plan_dir recipe
        in
        reporter.event (Build_summary result);
        phase_done Building;
        if result.failed > 0 then begin
          (* Stuff the structured failure summary into the [output]
             field. [Error.pp] prints it as part of the [error:]
             message — i.e. AFTER the bar has been finalised by the
             enclosing [Fun.protect], avoiding a mid-bar interject /
             print / resume that left the bar visible above the
             summary in the terminal. *)
          let pkg_summary =
            match result.failures with
            | [ f ] ->
                Fmt.str "package %s.%s in phase %s" f.package.name
                  f.package.version
                  (D10ir.Direct.phase_to_string f.phase)
            | fs -> Fmt.str "%d packages" (List.length fs)
          in
          let output =
            Fmt.str "%a@,@,%d built, %d cached, %d skipped"
              D10ir.Direct.pp_failures result.failures result.built
              result.cached result.skipped
          in
          Error.build_failed ~pkg:pkg_summary ~cmd:"d10ir.direct" ~output
        end;
        let prefix_hash = D10.Prefix.solve_hash hashes in
        let prefix_dir =
          Eio.Path.native_exn Eio.Path.(d10.root / "prefixes" / prefix_hash)
        in
        (try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix_dir)
         with _ -> ());
        persist_layer_cache ();
        hashes)
      end

let assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes =
  let d10 = make_d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key in
  D10.Prefix.assemble_cached d10 ~layer_hashes
