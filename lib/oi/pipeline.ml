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

let cache_urls ~cache ~remote =
  let local = Source.Mirror.url ~cache in
  match remote with
  | Some (`Http_remote r) -> [ local; Source.Mirror.remote_url ~registry:r ]
  | None | Some _ -> [ local ]

let fetch_parallelism ?jobs () =
  match jobs with
  | Some n when n > 0 -> n
  | _ -> (
      match Sys.getenv_opt "OI_BUILD_PARALLELISM" with
      | Some s -> (
          match int_of_string_opt s with Some n when n > 0 -> n | _ -> 4)
      | None -> min (Domain.recommended_domain_count ()) 4)

let fetch_remote_layers ?jobs ~remote ~d10 ~packages_dirs ~ctx ~pkgs build_plan
    =
  match remote with
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
      else begin
        let index = D10.Layer.fetch_remote_index d10 ~remote:r in
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
          Logs.info (fun m ->
              m "Fetching %d layer(s) from registry (%d needed)..."
                (List.length available)
                (List.length source_hashes));
          Eio.Fiber.List.iter
            ~max_fibers:(fetch_parallelism ?jobs ())
            (fun hash ->
              let sha256 =
                Option.map
                  (fun (e : D10.Layer.index_entry) -> e.sha256)
                  (Hashtbl.find_opt index hash)
              in
              if D10.Layer.pull_remote d10 ~remote:r ~hash ?sha256 () then
                Logs.info (fun m -> m "Fetched %s from registry" hash))
            available;
          Plan.build ctx ~d10 ~packages_dirs pkgs
        end
      end

(* -- Central build pipeline (was main.ml's solve_and_ensure_layers) ----- *)

let build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf ~os_key
    ?(dry_run = false) ?(extra_repos = []) ?(pins = []) ?(refresh = false)
    ?remote ?jobs ?toolchain ?(constraints = OpamPackage.Name.Map.empty)
    ?project_root ?local_packages_dir names =
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
  match fast_hashes with
  | Some hashes ->
      Logs.info (fun m ->
          m "layer cache hit %s (%d layers), skipping solve"
            (String.sub (Stdlib.Option.get layer_cache_key) 0 12)
            (List.length hashes));
      hashes
  | None ->
      let build_prefix = cache_root / "build" / "prefix" in
      let ctx =
        Solver.Ctx.create ~prefix:build_prefix ~packages_dirs ~conf
          ?toolchain:toolchain_ctx ()
      in
      let pkgs =
        match
          Solver.solve ~fs ~cache_root ctx ~packages_dirs ~constraints names
        with
        | Ok pkgs -> pkgs
        | Error msg -> Error.no_solution msg
      in
      let build_plan = Plan.build ctx ~d10 ~packages_dirs pkgs in
      if dry_run then begin
        let remote_has =
          match remote with
          | Some r ->
              let idx = D10.Layer.fetch_remote_index d10 ~remote:r in
              fun h -> Hashtbl.mem idx h
          | None -> fun _ -> false
        in
        Fmt.pr "%a@." (Plan.pp_tree ~remote_has) build_plan;
        exit 0
      end;
      let build_plan =
        fetch_remote_layers ?jobs ~remote ~d10 ~packages_dirs ~ctx ~pkgs
          build_plan
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
              "depext check: %d source pkg(s); platform os=%s os-distribution=%s \
               os-family=%s"
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
          (* Defensive nudge for the case where source packages exist but
             declare no depexts active for this platform. Common on macOS
             when the package's opam file only declares Linux distros, or
             when the homebrew filter clause is missing. The build may
             still need system libraries (gmp, openssl, pkg-config, …);
             we emit a one-liner so the user isn't blindsided by a
             low-level [./configure] failure with no prior warning. *)
          if OpamSysPkg.Set.is_empty all && conf.os = "macos" then
            Fmt.epr
              "%a no system depexts matched for %d source package(s) on \
               macOS. If the build fails with missing headers (gmp.h, \
               openssl/ssl.h, …), install them with %a.@."
              Style.warn_string "note:"
              (List.length source_pkgs)
              Style.accent_string "brew install <pkg>";
          if not (OpamSysPkg.Set.is_empty all) then (
            let st = Depexts.status all in
            Log.info (fun m ->
                m "depext status: installed=%d missing=%d not_found=%d"
                  (OpamSysPkg.Set.cardinal st.installed)
                  (OpamSysPkg.Set.cardinal st.missing)
                  (OpamSysPkg.Set.cardinal st.not_found));
            if not (OpamSysPkg.Set.is_empty st.missing) then begin
              Fmt.epr
                "%a system packages are not installed. The build may fail at \
                 compile time. Install them with:@.  %s@."
                Style.warn_string "warning:"
                (st.missing |> OpamSysPkg.Set.elements
                |> List.map OpamSysPkg.to_string
                |> String.concat " ")
            end;
            if not (OpamSysPkg.Set.is_empty st.not_found) then
              Fmt.epr
                "%a system packages are not known to the host package manager: \
                 %s@."
                Style.warn_string "warning:"
                (st.not_found |> OpamSysPkg.Set.elements
                |> List.map OpamSysPkg.to_string
                |> String.concat ", "));
          (* Force the warnings out to the terminal *before* Plan.resolve
             takes a few seconds and the Execute.run progress bar takes
             over the cursor. Format's [@.] does flush, but we belt-and-
             braces it here because [Ui] uses ANSI cursor moves that have
             been observed to interleave oddly with buffered stderr in
             some macOS terminals. *)
          (try Stdlib.flush stderr with _ -> ())
        end;
        let exec_plan =
          Plan.resolve ctx ~packages_dirs ~cache_root ~os_key
            ~ocaml_version:conf.ocaml_version build_plan
        in
        let urls = cache_urls ~cache ~remote in
        Execute.run ~cache_urls:urls ~proc_mgr ~fs ?jobs
          ~clock:(clock :> D10.Config.clk)
          ~sys ~os_key exec_plan;
        let prefix_hash = D10.Prefix.solve_hash hashes in
        let prefix_dir =
          Eio.Path.native_exn Eio.Path.(d10.root / "prefixes" / prefix_hash)
        in
        (try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix_dir)
         with _ -> ());
        persist_layer_cache ();
        hashes
      end

let assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes =
  let d10 = make_d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key in
  D10.Prefix.assemble_cached d10 ~layer_hashes
