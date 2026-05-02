open Cmdliner

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.cmd.build"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* End-of-run per-target summary table. One row per CLI target, in the
   order requested. Targets that share a solve group repeat the
   group-level counts. *)
let print_build_summary ~targets ~target_handle ~solve_failures ~target_group
    ~group_results =
  let module R = struct
    type t =
      | Skipped of string * string (* solver failure + log path *)
      | Ok of int * int * int
      | Failed of int * int * int * string * (string * string) list
      | Depext_fail of
          int
          * int
          * int
          * OpamSysPkg.Set.t
          * (string * OpamSysPkg.Set.t) list
          * string
  end in
  let result_for t =
    match Hashtbl.find_opt solve_failures t with
    | Some (msg, log_path) -> R.Skipped (msg, log_path)
    | None -> (
        match Hashtbl.find_opt target_group t with
        | None -> R.Skipped ("unknown", "")
        | Some gi -> (
            match Hashtbl.find_opt group_results gi with
            | None -> R.Skipped ("group not built", "")
            | Some (`Ok (p, b, c)) -> R.Ok (p, b, c)
            | Some (`Fail (p, b, c, msg, failures)) ->
                R.Failed (p, b, c, msg, failures)
            | Some (`Depext_fail (p, b, c, missing, per_pkg, log)) ->
                R.Depext_fail (p, b, c, missing, per_pkg, log)))
  in
  let handle_for t =
    match Hashtbl.find_opt target_handle t with Some h -> "@" ^ h | None -> ""
  in
  let rows = List.map (fun t -> (t, handle_for t, result_for t)) targets in
  let n_ok, n_failed, n_depext, n_skipped =
    List.fold_left
      (fun (o, f, d, s) (_, _, r) ->
        match r with
        | R.Ok _ -> (o + 1, f, d, s)
        | R.Failed _ -> (o, f + 1, d, s)
        | R.Depext_fail _ -> (o, f, d + 1, s)
        | R.Skipped _ -> (o, f, d, s + 1))
      (0, 0, 0, 0) rows
  in
  let status_col r =
    match r with
    | R.Ok _ -> Fmt.str "%a" Oi.Style.ok_string "ok"
    | R.Failed _ -> Fmt.str "%a" Oi.Style.error_string "fail"
    | R.Depext_fail _ -> Fmt.str "%a" Oi.Style.warn_string "depext-fail"
    | R.Skipped _ -> Fmt.str "%a" Oi.Style.warn_string "skip"
  in
  (* Shorten multi-line solver diagnostics to the first line so the
     table stays readable. Full detail is still logged per-target
     below. *)
  let first_line s =
    match String.split_on_char '\n' s with [] -> "" | h :: _ -> h
  in
  let detail_col r =
    match r with
    | R.Ok (p, b, c) -> Fmt.str "%d pkg (%d built, %d cached)" p b c
    | R.Failed (p, b, c, _, _) ->
        Fmt.str "%d pkg (%d built, %d cached), build failed" p b c
    | R.Depext_fail (p, b, c, missing, _, _) ->
        Fmt.str "%d pkg (%d cached, %d need build), missing system pkgs: %s" p c
          b
          (missing |> OpamSysPkg.Set.elements
          |> List.map OpamSysPkg.to_string
          |> String.concat " ")
    | R.Skipped (msg, _) -> Fmt.str "skipped (%s)" (first_line msg)
  in
  let target_width =
    List.fold_left (fun w (t, _, _) -> max w (String.length t)) 12 rows
  in
  let handle_width =
    List.fold_left (fun w (_, h, _) -> max w (String.length h)) 0 rows
  in
  let styled_handle h =
    if h = "" then String.make handle_width ' '
    else
      (* [Fmt.str] with styling inflates the visible length with ANSI
         codes; pad first, then colour. *)
      let padded = Fmt.str "%-*s" handle_width h in
      Fmt.str "%a" Oi.Style.info_string padded
  in
  Oi.Say.newline ();
  Oi.Say.header "Build summary";
  List.iter
    (fun (target, handle, r) ->
      if handle_width = 0 then
        Fmt.pr "  %-6s %-*s  %s@." (status_col r) target_width target
          (detail_col r)
      else
        Fmt.pr "  %-6s %s  %-*s  %s@." (status_col r) (styled_handle handle)
          target_width target (detail_col r);
      match r with
      | R.Failed (_, _, _, _, failures) ->
          List.iter
            (fun (pkg, log_path) ->
              Fmt.pr "         %a %s: %s@." Oi.Style.dim_string "↳ log" pkg
                log_path)
            failures
      | R.Depext_fail (_, _, _, _, per_pkg, log_path) ->
          List.iter
            (fun (pkg, set) ->
              Fmt.pr "         %a %s: %s@." Oi.Style.dim_string "↳ needs" pkg
                (set |> OpamSysPkg.Set.elements
                |> List.map OpamSysPkg.to_string
                |> String.concat " "))
            per_pkg;
          Fmt.pr "         %a %s@." Oi.Style.dim_string "↳ depext log:" log_path
      | R.Skipped (_, log_path) when log_path <> "" ->
          Fmt.pr "         %a %s@." Oi.Style.dim_string "↳ solver log:" log_path
      | _ -> ())
    rows;
  Oi.Say.newline ();
  Fmt.pr "  %a %d  %a %d  %a %d  %a %d@." Oi.Style.ok_string "ok" n_ok
    Oi.Style.error_string "failed" n_failed Oi.Style.warn_string "depext-fail"
    n_depext Oi.Style.warn_string "skipped" n_skipped;
  (* Dump per-target build-failure output at debug level so `-v` still
     shows the reason, without dumping a compiler transcript by
     default. *)
  List.iter
    (fun (target, _handle, r) ->
      match r with
      | R.Failed (_, _, _, msg, _) -> Log.info (fun m -> m "%s: %s" target msg)
      | R.Skipped (msg, _) when String.contains msg '\n' ->
          Log.info (fun m -> m "%s: %s" target msg)
      | _ -> ())
    rows

(* -- Overlay-wide depext helpers ---------------------------------------- *)

(* Walk the reporepo and return the list of [(handle, root_groups)]
   pairs that should drive depext computation for [oi docker --all].
   Excludes the [default] handle (the full opam-repository), toolchain
   definitions ([x-oi-toolchain-name] set — they're metadata views,
   not buildable), and any overlay without [x-root-packages]. *)
let overlay_root_targets reporepo_entries =
  reporepo_entries
  |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
  |> List.sort_uniq String.compare
  |> List.filter_map (fun h ->
      if h = "default" then None
      else
        match Oi.Source.Reporepo.latest reporepo_entries ~handle:h with
        | Some e when e.toolchain_name = None && e.root_packages <> [] ->
            Some (h, e.root_packages)
        | _ -> None)

(* Resolve the effective [packages_dirs] for a single overlay handle:
   its own materialised v1/ tree plus every overlay it depends on (via
   the reporepo's [depends:] entries), followed by the base
   opam/relocatable trees. First-wins ordering. *)
let packages_dirs_for_overlay ~base_packages_dirs ~reporepo_entries handle =
  let roots = [ { Oi.Source.Reporepo.handle; version = None } ] in
  let transitive =
    try Oi.Source.Reporepo.resolve reporepo_entries ~roots |> List.rev
    with Oi.Error.E _ -> []
  in
  let reporepo_path = Terms.reporepo_path () in
  let overlay_dirs =
    List.filter_map
      (fun (e : Oi.Source.Reporepo.entry) ->
        if e.url = "" then None
        else
          Some
            (Oi.Source.Reporepo.assert_overlay_dir ~path:reporepo_path
               ~handle:e.handle))
      transitive
  in
  let seen = Hashtbl.create 8 in
  let dedup xs =
    List.filter
      (fun d ->
        if Hashtbl.mem seen d then false
        else begin
          Hashtbl.replace seen d ();
          true
        end)
      xs
  in
  dedup (overlay_dirs @ base_packages_dirs)

(* Solve every overlay's [x-root-packages] under [host_conf] and return
   the resulting [(pkg_dirs, solved)] pairs. Solves happen once and are
   shared across every per-platform depext evaluation, since every
   target only differs in opam filter variables (os, os-family, …) —
   those don't influence the solver picks here. *)
let solve_overlay_root_groups ~fs ~sys ~cache ~data_dir ~refresh ~host_conf
    ?override ?handle_filter () =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  let base_packages_dirs =
    Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ()
  in
  let path = Terms.reporepo_path () in
  Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh ~path
    ~url:(Terms.reporepo_url ());
  let reporepo_entries = try Oi.Source.Reporepo.load ~path with _ -> [] in
  let targets =
    let all = overlay_root_targets reporepo_entries in
    match handle_filter with
    | None -> all
    | Some h -> List.filter (fun (handle, _) -> handle = h) all
  in
  let cache_root = Oi.Cache.root_s cache in
  let build_prefix = cache_root / "build" / "prefix" in
  let toolchain_for handle =
    Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf:host_conf
      ~install:false ~override ~handles:[ handle ] ()
  in
  List.concat_map
    (fun (handle, groups) ->
      let pkg_dirs =
        packages_dirs_for_overlay ~base_packages_dirs ~reporepo_entries handle
      in
      let toolchain = toolchain_for handle in
      let conf, tc_ctx = Oi.Pipeline.toolchain_views toolchain host_conf in
      List.filter_map
        (fun group ->
          let ctx =
            Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkg_dirs
              ~conf ?toolchain:tc_ctx ()
          in
          let items = List.map Target.parse_pkg_target group in
          let names = List.map fst items in
          let constraints =
            List.fold_left
              (fun acc (name, c) ->
                match c with
                | None -> acc
                | Some c -> OpamPackage.Name.Map.add name c acc)
              OpamPackage.Name.Map.empty items
          in
          match
            Oi.Solver.solve ~fs ~cache_root ctx ~packages_dirs:pkg_dirs
              ~constraints names
          with
          | Ok solved -> Some (pkg_dirs, solved)
          | Error msg ->
              Log.warn (fun m ->
                  m "overlay depexts: %s group failed to solve: %s" handle msg);
              None)
        groups)
    targets

let depexts_union ~conf solves =
  let all =
    List.fold_left
      (fun acc (pkg_dirs, solved) ->
        let entries =
          Oi.Depexts.compute_for_conf ~conf ~packages_dirs:pkg_dirs solved
        in
        List.fold_left
          (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
          acc entries)
      OpamSysPkg.Set.empty solves
  in
  OpamSysPkg.Set.elements all |> List.map OpamSysPkg.to_string

let compute_overlay_depexts_for_conf ~fs ~sys ~cache ~data_dir ~refresh ~conf
    ?override ?handle () =
  let solves =
    solve_overlay_root_groups ~fs ~sys ~cache ~data_dir ~refresh ~host_conf:conf
      ?override ?handle_filter:handle ()
  in
  depexts_union ~conf solves

let compute_overlay_depexts_per_distro ~fs ~sys ~cache ~data_dir ~refresh
    ~platform ~distros =
  let host_conf =
    Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let solves =
    solve_overlay_root_groups ~fs ~sys ~cache ~data_dir ~refresh ~host_conf ()
  in
  List.map
    (fun distro ->
      let vars = Registry_docker.opam_vars_of_distro distro in
      let distro_conf =
        {
          host_conf with
          os = "linux";
          os_distribution = vars.os_distribution;
          os_family = vars.os_family;
          os_version = vars.os_version;
        }
      in
      (distro, depexts_union ~conf:distro_conf solves))
    distros

(* -- Single-target test mode ------------------------------------------- *)

(* Locate the layer whose package name matches [pkg_name]. The dotted
   prefix match keys on [name + "."], so the exact version found by the
   solver doesn't have to be hardcoded. *)
let find_target_layer ~fs ~cache ~os_key ~pkg_name layer_hashes =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let prefix = pkg_name ^ "." in
  List.find_map
    (fun h ->
      match
        D10.Layer.load_meta Eio.Path.(fs / layers_dir / h / "layer.json")
      with
      | Some (m : D10.Layer.meta)
        when String.length m.package >= String.length prefix
             && String.sub m.package 0 (String.length prefix) = prefix ->
          Some (h, m.package)
      | _ -> None)
    layer_hashes

(* Build [target]'s closure, then run [dune runtest --profile=release]
   inside the target's persisted build dir against the assembled
   consumer prefix. Backs [oi build PKG --test] / [oi build @h/PKG
   --test]. *)
let run_target_test ~target ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache
    ~data_dir ~registry ?(refresh = false) ?(with_repos = []) ?(with_deps = [])
    ?jobs ?toolchain ?(dry_run = false) () =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let conf =
    Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let remote = Terms.remote_of_registry registry in
  let target_display = target in
  let target, with_repos, with_deps, target_pin =
    match Target.split_handle_prefix target with
    | None -> (target, with_repos, with_deps, None)
    | Some (h, pkg_spec) ->
        let pkg, user_constr = OpamFormula.atom_of_string pkg_spec in
        let pin = { Target.handle = h; pkg; user_constr } in
        ( OpamPackage.Name.to_string pkg,
          with_repos @ [ h ],
          with_deps @ [ pkg_spec ],
          Some pin )
  in
  let with_deps, with_repos, with_pins =
    Target.extract_handle_pins ~with_repos with_deps
  in
  let extra_deps, url_project =
    Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
  in
  let handle_pins = Stdlib.Option.to_list target_pin @ with_pins in
  let tc_handles =
    Target.pin_handles handle_pins
    @ Target.handles_of_tokens with_repos
    @ url_project.overlays
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:true
      ~override:toolchain ~handles:tc_handles ()
  in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
  let all_extras =
    Target.merge_extras ~cli:cli_extras ~project:url_project.extra_repos
  in
  let handle_constraints =
    Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
  in
  let extra_constraints =
    OpamPackage.Name.Map.union
      (fun a _ -> a)
      handle_constraints
      (Oi.Project.Script.constraints extra_deps)
  in
  let pkg_name = OpamPackage.Name.of_string target in
  let names =
    [ pkg_name ]
    |> Oi.Pipeline.drop_override_compiler_roots ~override:None ~toolchain
  in
  if dry_run then begin
    Fmt.pr
      "@[<v>%a@,\
       @,\
      \  oi build %s@,\
      \  cd <build_dir>@,\
      \  dune runtest --profile=release@,\
       @]@."
      Oi.Style.header_string "Would run:" target_display;
    0
  end
  else begin
    let layer_hashes =
      let on_phase msg = Oi.Say.step "%s" msg in
      let on_progress = Oi.Say.progress in
      Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf ~os_key
        ~extra_repos:all_extras ~pins:url_project.pins ~refresh
        ~constraints:extra_constraints ?remote ?jobs ?toolchain
        ?local_packages_dir:url_project.packages_dir ~on_phase ~on_progress
        names
    in
    match
      find_target_layer ~fs ~cache ~os_key ~pkg_name:target layer_hashes
    with
    | None ->
        Oi.Error.not_found target
          "no built layer matched %s; the solve may have substituted a \
           different package."
          target
    | Some (layer_hash, pkg_full) ->
        let short =
          String.sub layer_hash 0 (min 12 (String.length layer_hash))
        in
        let build_dir =
          Oi.Cache.root_s cache / "build" / "_build" / (pkg_full ^ "-" ^ short)
        in
        if not (Sys.file_exists build_dir) then
          Oi.Error.not_found target
            "build dir %s missing (layer was cached without preserved source). \
             Pass --refresh to rebuild from source."
            build_dir;
        if not (Sys.file_exists (build_dir / "dune-project")) then
          Oi.Error.config_error
            "%s has no dune-project; native opam test commands not yet \
             supported."
            build_dir;
        let prefix =
          Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key
            ~layer_hashes
        in
        let dune_cache_root = Oi.Cache.dune_root cache in
        let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info toolchain in
        let env =
          Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
        in
        Fmt.pr "@.%a %s@.%a %s@." Oi.Style.header_string "Testing" pkg_full
          Oi.Style.dim_string "→" build_dir;
        let cmd = Fmt.str "cd %s && dune runtest --profile=release" build_dir in
        let ec = Subprocess.run proc_mgr ~env [ "/bin/sh"; "-c"; cmd ] in
        if ec <> 0 then begin
          Fmt.epr "%a (dune runtest exit %d)@." Oi.Style.error_string
            "Test failed" ec;
          ec
        end
        else begin
          Fmt.pr "%a@." Oi.Style.ok_string "Test successful";
          0
        end
  end

(* -- oi build dispatcher ------------------------------------------------ *)

let cmd =
  let run () data_dir cache_dir refresh skip_local dry_run all only skip
      registry with_repos with_deps jobs toolchain_override depext_only export
      envrc_mode deps_only targets =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache } =
      Harness.bootstrap env cache_dir
    in
    (* Project mode: no positional, no --all, *.opam present in cwd.
       [--skip-local] forces non-project mode regardless. *)
    let cwd_s, _ = Workspace.resolved_cwd fs in
    let project_mode =
      (not skip_local) && targets = [] && (not all)
      &&
        try
          Sys.readdir cwd_s
          |> Array.exists (fun f ->
              Filename.check_suffix f ".opam"
              && Filename.chop_suffix f ".opam" <> "")
        with Sys_error _ -> false
    in
    let needs_spec what =
      Oi.Error.config_error
        "oi build %s: no spec and no project (cwd has no *.opam). Pass a PKG, \
         @HANDLE, or --all."
        what
    in
    (* Flag validation. Mode-specific dispatch happens after this
       block; here we only reject combinations that can't possibly
       proceed: [--export] + [--depext] together, and any flag whose
       result depends on solving when there's nothing to solve. *)
    if export <> None && depext_only then
      Oi.Error.config_error
        "oi build: --export and --depext are mutually exclusive";
    if deps_only && not project_mode then
      Oi.Error.config_error
        "oi build --deps-only: only valid in project mode (cwd has no *.opam, \
         or a PKG / @HANDLE / --all was given)";
    let no_spec = targets = [] && (not all) && not project_mode in
    if no_spec && export <> None then needs_spec "--export";
    if no_spec && depext_only then needs_spec "--depext";
    if depext_only && all then begin
      let conf =
        Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
      in
      let names =
        compute_overlay_depexts_for_conf ~fs ~sys ~cache ~data_dir ~refresh
          ~conf ?override:toolchain_override ()
      in
      List.iter (fun n -> Fmt.pr "%s@." n) names;
      exit 0
    end;
    let do_export_if_set ?(ok = true) () =
      match export with
      | Some output when ok && not dry_run ->
          Registry_export.do_registry_export ~fs
            ~clock:(clock :> D10.Config.clk)
            ~sys ~os_key ~cache ~registry ~output
      | _ -> ()
    in
    if project_mode then begin
      let ec =
        if depext_only then
          Project_build.depexts ~fs ~sys ~platform ~cache ~data_dir ~refresh
            ~with_repos ~with_deps ?toolchain:toolchain_override ~cwd:cwd_s ()
        else
          let action = if deps_only then `Deps_only else `Build in
          Project_build.run ~action ~fs ~proc_mgr ~clock ~sys ~platform ~os_key
            ~cache ~data_dir ~registry ~refresh ~with_repos ~with_deps ?jobs
            ?toolchain:toolchain_override ~envrc_mode ~dry_run ~cwd:cwd_s ()
      in
      do_export_if_set ~ok:(ec = 0) ();
      exit ec
    end;
    (* Timestamp for filtering stale log files out of the end-of-run
       "transient fetch errors" listing. Any [fetch-*.log] with mtime
       older than this was left over by a previous invocation. *)
    let run_start_time = Unix.time () in
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf =
      Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    let remote = Terms.remote_of_registry registry in
    (* When [--all] is set, walk every overlay in the reporepo and
       derive targets from each one:
       - skip [default] (ocaml/opam-repository) — its ~10k packages
         are never what [--all] should mean;
       - skip toolchain-definition entries ([x-oi-toolchain-name]
         set, url-less) — they're metadata views over other overlays,
         not buildable themselves;
       - if the overlay has [x-root-packages], emit one [@handle/pkg]
         per entry;
       - otherwise fall back to [@handle], which expands to every
         package the overlay's clone ships.
       [--only] restricts to named handles; [--skip] excludes them.
       [default] can still be included by explicitly listing it via
       [--only default]. *)
    let reporepo_target_groups =
      if not all then []
      else begin
        let path = Terms.reporepo_path () in
        Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh ~path
          ~url:(Terms.reporepo_url ());
        let entries = Oi.Source.Reporepo.load ~path in
        let only_set =
          if only = [] then None else Some (List.sort_uniq compare only)
        in
        let skip_set = List.sort_uniq compare skip in
        let handles =
          List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle) entries
          |> List.sort_uniq String.compare
        in
        List.concat_map
          (fun h ->
            let default_skipped =
              h = "default"
              &&
              match only_set with
              | None -> true
              | Some s -> not (List.mem h s)
            in
            if default_skipped then begin
              Log.info (fun m ->
                  m "--all: skipping %s (pass --only default to include)" h);
              []
            end
            else
              let included =
                (match only_set with None -> true | Some s -> List.mem h s)
                && not (List.mem h skip_set)
              in
              if not included then []
              else
                match Oi.Source.Reporepo.latest entries ~handle:h with
                | None -> []
                | Some e when e.toolchain_name <> None ->
                    Log.info (fun m ->
                        m "--all: skipping toolchain definition %s" h);
                    []
                | Some e ->
                    if e.root_packages = [] then begin
                      Log.info (fun m ->
                          m
                            "--all: overlay %s has no x-root-packages, \
                             expanding to every package in the overlay"
                            h);
                      [ [ "@" ^ h ] ]
                    end
                    else
                      List.map
                        (fun group ->
                          List.map (fun p -> "@" ^ h ^ "/" ^ p) group)
                        e.root_packages)
          handles
      end
    in
    (* Each CLI-supplied target is its own (singleton) solve group, so
       [oi build a b] solves [a] and [b] independently. Reporepo groups
       may be multi-element (compiler variants etc.). The tokens are
       still raw — [@handle]-only entries haven't been fanned out to
       the overlay's packages yet (we need the clone first). *)
    let token_groups =
      List.map (fun t -> [ t ]) targets @ reporepo_target_groups
    in
    let tokens = List.concat token_groups in
    if tokens = [] then
      begin if all then
        Oi.Error.config_error
          "--all expanded to nothing in %s (all overlays filtered by \
           --skip/--only, or the reporepo only contains 'default')"
          (Terms.reporepo_path ())
      else
        Oi.Error.config_error
          "no targets to build (pass PKG arguments or --all)"
      end;
    (* Classify each input into a plain target or an overlay form.
       Overlay forms collect handles to thread through [with_repos]
       so the later [Target.cli_extra_repos] run clones them up front. The
       "build everything in this overlay" form is expanded once the
       clones exist. *)
    let parsed = List.map Target.parse_build_target tokens in
    let with_repos =
      let handles =
        List.filter_map
          (function
            | Target.Plain_target _ -> None
            | Target.Overlay_pkg (h, _) | Target.Overlay_all h -> Some h)
          parsed
        |> List.sort_uniq String.compare
      in
      with_repos @ handles
    in
    let extra_cli, url_project =
      Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    (* Split handles into two scopes:
       - [global_handles] apply to every solve (explicit [--with-repo]
         + any URL-project [x-repos] @-handles).
       - [token_handles] come from [@h/pkg] tokens and only apply to
         their group's solve.
       [with_repos] at this point already contains both, so recover
       [global_handles] by subtracting the token-derived set. *)
    let token_handles =
      List.filter_map
        (function
          | Target.Overlay_pkg (h, _) | Target.Overlay_all h -> Some h
          | Target.Plain_target _ -> None)
        parsed
    in
    let global_handles =
      let tokens = List.sort_uniq String.compare token_handles in
      List.filter (fun h -> not (List.mem h tokens)) with_repos
      @ url_project.overlays
    in
    (* Clone every relevant overlay (global + token) upfront so
       per-group resolution below just reads already-materialised
       packages dirs. We don't keep the merged paths list — packages
       dirs are recomputed per-group from the handle subset. *)
    let all_handles =
      List.sort_uniq String.compare (global_handles @ token_handles)
    in
    let cli_extras_records =
      Target.merge_extras
        ~cli:(Target.cli_extra_repos ~fs ~sys all_handles)
        ~project:url_project.extra_repos
    in
    let _ : string list =
      Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh cli_extras_records
    in
    (* URL-project pins materialize into a synthetic packages/ tree
       the solver consumes ahead of everything else, so the URL's
       dev-version of each local package wins over any stable version
       from the opam-repository. *)
    let pin_dir =
      Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh url_project.pins
    in
    (* Expand [@handle] into every package the overlay's clone
       provides. List just the top-level names under the overlay's
       [packages/] dir — the solver will pick specific versions. If
       the overlay was force-bumped to a new version since the last
       [ensure_extra] call (e.g. the user just ran [oi repo add
       --force] pointing at a new URL), clone it on the fly rather
       than fail out. *)
    let overlay_packages handle =
      let path = Terms.reporepo_path () in
      let entries = Oi.Source.Reporepo.load ~path in
      match Oi.Source.Reporepo.latest entries ~handle with
      | None -> Oi.Error.config_error "no overlay %s in reporepo" handle
      | Some e ->
          let pkgs_dir =
            Oi.Source.Reporepo.overlay_packages_dir ~path ~handle:e.handle
          in
          if not (Sys.file_exists pkgs_dir) then
            Oi.Error.config_error
              "overlay %s.%s is not materialised at %s; run 'oi repo bump %s' \
               to populate it (upstream %s)"
              handle e.version pkgs_dir handle e.url;
          Sys.readdir pkgs_dir |> Array.to_list
          |> List.filter (fun n -> Sys.is_directory (pkgs_dir / n))
          |> List.sort String.compare
    in
    (* Remember which handle each target came from so the summary
       table can render a column for it. Targets from plain PKG
       arguments have no handle. If two overlays contribute the same
       bare package name the later one wins for display purposes; the
       solver still sees them as a single target. *)
    let target_handle : (string, string) Hashtbl.t = Hashtbl.create 16 in
    (* Expand each raw group into package-name groups. A group
       containing just an [@handle] fallback (no [x-root-packages])
       fans out into one singleton group per package the overlay
       ships — "build everything in the overlay" isn't a single-solve
       concept. Groups composed of [@handle/pkg] or plain [pkg] tokens
       keep their shape so multi-package solve groups (compiler
       variants) survive intact.

       Each result carries its own [handles] — the set of overlay
       handles that should be visible to the solver for this group.
       [@avsm/karakeep] yields a group with handles [avsm], nothing
       else. That scope is what keeps [@avsm] solves from picking up
       conflicting packages out of [@samoht]'s overlay. *)
    let raw_target_groups :
        (string list (* targets *) * string list (* handles *)) list =
      List.concat_map
        (fun raw_group ->
          match List.map Target.parse_build_target raw_group with
          | [ Overlay_all h ] ->
              let ps = overlay_packages h in
              List.iter (fun p -> Hashtbl.replace target_handle p h) ps;
              Log.info (fun m ->
                  m "Overlay %s: %d package(s) to build" h (List.length ps));
              List.map (fun p -> ([ p ], [ h ])) ps
          | classified ->
              let names =
                List.map
                  (function
                    | Target.Plain_target t -> t
                    | Target.Overlay_pkg (h, pkg_spec) ->
                        Hashtbl.replace target_handle pkg_spec h;
                        pkg_spec
                    | Target.Overlay_all h ->
                        Oi.Error.config_error
                          "@%s cannot appear inside a multi-package solve \
                           group; use @%s/PKG or list packages explicitly"
                          h h)
                  classified
              in
              let handles =
                List.filter_map
                  (function
                    | Target.Plain_target _ -> None
                    | Target.Overlay_pkg (h, _) | Target.Overlay_all h -> Some h)
                  classified
                |> List.sort_uniq String.compare
              in
              [ (names, handles) ])
        token_groups
    in
    let target_groups = raw_target_groups in
    let targets = List.concat_map fst target_groups in
    (* [--dry-run --all] prints the expanded target list (with handles
       and the latest version each overlay ships) and stops before
       solving. Useful to audit what [--all] would attempt without
       paying the solver's cost. Non-[--all] dry-runs keep the existing
       per-group build-plan tree output downstream. *)
    if dry_run && all then begin
      let entries = Oi.Source.Reporepo.load ~path:(Terms.reporepo_path ()) in
      let n = List.length target_groups in
      let n_targets = List.length targets in
      (* Display-only grouping: per-target solves are kept for speed
         and failure isolation (opam-0install scales badly with root
         count), but we bucket the dry-run output by handle signature
         so the reader sees one row per reporepo overlay set, not one
         per root package. *)
      let display_groups =
        let tbl : (string list, string list ref) Hashtbl.t = Hashtbl.create 8 in
        let order = ref [] in
        List.iter
          (fun (targets, handles) ->
            let key = List.sort_uniq String.compare handles in
            match Hashtbl.find_opt tbl key with
            | Some acc -> acc := !acc @ targets
            | None ->
                Hashtbl.add tbl key (ref targets);
                order := key :: !order)
          target_groups;
        List.rev_map
          (fun key ->
            let ts = !(Hashtbl.find tbl key) |> List.sort_uniq String.compare in
            (ts, key))
          !order
      in
      let n_display = List.length display_groups in
      Fmt.pr "@.%a@." Oi.Style.header_string
        (Fmt.str
           "--all would build %d target%s in %d solve group%s (grouped into %d \
            handle scope%s):"
           n_targets
           (if n_targets = 1 then "" else "s")
           n
           (if n = 1 then "" else "s")
           n_display
           (if n_display = 1 then "" else "s"));
      let reporepo_path = Terms.reporepo_path () in
      let handle_dir = Hashtbl.create 8 in
      let dir_for_handle h =
        match Hashtbl.find_opt handle_dir h with
        | Some v -> v
        | None ->
            let v =
              match Oi.Source.Reporepo.latest entries ~handle:h with
              | None -> None
              | Some e when e.url = "" -> None
              | Some _ ->
                  let d =
                    Oi.Source.Reporepo.overlay_packages_dir ~path:reporepo_path
                      ~handle:h
                  in
                  if Sys.file_exists d then Some d else None
            in
            Hashtbl.replace handle_dir h v;
            v
      in
      let bare_name t =
        let stop = [ '='; '<'; '>'; '.'; '{' ] in
        let len = String.length t in
        let rec find i =
          if i >= len then len
          else if List.mem t.[i] stop then i
          else find (i + 1)
        in
        String.sub t 0 (find 0)
      in
      let version_for target handles =
        List.find_map
          (fun h ->
            match dir_for_handle h with
            | Some d ->
                Target.latest_version_in_dirs ~pkg:(bare_name target) [ d ]
            | None -> None)
          handles
      in
      let overlays_summary_for handles =
        let eff =
          List.sort_uniq compare ("relocatable" :: (global_handles @ handles))
        in
        let resolved =
          let roots =
            List.map
              (fun h : Oi.Source.Reporepo.root ->
                { handle = h; version = None })
              eff
          in
          try Oi.Source.Reporepo.resolve entries ~roots
          with Oi.Error.E _ -> []
        in
        String.concat ", "
          (List.map
             (fun (e : Oi.Source.Reporepo.entry) ->
               "@" ^ e.handle ^ "." ^ e.version)
             resolved)
      in
      let group_label handles =
        match handles with
        | [] -> "(no overlay)"
        | hs -> String.concat "+" (List.map (fun h -> "@" ^ h) hs)
      in
      let sort_key (_, handles) = group_label handles in
      let sorted_groups =
        List.sort
          (fun a b -> String.compare (sort_key a) (sort_key b))
          display_groups
      in
      let label_w =
        List.fold_left
          (fun w (_, handles) -> max w (String.length (group_label handles)))
          0 sorted_groups
      in
      List.iter
        (fun (targets, handles) ->
          let label = group_label handles in
          Fmt.pr "@.  %a %-*s  %a@." Oi.Style.info_string "▸" label_w label
            Oi.Style.dim_string
            (overlays_summary_for handles);
          let sorted_targets = List.sort String.compare targets in
          let with_versions =
            List.map
              (fun t ->
                match version_for t handles with
                | Some v -> t ^ "." ^ v
                | None -> t)
              sorted_targets
          in
          Fmt.pr "      %s@." (String.concat ", " with_versions))
        sorted_groups;
      Fmt.pr "@.";
      exit 0
    end;
    if targets = [] && url_project.roots = [] then
      Oi.Error.config_error "no targets to build";
    let cache_root = Oi.Cache.root_s cache in
    let build_prefix = cache_root / "build" / "prefix" in
    let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
    let base_packages_dirs =
      Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ()
    in
    (* Per-group packages_dirs: each solve group sees ONLY the overlay
       handles it actually uses (plus its transitive base deps via
       reporepo [depends:] resolution, plus any globally-scoped handles
       from [--with-repo] / URL-project [x-repos] @-handles). This is what
       keeps [@avsm/karakeep] from accidentally picking up packages
       out of [@samoht]'s overlay — the samoht clone is on disk but
       isn't in this group's solver search path. *)
    let reporepo_entries_cache =
      try Oi.Source.Reporepo.load ~path:(Terms.reporepo_path ()) with _ -> []
    in
    (* Toolchain resolution per group goes through {Pipeline.resolve_toolchain}
       — same precedence rulebook every command shares ([--toolchain=NAME]
       override, then implicit pickup from the group's handles, then the
       reporepo default). The cache memoises the resolved [info] so 30
       [@avsm/...] groups share one [Toolchain.resolve] /
       [ensure_installed] pass. With [--toolchain=NAME] every group
       resolves to the same toolchain regardless of its handles, so all
       groups share one cache slot. *)
    let resolved_toolchains : (string list, Oi.Toolchain.info option) Hashtbl.t
        =
      Hashtbl.create 4
    in
    let toolchain_for_handles handles =
      let key =
        if toolchain_override <> None then []
        else List.sort_uniq String.compare handles
      in
      match Hashtbl.find_opt resolved_toolchains key with
      | Some i -> i
      | None ->
          let info =
            Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:true
              ~override:toolchain_override ~handles:key ()
          in
          Hashtbl.add resolved_toolchains key info;
          info
    in
    let overlay_entries_for_handles handles =
      let roots =
        List.rev handles
        |> List.map (fun h : Oi.Source.Reporepo.root ->
            { handle = h; version = None })
      in
      try
        Oi.Source.Reporepo.resolve reporepo_entries_cache ~roots
        (* [resolve] returns deps-first; reverse so dependents win on
           name collisions under the solver's first-wins fold. *)
        |> List.rev
      with Oi.Error.E _ -> []
    in
    (* Direct (non-transitive) lookup of an explicit handle's latest
       reporepo entry. Used in place of {!overlay_entries_for_handles}
       when a toolchain is active so the consumer solve doesn't end up
       seeing the same opam-repository at two different commits — once
       at the toolchain's pinned commit (via [info.packages_dirs]) and
       once at the overlay's transitively-resolved commit (via
       [Reporepo.resolve]'s closure). With both visible, a 5.5.0
       toolchain would still let the solver pick [ocaml.5.4.1] from
       the overlay's older default-repo pin and the conflict-class
       chain on [ocaml-base-compiler] would explode. *)
    let overlay_entries_direct handles =
      List.filter_map
        (fun h -> Oi.Source.Reporepo.latest reporepo_entries_cache ~handle:h)
        handles
    in
    let packages_dirs_for_handles ?toolchain handles =
      (* Drop globally-scoped overlays whose [x-oi-toolchain] is
         incompatible with this group's toolchain. Per-group token
         handles are kept verbatim — the user named those explicitly
         via [@h/pkg] and expects them in scope regardless. *)
      let global_handles =
        Oi.Pipeline.filter_compatible_overlays
          ~reporepo_path:(Terms.reporepo_path ()) ~toolchain global_handles
      in
      let effective =
        global_handles @ handles |> List.sort_uniq String.compare
      in
      let overlay_entries =
        match (toolchain : Oi.Toolchain.info option) with
        | None -> overlay_entries_for_handles effective
        | Some _ -> overlay_entries_direct effective
      in
      let reporepo_path = Terms.reporepo_path () in
      let overlay_dirs =
        List.map
          (fun (e : Oi.Source.Reporepo.entry) ->
            Oi.Source.Reporepo.assert_overlay_dir ~path:reporepo_path
              ~handle:e.handle)
          overlay_entries
      in
      (* When a toolchain is active, [info.packages_dirs] (the
         toolchain's own clone plus its declared base overlays) takes
         the place of [base_packages_dirs] — same logic as [oi run
         --toolchain]. The toolchain's chosen base set is what should
         be in scope, not the reporepo's default
         [relocatable]/[default] pair (e.g. an [oxcaml] solve must not
         see [relocatable]). *)
      let base =
        match (toolchain : Oi.Toolchain.info option) with
        | None -> base_packages_dirs
        | Some i -> i.packages_dirs
      in
      let seen = Hashtbl.create 8 in
      let dedup xs =
        List.filter
          (fun d ->
            if Hashtbl.mem seen d then false
            else begin
              Hashtbl.replace seen d ();
              true
            end)
          xs
      in
      dedup (Stdlib.Option.to_list pin_dir @ overlay_dirs @ base)
    in
    (* [--with] adds extra packages to every target's root set plus any
       version constraints they carry. *)
    let base_constraints = Oi.Project.Script.constraints extra_cli in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Project.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_cli
      @ List.map OpamPackage.Name.of_string url_project.roots
    in
    let target_groups =
      target_groups @ List.map (fun r -> ([ r ], [])) url_project.roots
    in
    let targets = List.concat_map fst target_groups in
    (* Per-target result tracking; the final summary walks [targets] in
       order and looks each name up here. A target either fails to
       solve (status stored directly), or lands in some group. Groups
       are keyed by index; their build result (ok / failed, with the
       package counts) is written into [group_results] when the group
       finishes. *)
    let solve_failures : (string, string * string) Hashtbl.t =
      Hashtbl.create 16
    in
    (* Dump the solver's diagnostic so the user can grep for conflicting
       version constraints. The file name's short hash comes from the
       (targets, handles) tuple so two solve groups whose first target
       name collides don't clobber each other's logs. *)
    let write_solve_failure_log ~targets ~handles ~msg =
      let key =
        String.concat " " (targets @ List.map (fun h -> "@" ^ h) handles)
      in
      let hash = Digest.to_hex (Digest.string key) in
      let first = match targets with t :: _ -> t | [] -> "solve" in
      let path =
        Oi.Cache.Logs.path ~cache_root ~kind:"solve" ~name:first ~hash
      in
      let handles_str =
        if handles = [] then "(base only)"
        else String.concat ", " (List.map (fun h -> "@" ^ h) handles)
      in
      let trailing_nl =
        if msg = "" || msg.[String.length msg - 1] = '\n' then "" else "\n"
      in
      let body =
        Fmt.str "targets: %s\nhandles: %s\n\n%s%s"
          (String.concat ", " targets)
          handles_str msg trailing_nl
      in
      Oi.Cache.Logs.write ~fs ~cache_root path body;
      path
    in
    let target_group : (string, int) Hashtbl.t = Hashtbl.create 16 in
    let group_results :
        ( int,
          [ `Ok of int * int * int
          | `Fail of int * int * int * string * (string * string) list
          | `Depext_fail of
            int
            * int
            * int
            * OpamSysPkg.Set.t
            * (string * OpamSysPkg.Set.t) list
            * string ] )
        Hashtbl.t =
      Hashtbl.create 16
    in
    (* 1. Solve each solve-group against that group's scoped
       [packages_dirs] — handles from the group's own tokens plus any
       global [--with-repo] ones. Different groups see different
       overlays so [@avsm/...] never solves against [@samoht/...]. *)
    let solutions =
      let n_groups = List.length target_groups in
      let group_label group = String.concat " " group in
      let solve_one (group, handles) =
        let toolchain = toolchain_for_handles handles in
        let pkg_dirs = packages_dirs_for_handles ?toolchain handles in
        let group_conf, tc_ctx = Oi.Pipeline.toolchain_views toolchain conf in
        let gctx =
          Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkg_dirs
            ~conf:group_conf ?toolchain:tc_ctx ()
        in
        (* When [--toolchain=NAME] is explicit, strip any compiler-family
           entries the overlay's [x-root-packages] declares so the
           override's compiler pins land cleanly. Same intent as
           {Pipeline.drop_override_compiler_roots} but applied to the raw
           [pkg] strings (e.g. ["oxcaml-compiler.5.2.0+ox"]) rather than
           parsed [Name.t]s. *)
        let group =
          match (toolchain_override, toolchain) with
          | Some _, Some (info : Oi.Toolchain.info) ->
              List.filter
                (fun pkg ->
                  let name, _ = Target.parse_pkg_target pkg in
                  not (OpamPackage.Name.Set.mem name info.root_names))
                group
          | _ -> group
        in
        if group = [] then begin
          Log.info (fun m ->
              m
                "Skipping solve group: every entry was a root package replaced \
                 by --toolchain");
          None
        end
        else
          let items = List.map Target.parse_pkg_target group in
          let names = List.map fst items in
          let constraints =
            List.fold_left
              (fun acc (name, c) ->
                match c with
                | None -> acc
                | Some c -> OpamPackage.Name.Map.add name c acc)
              base_constraints items
          in
          match
            Oi.Solver.solve ~fs ~cache_root gctx ~packages_dirs:pkg_dirs
              ~constraints (names @ extra_names)
          with
          | Ok pkgs ->
              Log.info (fun m ->
                  let tc_label =
                    match toolchain with
                    | None -> ""
                    | Some i -> Fmt.str " (toolchain %s)" i.handle
                  in
                  m "Solved %s%s: %d packages" (group_label group) tc_label
                    (List.length pkgs));
              Some
                (group, handles, pkg_dirs, pkgs, toolchain, group_conf, tc_ctx)
          | Error msg ->
              let log_path =
                write_solve_failure_log ~targets:group ~handles ~msg
              in
              List.iter
                (fun t -> Hashtbl.replace solve_failures t (msg, log_path))
                group;
              (* Append one Audit event per failed target so the manifest
                 picks them up at export time. The [layer_hash] is a stable
                 digest of the solve key, matching the text log's filename
                 — solve failures don't have a real layer to point at. *)
              let layer_hash =
                let key =
                  String.concat " " (group @ List.map (fun h -> "@" ^ h) handles)
                in
                Digest.to_hex (Digest.string key)
              in
              let tail = Oi.Audit.tail_of_file ~path:log_path in
              let now = Unix.gettimeofday () in
              let context : Oi.Audit.context =
                {
                  (Oi.Audit.default_context ()) with
                  overlay =
                    (match handles with
                    | [ h ] -> Some { D10.Overlay.handle = h; version = "" }
                    | _ -> None);
                  toolchain =
                    Option.map
                      (fun (i : Oi.Toolchain.info) -> i.handle)
                      toolchain;
                }
              in
              List.iter
                (fun target ->
                  let event : Oi.Audit.event =
                    {
                      schema = 1;
                      event_id = Oi.Audit.ulid ();
                      invocation_id = Oi.Audit.invocation_id ();
                      ts = now;
                      os_key;
                      target = Solve_key layer_hash;
                      pkg = Oi.Identity.of_string target;
                      outcome = Solve_failed { reason = msg };
                      duration_s = 0.0;
                      context;
                      log = Some { text_path = log_path; tail };
                    }
                  in
                  Oi.Audit.append ~fs ~cache_root:(Oi.Cache.root_s cache) event)
                group;
              Log.debug (fun m ->
                  m "solve failed: %s: %s" (group_label group) msg);
              None
      in
      if n_groups <= 1 then List.filter_map solve_one target_groups
      else
        let acc = ref [] in
        Eio.Switch.run @@ fun sw ->
        Oi.Ui.run ~status_lines:0 ~sw
          ~clock:(clock :> _ Eio.Time.clock)
          ~total:n_groups ~title:"solve"
          (fun ui ->
            List.iter
              (fun ((g, _) as group_and_handles) ->
                let label = group_label g in
                Oi.Ui.with_msg ui (Fmt.str "solve %s" label);
                (match solve_one group_and_handles with
                | Some s -> acc := s :: !acc
                | None -> ());
                Oi.Ui.tick ui)
              target_groups);
        List.rev !acc
    in
    if solutions = [] then Oi.Error.msg "no packages solved successfully";
    (* [--depext] intercept: every solve group is now resolved, so we
       have enough to compute depexts without running the build loop.
       The [--all] / bare [@h] cases short-circuited above; this
       handles single PKG / @HANDLE/PKG / mixed-target invocations. *)
    if depext_only then begin
      let all =
        List.fold_left
          (fun acc (_, _, pkg_dirs, pkgs, _, group_conf, _) ->
            let entries =
              Oi.Depexts.compute_for_conf ~conf:group_conf
                ~packages_dirs:pkg_dirs pkgs
            in
            List.fold_left
              (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
              acc entries)
          OpamSysPkg.Set.empty solutions
      in
      OpamSysPkg.Set.iter (fun p -> Fmt.pr "%s@." (OpamSysPkg.to_string p)) all;
      exit 0
    end;
    (* 2. Each solve group is its own build group (no cross-group
       merging). Dedup-by-layer-hash already happens inside the layer
       cache; merging here would only gain shared plan construction
       at the cost of failure cascades. *)
    let n_groups = List.length solutions in
    Log.info (fun m -> m "%d build group(s)" n_groups);
    (* Shared failure tracker across every build group: if [foo.1.2]
       fails in group 1, any later group that depends on the same
       layer hash skips it and marks its own dependents as failed
       rather than retrying the doomed build. Keyed by layer_hash so
       the same [name.version] resolved to a different layer (e.g.
       via a different dep set in another overlay) still gets its own
       attempt. *)
    let failed_layers : (string, string) Hashtbl.t = Hashtbl.create 64 in
    (* 3. Build each group, threading a single cross-group progress
       bar and accounting. The bar shows total packages processed
       (across every build group), the group counter, and live
       counts of each outcome so the user can see at a glance how
       many are built vs. cached vs. failing to build vs. skipped
       because a dep failed. *)
    let total_pkgs_estimate =
      List.fold_left
        (fun acc (_, _, _, pkgs, _, _, _) -> acc + List.length pkgs)
        0 solutions
    in
    let counters =
      object
        val mutable ok = 0
        val mutable cached = 0
        val mutable build_failed = 0
        val mutable dep_failed = 0
        val mutable cur_group = 0
        val mutable cur_pkg = ""
        method ok = ok
        method cached = cached
        method build_failed = build_failed
        method dep_failed = dep_failed
        method set_group g = cur_group <- g
        method set_pkg p = cur_pkg <- p
        method incr_ok = ok <- ok + 1
        method incr_cached = cached <- cached + 1
        method incr_build_failed = build_failed <- build_failed + 1
        method incr_dep_failed = dep_failed <- dep_failed + 1

        method status =
          Fmt.str "groups %d/%d  ok:%d cached:%d fail:%d dep:%d  %s" cur_group
            n_groups ok cached build_failed dep_failed cur_pkg
      end
    in
    let make_reporter ui =
      Oi.Execute.
        {
          pkg_event =
            (fun e ->
              (match e with
              | Started { pkg; _ } -> counters#set_pkg pkg
              | Cached _ -> counters#incr_cached
              | Built _ -> counters#incr_ok
              | Build_failed { pkg; log } ->
                  counters#incr_build_failed;
                  Oi.Ui.suspend ui (fun () ->
                      Fmt.epr "  %a %s → %s@." Oi.Style.error_string "FAIL" pkg
                        log)
              | Install_failed { pkg; log } ->
                  counters#incr_build_failed;
                  Oi.Ui.suspend ui (fun () ->
                      Fmt.epr "  %a %s (install) → %s@." Oi.Style.error_string
                        "FAIL" pkg log)
              | Dep_failed _ -> counters#incr_dep_failed);
              match e with
              | Started _ -> Oi.Ui.with_msg ui counters#status
              | Cached _ | Built _ | Build_failed _ | Install_failed _
              | Dep_failed _ ->
                  Oi.Ui.tick ~msg:counters#status ui);
        }
    in
    let in_progress_reporter f =
      if dry_run then f None
      else begin
        Eio.Switch.run @@ fun sw ->
        Oi.Ui.run ~status_lines:0 ~sw
          ~clock:(clock :> _ Eio.Time.clock)
          ~total:total_pkgs_estimate ~title:"build"
          (fun ui -> f (Some (make_reporter ui)))
      end
    in
    in_progress_reporter @@ fun reporter ->
    List.iteri
      (fun gi
           ( group_targets_list,
             _handles,
             pkg_dirs,
             solution_pkgs,
             toolchain,
             group_conf,
             tc_ctx ) ->
        counters#set_group (gi + 1);
        let group_targets = String.concat ", " group_targets_list in
        List.iter
          (fun t -> Hashtbl.replace target_group t gi)
          group_targets_list;
        (* Solution packages are already unique within a single solve,
           so no cross-solution dedup is needed. *)
        let merged_pkgs = solution_pkgs in
        let group_ctx =
          Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkg_dirs
            ~conf:group_conf ?toolchain:tc_ctx ()
        in
        let sorted_pkgs =
          Oi.Solver.topo_sort ~packages_dirs:pkg_dirs
            ~conf:(Oi.Solver.Ctx.conf group_ctx)
            merged_pkgs
        in
        if n_groups > 1 then
          Log.info (fun m ->
              m "Group %d/%d [%s]: %d packages" (gi + 1) n_groups group_targets
                (List.length sorted_pkgs))
        else
          Log.info (fun m -> m "%d unique packages" (List.length sorted_pkgs));
        let build_plan =
          Oi.Plan.build group_ctx ~d10 ~packages_dirs:pkg_dirs sorted_pkgs
        in
        let count_by f =
          List.length (List.filter f (Oi.Plan.nodes build_plan))
        in
        let n_build = count_by (fun (n : Oi.Plan.node) -> n.method_ = Source) in
        let n_cached =
          count_by (fun (n : Oi.Plan.node) -> n.method_ = Binary)
        in
        let n_pkgs = n_build + n_cached in
        if dry_run then begin
          let remote_has =
            match remote with
            | Some r ->
                let idx = D10.Layer.fetch_remote_index d10 ~remote:r in
                fun h -> Hashtbl.mem idx h
            | None -> fun _ -> false
          in
          Fmt.pr "%a@." (Oi.Plan.pp_tree ~remote_has) build_plan
        end
        else if n_build = 0 then begin
          (* Fast path for fully-cached groups: every layer is already
             in the d10 cache, so there's no reason to pay for
             [Plan.create] (resolves commands / install files),
             [Execute.run] (prefix wipe + layer restore), or the
             source-mirror promotion pass. Just emit Cached events for
             the reporter counters and call it done. This is the
             common case when re-running [oi build --all] against an
             already-populated cache. *)
          Log.info (fun m -> m "all %d packages cached" n_cached);
          (match reporter with
          | None -> ()
          | Some r ->
              List.iter
                (fun (n : Oi.Plan.node) ->
                  r.pkg_event
                    (Oi.Execute.Cached { pkg = OpamPackage.to_string n.pkg }))
                (Oi.Plan.nodes build_plan));
          (* Append one Audit event per cached pkg so the manifest's
             [callers[]] list sees this group's contribution — the fast path
             skips Execute.run (which is where audit lines normally get
             appended). Overlay is resolved from [pkg_dirs] so the event
             attributes the right handle whether or not Execute.run ran. *)
          let now = Unix.gettimeofday () in
          let toolchain_handle =
            Option.map (fun (i : Oi.Toolchain.info) -> i.handle) toolchain
          in
          List.iter
            (fun (n : Oi.Plan.node) ->
              let context : Oi.Audit.context =
                {
                  (Oi.Audit.default_context ()) with
                  overlay = Oi.Plan.overlay_of_pkg ~packages_dirs:pkg_dirs n.pkg;
                  toolchain = toolchain_handle;
                }
              in
              let event : Oi.Audit.event =
                {
                  schema = 1;
                  event_id = Oi.Audit.ulid ();
                  invocation_id = Oi.Audit.invocation_id ();
                  ts = now;
                  os_key;
                  target = Layer n.layer_hash;
                  pkg = Oi.Identity.of_opam n.pkg;
                  outcome = Cached;
                  duration_s = 0.0;
                  context;
                  log = None;
                }
              in
              Oi.Audit.append ~fs ~cache_root:(Oi.Cache.root_s cache) event)
            (Oi.Plan.nodes build_plan);
          Hashtbl.replace group_results gi (`Ok (n_pkgs, n_build, n_cached))
        end
        else begin
          Log.info (fun m -> m "%d to build, %d cached" n_build n_cached);
          (* Depext pre-flight: warn (do not skip) when the host's
             package manager reports any of this group's depexts as
             missing. We used to mark the group [depext-fail] and skip
             the build, but [OpamSysInteract.packages_status] returns
             false positives inside containers (alpine in particular —
             [pkgconf], [linux-headers], [gmp-dev] all report missing
             when they're in the just-completed [apk add]), so the
             skip throws away groups that would have built fine. The
             only true cost of letting the build proceed is that
             genuinely-missing depexts surface as compile failures of
             the [conf-*] probe packages rather than a precise list
             upfront — those packages already log their failure log
             paths individually. *)
          let depext_classify () =
            let source_pkgs =
              Oi.Plan.nodes build_plan
              |> List.filter_map (fun (n : Oi.Plan.node) ->
                  match n.method_ with
                  | Oi.Identity.Source -> Some n.pkg
                  | Binary -> None)
            in
            if source_pkgs = [] then None
            else
              let entries =
                Oi.Depexts.compute group_ctx ~packages_dirs:pkg_dirs source_pkgs
              in
              let all =
                List.fold_left
                  (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
                  OpamSysPkg.Set.empty entries
              in
              if OpamSysPkg.Set.is_empty all then None
              else
                let st = Oi.Depexts.status all in
                if OpamSysPkg.Set.is_empty st.missing then None
                else
                  let per_pkg =
                    List.filter_map
                      (fun (e : Oi.Depexts.entry) ->
                        let m = OpamSysPkg.Set.inter e.sys_pkgs st.missing in
                        if OpamSysPkg.Set.is_empty m then None
                        else Some (OpamPackage.to_string e.pkg, m))
                      entries
                  in
                  Some (st.missing, per_pkg)
          in
          (match depext_classify () with
          | Some (missing, per_pkg) ->
              let log_path =
                Oi.Cache.Logs.path ~cache_root ~kind:"depext"
                  ~name:(List.hd group_targets_list)
                  ~hash:(Digest.to_hex (Digest.string group_targets))
              in
              let body =
                let buf = Buffer.create 512 in
                Buffer.add_string buf "Group: ";
                Buffer.add_string buf group_targets;
                Buffer.add_string buf
                  "\n\nSystem packages reported missing by opam:\n";
                OpamSysPkg.Set.iter
                  (fun p ->
                    Buffer.add_string buf "  ";
                    Buffer.add_string buf (OpamSysPkg.to_string p);
                    Buffer.add_char buf '\n')
                  missing;
                Buffer.add_string buf "\nRequired by:\n";
                List.iter
                  (fun (pkg, set) ->
                    Buffer.add_string buf "  ";
                    Buffer.add_string buf pkg;
                    Buffer.add_string buf ": ";
                    Buffer.add_string buf
                      (set |> OpamSysPkg.Set.elements
                      |> List.map OpamSysPkg.to_string
                      |> String.concat ", ");
                    Buffer.add_char buf '\n')
                  per_pkg;
                Buffer.contents buf
              in
              Oi.Cache.Logs.write ~fs ~cache_root log_path body;
              Log.warn (fun m ->
                  m
                    "%s: opam reports %d missing system package(s); proceeding \
                     with the build anyway. See %s"
                    group_targets
                    (OpamSysPkg.Set.cardinal missing)
                    log_path)
          | None -> ());
          (* After the build, any package in this group's plan whose
             layer hash is in [failed_layers] with a non-empty path
             either failed directly (its own build log) or inherited a
             log from a failed upstream dep (cascaded). Dedup by log
             path so a single upstream failure doesn't produce N
             repeated summary lines for its dependents. *)
          let collect_failures (exec_plan : Oi.Plan.t) =
            let seen = Hashtbl.create 16 in
            List.concat_map
              (fun (g : Oi.Plan.group) ->
                List.filter_map
                  (fun (p : Oi.Plan.package_plan) ->
                    match Hashtbl.find_opt failed_layers p.layer_hash with
                    | Some path when path <> "" && not (Hashtbl.mem seen path)
                      ->
                        Hashtbl.replace seen path ();
                        Some (p.pkg, path)
                    | _ -> None)
                  g.packages)
              exec_plan.groups
          in
          let build_outcome : [ `Ok | `Fail of string * (string * string) list ]
              =
            let build_plan =
              Oi.Pipeline.fetch_remote_layers ?jobs ~remote ~d10
                ~packages_dirs:pkg_dirs ~ctx:group_ctx ~pkgs:sorted_pkgs
                build_plan
            in
            let exec_plan_ref = ref None in
            try
              let exec_plan =
                Oi.Plan.resolve group_ctx ~packages_dirs:pkg_dirs ~cache_root
                  ~os_key ~ocaml_version:conf.ocaml_version build_plan
              in
              exec_plan_ref := Some exec_plan;
              let cache_urls = Oi.Pipeline.cache_urls ~cache ~remote in
              let audit_base : Oi.Audit.context =
                {
                  (Oi.Audit.default_context ()) with
                  toolchain =
                    Option.map
                      (fun (i : Oi.Toolchain.info) -> i.handle)
                      toolchain;
                }
              in
              Oi.Execute.run ~cache_urls ?jobs ~failed_layers ?reporter
                ~audit_base ~proc_mgr ~fs
                ~clock:(clock :> D10.Config.clk)
                ~sys ~os_key exec_plan;
              `Ok
            with
            | Oi.Error.E e ->
                let failures =
                  match !exec_plan_ref with
                  | Some p -> collect_failures p
                  | None -> []
                in
                `Fail (Fmt.str "%a" Oi.Error.pp e, failures)
            | Failure msg ->
                let failures =
                  match !exec_plan_ref with
                  | Some p -> collect_failures p
                  | None -> []
                in
                `Fail (msg, failures)
          in
          Hashtbl.replace group_results gi
            (match build_outcome with
            | `Ok -> `Ok (n_pkgs, n_build, n_cached)
            | `Fail (msg, failures) ->
                `Fail (n_pkgs, n_build, n_cached, msg, failures))
        end)
      solutions;
    if not dry_run then begin
      print_build_summary ~targets ~target_handle ~solve_failures ~target_group
        ~group_results;
      (* Point at any fetch-retry logs collected during the run so
         the user can investigate transient errors (git fetch
         failures, opam archive 500s) without them polluting the
         live output. *)
      let logs_dir = Oi.Cache.Logs.dir ~cache_root in
      if Sys.file_exists logs_dir then begin
        (* Only list logs that were (re-)written during THIS run. Stale
           [fetch-*.log] files left over from previous invocations
           would otherwise show up unrelated to the current build. *)
        let written_this_run p =
          try (Unix.stat p).Unix.st_mtime >= run_start_time
          with Unix.Unix_error _ -> false
        in
        let entries =
          try
            Sys.readdir logs_dir |> Array.to_list
            |> List.filter (fun n -> String.starts_with ~prefix:"fetch-" n)
            |> List.map (fun n -> logs_dir / n)
            |> List.filter written_this_run
            |> List.sort String.compare
          with Sys_error _ -> []
        in
        if entries <> [] then begin
          Fmt.pr "@.%a (%d):@." Oi.Style.dim_string "transient fetch errors"
            (List.length entries);
          List.iter (fun p -> Fmt.pr "  %s@." p) entries
        end
      end
    end;
    (* [--export DIR]: publish the local cache once the build phase has
       settled. Skipped under [--dry-run]. *)
    do_export_if_set ()
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"PKG"
          ~doc:"Packages to build. Empty in project mode or with $(b,--all)." [])
  in
  let all =
    Arg.(
      value & flag
      & info
          ~doc:
            "Build every overlay's $(b,x-root-packages) (and each remaining \
             overlay's full content via $(b,@HANDLE)). The $(b,default) \
             overlay is skipped unless $(b,--only default) is given."
          [ "all" ])
  in
  let only =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:"Restrict $(b,--all) to these handles. Repeatable." [ "only" ])
  in
  let skip =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:"Exclude these handles from $(b,--all). Repeatable." [ "skip" ])
  in
  let dry_run =
    Arg.(
      value & flag
      & info ~doc:"Print the build plan; do nothing." [ "n"; "dry-run" ])
  in
  let depext_only =
    Arg.(
      value & flag
      & info
          ~doc:
            "Solve only; print system packages required by the result, one per \
             line. Pipe to a system package manager."
          [ "depext" ])
  in
  let export =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"DIR"
          ~doc:
            "After the build, publish the local cache (layers + index + \
             sources) into $(b,DIR). Errors out without a $(b,PKG), \
             $(b,@HANDLE), $(b,--all), or project."
          [ "export" ])
  in
  let deps_only =
    Arg.(
      value & flag
      & info
          ~doc:
            "Project mode: install deps + dev tools into $(b,_oi/), stop \
             before $(b,dune build). Use after a manifest edit."
          [ "deps-only" ])
  in
  let info =
    Cmd.info "build" ~doc:"Build a project, package, overlay, or every overlay"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve and build the requested target into the layer cache. With \
             no $(b,PKG), syncs the cwd's $(b,*.opam) deps + dev tools into \
             $(b,_oi/) and runs $(b,dune build --profile=release).";
          `S "TARGETS";
          `I ("(none)", "Cwd's $(b,*.opam) project.");
          `I ("$(b,PKG)", "Single package.");
          `I ("$(b,@HANDLE/PKG)", "Package from overlay $(b,HANDLE).");
          `I ("$(b,@HANDLE)", "Every package in overlay $(b,HANDLE).");
          `I ("$(b,--all)", "Every overlay's $(b,x-root-packages).");
          `S "PROJECT EXTRAS";
          `P "In project mode, the cwd's metadata feeds the solve:";
          `I
            ( "$(b,*.opam)",
              "$(b,depends:), $(b,pin-depends:), and $(b,x-repos:) merge into \
               the solve." );
          `I
            ( "$(b,packages/) + $(b,repo)",
              "When the project root contains a $(b,repo) marker, its \
               $(b,packages/) tree is injected as the highest-priority \
               opam-repository — patch a transitive dep's opam file without \
               vendoring its sources." );
          `S Manpage.s_examples;
          `Pre
            "  oi build\n\
            \  oi build --deps-only\n\
            \  oi build dune\n\
            \  oi build @avsm/owntracks\n\
            \  oi build @avsm\n\
            \  oi build --all --export ./registry\n\
            \  oi build --all --depext | sudo apt install -y -";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ Terms.skip_local $ dry_run $ all $ only $ skip $ Terms.registry
      $ Terms.with_repos $ Terms.with_deps $ Terms.jobs $ Terms.toolchain
      $ depext_only $ export $ Sync.envrc_mode_arg $ deps_only $ targets)

(* -- oi test ------------------------------------------------------------ *)

let test_cmd =
  let run () data_dir cache_dir refresh skip_local registry with_repos with_deps
      jobs toolchain_override envrc_mode dry_run targets =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache } =
      Harness.bootstrap env cache_dir
    in
    let cwd_s, _ = Workspace.resolved_cwd fs in
    let project_mode =
      (not skip_local) && targets = []
      &&
        try
          Sys.readdir cwd_s
          |> Array.exists (fun f ->
              Filename.check_suffix f ".opam"
              && Filename.chop_suffix f ".opam" <> "")
        with Sys_error _ -> false
    in
    match targets with
    | [] ->
        if not project_mode then
          Oi.Error.config_error
            "oi test: no *.opam in %s. Run from a project, or pass a PKG / \
             @HANDLE/PKG."
            cwd_s;
        let ec =
          Project_build.run ~action:`Test ~fs ~proc_mgr ~clock ~sys ~platform
            ~os_key ~cache ~data_dir ~registry ~refresh ~with_repos ~with_deps
            ?jobs ?toolchain:toolchain_override ~envrc_mode ~dry_run ~cwd:cwd_s
            ()
        in
        exit ec
    | [ target ] ->
        let ec =
          run_target_test ~target ~fs ~proc_mgr ~clock ~sys ~platform ~os_key
            ~cache ~data_dir ~registry ~refresh ~with_repos ~with_deps ?jobs
            ?toolchain:toolchain_override ~dry_run ()
        in
        exit ec
    | _ ->
        Oi.Error.config_error
          "oi test takes at most one PKG / @HANDLE/PKG target."
  in
  let dry_run =
    Arg.(
      value & flag
      & info ~doc:"Print the test command; do nothing." [ "n"; "dry-run" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"PKG"
          ~doc:
            "Single package or $(b,@HANDLE/PKG). Builds it (and its deps), \
             then runs $(b,dune runtest) in the package's build dir."
          [])
  in
  let info =
    Cmd.info "test" ~doc:"Run a project's or a package's tests"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "With no $(b,PKG): syncs the cwd's $(b,*.opam) deps + dev tools \
             into $(b,_oi/) and runs $(b,dune runtest --profile=release).";
          `P
            "With $(b,PKG) or $(b,@HANDLE/PKG): builds the package's layer \
             closure (same as $(b,oi build PKG)), then runs $(b,dune runtest) \
             in the package's build dir against the assembled prefix.";
          `P "Use $(b,oi docker --test) to generate a CI Dockerfile.";
          `S Manpage.s_examples;
          `Pre "  oi test\n  oi test @avsm/owntracks";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.refresh
      $ Terms.skip_local $ Terms.registry $ Terms.with_repos $ Terms.with_deps
      $ Terms.jobs $ Terms.toolchain $ Sync.envrc_mode_arg $ dry_run $ targets)
