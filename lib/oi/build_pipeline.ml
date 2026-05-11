let ( / ) = Filename.concat

type env = {
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  os_key : string;
  cache : Cache.t;
  data_dir : string;
  http_session : D10.Sysops.Http.session;
}

type target =
  | Plain of string
  | Group of { tokens : string list; handles : string list }
  | Overlay_pkg of { handle : string; spec : string }
  | Overlay_all of string

let parse s =
  match Build_request.parse_build_target s with
  | Plain_target s -> Plain s
  | Overlay_pkg (h, p) -> Overlay_pkg { handle = h; spec = p }
  | Overlay_all h -> Overlay_all h

type request = {
  targets : target list;
  with_repos : string list;
  pins : Project.pin list;
  extra_repos : Project.extra_repo list;
  constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  toolchain_override : string option;
  toolchain : Toolchain.info option;
  conf : Solver.Ctx.conf;
  local_packages_dir : string option;
  project_root : string option;
  force_source : bool;
  refresh : bool;
}

type group = {
  label : string;
  tokens : string list;
  names : OpamPackage.Name.t list;
  handles : string list;
  group_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
}

type group_error =
  | Solve_failed of { msg : string; log_path : string }
  | Cycle of OpamPackage.t list list
  | Empty_after_strip
  | Elaborate_failed of { msg : string }
  | Emit_failed of { msg : string }

type group_result = {
  group : group;
  toolchain : Toolchain.info option;
  pkgs_dir : string list;
  pkgs : OpamPackage.t list;
  exec_plan : Plan.t option;
  recipe : D10ir.Plan.t option;
  error : (unit, group_error) result;
}

type solved = {
  groups : group_result list;
  merged : D10ir.Plan.t option;
  toolchain : Toolchain.info option;
}

(* -- Target → solve-group expansion --------------------------------------- *)

(* Expand bare [@HANDLE] entries into per-package solve groups by reading the
   overlay's [x-root-packages] (or, if empty, every package the overlay
   ships). Plain and [Overlay_pkg] targets become singleton groups; the
   overall token list mirrors what [oi build]'s loop sees today. *)
let expand_targets ~fs ~sys ~reporepo_path ~reporepo_url (targets : target list)
    : (string list * string list) list =
  let need_reporepo =
    List.exists
      (function
        | Overlay_all _ | Overlay_pkg _ -> true
        | Plain _ -> false
        | Group { handles; _ } -> handles <> [])
      targets
  in
  let entries_lazy =
    if need_reporepo then
      lazy
        (Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false
           ~path:reporepo_path ~url:reporepo_url ();
         Source.Reporepo.load ~path:reporepo_path)
    else lazy []
  in
  List.concat_map
    (fun t ->
      match t with
      | Plain p -> [ ([ p ], []) ]
      | Group { tokens; handles } -> [ (tokens, handles) ]
      | Overlay_pkg { handle; spec } -> [ ([ spec ], [ handle ]) ]
      | Overlay_all handle -> begin
          let entries = Lazy.force entries_lazy in
          match Source.Reporepo.latest entries ~handle with
          | None -> Error.config_error "no overlay @%s in reporepo" handle
          | Some (e : Source.Reporepo.entry) ->
              let groups =
                if e.root_packages <> [] then e.root_packages
                else
                  let pkgs_dir =
                    Source.Reporepo.assert_overlay_dir ~path:reporepo_path
                      ~handle
                  in
                  Sys.readdir pkgs_dir |> Array.to_list
                  |> List.filter (fun n -> Sys.is_directory (pkgs_dir / n))
                  |> List.sort String.compare
                  |> List.map (fun p -> [ p ])
              in
              List.map (fun group -> (group, [ handle ])) groups
        end)
    targets

(* -- Per-group toolchain / packages_dirs ---------------------------------- *)

(* Pick one toolchain for the whole batch. Same precedence rules
   {Pipeline.pick_toolchain} uses for any other command — explicit
   override wins, then implicit pickup from the union of in-scope
   handles, then the reporepo default. Per-group toolchain selection
   is a corner case [oi build] supports today; we'll thread that in if
   a caller actually needs it. *)
let pick_batch_toolchain ~env ~conf ~override ~all_handles =
  let key = List.sort_uniq String.compare all_handles in
  Pipeline.pick_toolchain ~fs:env.fs ~sys:env.sys ~data_dir:env.data_dir ~conf
    ~install:false ~override ~handles:key ()

let packages_dirs_for_group ~env ~reporepo_path ~base_pkgs_dirs ?pin_dir
    ?local_packages_dir ~global_handles ~toolchain handles =
  let global_handles =
    Pipeline.filter_compatible_overlays ~reporepo_path ~toolchain global_handles
  in
  let effective = global_handles @ handles |> List.sort_uniq String.compare in
  let entries = try Source.Reporepo.load ~path:reporepo_path with _ -> [] in
  let resolved =
    match (toolchain : Toolchain.info option) with
    | None -> (
        let roots =
          List.rev effective
          |> List.map (fun h : Source.Reporepo.root ->
              { handle = h; version = None })
        in
        try Source.Reporepo.resolve entries ~roots |> List.rev
        with Error.E _ -> [])
    | Some _ ->
        List.filter_map
          (fun h -> Source.Reporepo.latest entries ~handle:h)
          effective
  in
  let overlay_dirs =
    List.map
      (fun (e : Source.Reporepo.entry) ->
        Source.Reporepo.assert_overlay_dir ~path:reporepo_path ~handle:e.handle)
      resolved
  in
  let base =
    match (toolchain : Toolchain.info option) with
    | None -> base_pkgs_dirs
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
  let _ = env in
  dedup
    (Stdlib.Option.to_list local_packages_dir
    @ Stdlib.Option.to_list pin_dir
    @ overlay_dirs @ base)

(* -- Per-group solve / elaborate / emit ----------------------------------- *)

let solve_group ~env ~conf ~toolchain_override ~global_handles ~base_pkgs_dirs
    ~pin_dir ?local_packages_dir ~reporepo_path ~base_constraints ~build_prefix
    ~cache_root ~d10 ~force_source (toolchain : Toolchain.info option)
    ((tokens, group_handles) : string list * string list) : group_result =
  let label = String.concat ", " tokens in
  let pkgs_dir =
    packages_dirs_for_group ~env ~reporepo_path ~base_pkgs_dirs ?pin_dir
      ?local_packages_dir ~global_handles ~toolchain group_handles
  in
  let group_conf, tc_ctx = Pipeline.solver_inputs toolchain conf in
  (* Mirror [oi build]'s [--toolchain=NAME] behavior: strip
     compiler-family roots from the group's tokens so the override's
     compiler pins land cleanly. *)
  let stripped_tokens =
    match (toolchain_override, toolchain) with
    | Some _, Some (info : Toolchain.info) ->
        List.filter
          (fun pkg ->
            let name, _ = Build_request.parse_pkg_target pkg in
            not (OpamPackage.Name.Set.mem name info.root_names))
          tokens
    | _ -> tokens
  in
  let make_failure ?(toolchain = toolchain) ?(pkgs = []) ?(exec_plan = None)
      ?(recipe = None) err =
    {
      group =
        {
          label;
          tokens;
          names = [];
          handles = group_handles;
          group_constraints = OpamPackage.Name.Map.empty;
        };
      toolchain;
      pkgs_dir;
      pkgs;
      exec_plan;
      recipe;
      error = Error err;
    }
  in
  if stripped_tokens = [] then make_failure Empty_after_strip
  else
    let items = List.map Build_request.parse_pkg_target stripped_tokens in
    let names = List.map fst items in
    let group_constraints =
      List.fold_left
        (fun acc (name, c) ->
          match c with
          | None -> acc
          | Some c -> OpamPackage.Name.Map.add name c acc)
        base_constraints items
    in
    let group =
      { label; tokens; names; handles = group_handles; group_constraints }
    in
    let gctx =
      Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkgs_dir
        ~conf:group_conf ?toolchain:tc_ctx ()
    in
    match
      Solver.solve ~fs:env.fs ~cache_root gctx ~packages_dirs:pkgs_dir
        ~constraints:group_constraints names
    with
    | Error msg ->
        let log_path =
          let key =
            String.concat " "
              (stripped_tokens @ List.map (fun h -> "@" ^ h) group_handles)
          in
          let hash = Digest.to_hex (Digest.string key) in
          let first =
            match stripped_tokens with t :: _ -> t | [] -> "solve"
          in
          let path =
            Cache.Logs.path ~cache_root ~kind:"solve" ~name:first ~hash
          in
          let body =
            Fmt.str "targets: %s\nhandles: %s\n\n%s%s"
              (String.concat ", " stripped_tokens)
              (if group_handles = [] then "(base only)"
               else
                 String.concat ", " (List.map (fun h -> "@" ^ h) group_handles))
              msg
              (if msg = "" || msg.[String.length msg - 1] = '\n' then ""
               else "\n")
          in
          Cache.Logs.write ~fs:env.fs ~cache_root path body;
          path
        in
        {
          group;
          toolchain;
          pkgs_dir;
          pkgs = [];
          exec_plan = None;
          recipe = None;
          error = Error (Solve_failed { msg; log_path });
        }
    | Ok pkgs -> (
        match
          try
            let plan_d10 = if force_source then None else Some d10 in
            Ok
              (Plan.of_solution gctx ?d10:plan_d10 ~packages_dirs:pkgs_dir pkgs)
          with Plan.Cycle cs -> Error cs
        with
        | Error cs ->
            {
              group;
              toolchain;
              pkgs_dir;
              pkgs;
              exec_plan = None;
              recipe = None;
              error = Error (Cycle cs);
            }
        | Ok build_plan ->
            begin try
              let exec_plan =
                Plan.elaborate gctx ~packages_dirs:pkgs_dir ~cache_root
                  ~os_key:env.os_key ~ocaml_version:group_conf.ocaml_version
                  build_plan
              in
              let toolchain_name =
                match toolchain with Some i -> i.handle | None -> "system"
              in
              (* [base_layer] in the recipe is informational (see
                 [d10ir/plan.ml] merge comment): it identifies the toolchain
                 root for diagnostics. Prefer one of the toolchain's own
                 layer hashes when present (non-relocatable toolchain — the
                 toolchain packages are filtered out of [packages] and
                 surfaced via [external_layer_hashes]); otherwise fall back
                 to the first consumer package's layer hash. *)
              let toolchain_layer =
                match exec_plan.external_layer_hashes, exec_plan.packages with
                | h :: _, _ -> h
                | [], p :: _ -> p.layer_hash
                | [], [] -> ""
              in
              if toolchain_layer = "" then
                (* Every selected package was filtered out by [elaborate]
                   (target reduced entirely to toolchain-provided packages,
                   e.g. [oi build ocaml-variants] under a non-relocatable
                   toolchain). Nothing for the d10ir executor to do — record
                   a clean failure instead of crashing on an empty layer
                   hash. *)
                {
                  group;
                  toolchain;
                  pkgs_dir;
                  pkgs;
                  exec_plan = Some exec_plan;
                  recipe = None;
                  error =
                    Error
                      (Emit_failed
                         {
                           msg =
                             "all selected packages are toolchain-provided; \
                              nothing to build";
                         });
                }
              else begin
                try
                  let recipe =
                    Recipe_emitter.emit ~d10
                      ~cli_invocation:(Array.to_list Sys.argv) ~toolchain_name
                      ~toolchain_layer exec_plan
                  in
                  {
                    group;
                    toolchain;
                    pkgs_dir;
                    pkgs;
                    exec_plan = Some exec_plan;
                    recipe = Some recipe;
                    error = Ok ();
                  }
                with
                | Error.E e ->
                    {
                      group;
                      toolchain;
                      pkgs_dir;
                      pkgs;
                      exec_plan = Some exec_plan;
                      recipe = None;
                      error =
                        Error (Emit_failed { msg = Fmt.str "%a" Error.pp e });
                    }
                | Failure msg ->
                    {
                      group;
                      toolchain;
                      pkgs_dir;
                      pkgs;
                      exec_plan = Some exec_plan;
                      recipe = None;
                      error = Error (Emit_failed { msg });
                    }
                | Invalid_argument msg ->
                    {
                      group;
                      toolchain;
                      pkgs_dir;
                      pkgs;
                      exec_plan = Some exec_plan;
                      recipe = None;
                      error = Error (Emit_failed { msg });
                    }
              end
            with
            | Error.E e ->
                {
                  group;
                  toolchain;
                  pkgs_dir;
                  pkgs;
                  exec_plan = None;
                  recipe = None;
                  error =
                    Error (Elaborate_failed { msg = Fmt.str "%a" Error.pp e });
                }
            | Failure msg ->
                {
                  group;
                  toolchain;
                  pkgs_dir;
                  pkgs;
                  exec_plan = None;
                  recipe = None;
                  error = Error (Elaborate_failed { msg });
                }
            end)

(* -- Persistent cache for the [solved] struct ----------------------------- *)

(* Bumped whenever the [solved] / [group_result] / [Plan.t] / [D10ir.Plan.t]
   shape changes in a way that breaks Marshal compatibility. Folded into
   the cache key so a stale entry produces a key miss rather than a
   crash on [Marshal.from_channel]. *)
let cache_schema = "v1"
let log_src = Logs.Src.create "oi.build_pipeline.cache"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* Process-memo of [git -C <repo> rev-parse HEAD] so a multi-call
   command (e.g. [oi build] solving 81 groups) doesn't re-shell N
   times for the same repo. *)
let head_memo : (string, string option) Hashtbl.t = Hashtbl.create 8

let read_process_output cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 64 in
  let chunk = Bytes.create 1024 in
  let rec loop () =
    let n = input ic chunk 0 (Bytes.length chunk) in
    if n > 0 then begin
      Buffer.add_subbytes buf chunk 0 n;
      loop ()
    end
  in
  loop ();
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Some (String.trim (Buffer.contents buf))
  | _ -> None

let git_head_of dir =
  match Hashtbl.find_opt head_memo dir with
  | Some r -> r
  | None ->
      let r =
        read_process_output
          (Printf.sprintf "git -C %s rev-parse HEAD 2>/dev/null"
             (Filename.quote dir))
      in
      let r = match r with Some s when s <> "" -> Some s | _ -> None in
      Hashtbl.add head_memo dir r;
      r

(* Cache-key signature: the reporepo HEAD plus a digest of the
   request's targets / scope / constraints / conf. Returns [None] if
   the reporepo isn't a git tree (the common case in tests / fresh
   installs without [oi repo bump]) — the caller falls back to an
   uncached solve. *)
let cache_key ~reporepo_path (req : request) : string option =
  match git_head_of reporepo_path with
  | None ->
      Log.info (fun m -> m "cache disabled: no git HEAD at %s" reporepo_path);
      None
  | Some head ->
      let buf = Buffer.create 1024 in
      let add s =
        Buffer.add_string buf s;
        Buffer.add_char buf '\x00'
      in
      add ("schema:" ^ cache_schema);
      add ("repo:" ^ head);
      add "TARGETS:";
      List.iter
        (fun t ->
          match t with
          | Plain p -> add ("Plain:" ^ p)
          | Group { tokens; handles } ->
              add "Group:";
              List.iter add tokens;
              add "|";
              List.iter add handles
          | Overlay_pkg { handle; spec } -> add ("Pkg:" ^ handle ^ "/" ^ spec)
          | Overlay_all h -> add ("All:" ^ h))
        req.targets;
      add "WITH_REPOS:";
      List.iter add (List.sort String.compare req.with_repos);
      add "PINS:";
      List.iter
        (fun (p : Project.pin) ->
          add (OpamPackage.to_string p.pkg);
          add (OpamUrl.to_string p.url))
        req.pins;
      add "EXTRA:";
      List.iter
        (fun (e : Project.extra_repo) ->
          add e.name;
          add e.url;
          add (Stdlib.Option.value e.local_packages_dir ~default:""))
        req.extra_repos;
      add "CONSTRAINTS:";
      OpamPackage.Name.Map.iter
        (fun n (relop, v) ->
          add (OpamPackage.Name.to_string n);
          add (OpamPrinter.FullPos.relop_kind relop);
          add (OpamPackage.Version.to_string v))
        req.constraints;
      add ("override:" ^ Stdlib.Option.value req.toolchain_override ~default:"");
      add
        ("toolchain_hash:"
        ^ match req.toolchain with Some i -> i.hash | None -> "");
      add ("ocaml:" ^ req.conf.ocaml_version);
      add ("arch:" ^ req.conf.arch);
      add ("os:" ^ req.conf.os);
      add ("os_distribution:" ^ req.conf.os_distribution);
      add ("os_version:" ^ req.conf.os_version);
      add ("os_family:" ^ req.conf.os_family);
      add
        ("local_pkg_dir:"
        ^ Stdlib.Option.value req.local_packages_dir ~default:"");
      add ("project_root:" ^ Stdlib.Option.value req.project_root ~default:"");
      add ("force_source:" ^ string_of_bool req.force_source);
      Some (Digest.to_hex (Digest.string (Buffer.contents buf)))

let cache_path ~cache_root ~key =
  Filename.concat cache_root
    (Filename.concat "solve-cache"
       (Filename.concat (String.sub key 0 2) (key ^ ".marshal")))

let cache_lookup ~cache_root ~key : solved option =
  let path = cache_path ~cache_root ~key in
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> Some (Marshal.from_channel ic : solved))
    with exn ->
      Log.info (fun m ->
          m "solve cache: ignoring stale entry %s (%s)" (String.sub key 0 12)
            (Printexc.to_string exn));
      (try Sys.remove path with _ -> ());
      None

let cache_store ~fs ~cache_root ~key (s : solved) : unit =
  let path = cache_path ~cache_root ~key in
  let dir = Filename.dirname path in
  (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir)
   with _ -> ());
  let tmp = path ^ ".tmp" in
  try
    Out_channel.with_open_bin tmp (fun oc -> Marshal.to_channel oc s []);
    Sys.rename tmp path;
    Log.info (fun m ->
        m "solve cache stored %s (%d groups)" (String.sub key 0 12)
          (List.length s.groups))
  with exn -> (
    Log.info (fun m ->
        m "solve cache: failed to store %s (%s)" (String.sub key 0 12)
          (Printexc.to_string exn));
    try Sys.remove tmp with _ -> ())

(* -- The top-level entry point -------------------------------------------- *)

let solve_uncached env ?(reporter = Build_progress.null) (req : request) :
    solved =
  let reporepo_path = Source.Reporepo.env_path () in
  let reporepo_url = Source.Reporepo.env_url () in
  Pipeline.init_opam_root ~fs:env.fs ~data_dir:env.data_dir;
  let _ : string list =
    Source.Reporepo.ensure_base ~fs:env.fs ~sys:env.sys ~data_dir:env.data_dir
      ~refresh:req.refresh ()
  in
  (* Determine the union of every handle in scope across the request:
     [with_repos] (global), [extra_repos] handles, and per-target
     [@h] tokens. Used both for toolchain selection and for the
     per-group [packages_dirs] computation below. *)
  let token_handles =
    List.concat_map
      (function
        | Plain _ -> []
        | Group { handles; _ } -> handles
        | Overlay_pkg { handle; _ } | Overlay_all handle -> [ handle ])
      req.targets
  in
  let all_handles =
    List.sort_uniq String.compare (req.with_repos @ token_handles)
  in
  let toolchain =
    match req.toolchain with
    | Some i -> Some i
    | None ->
        pick_batch_toolchain ~env ~conf:req.conf
          ~override:req.toolchain_override ~all_handles
  in
  let conf, _tc_ctx = Pipeline.solver_inputs toolchain req.conf in
  (* Materialise pin sources and CLI / project extras so the
     per-group [packages_dirs] resolution below has paths it can
     stat. *)
  let pins =
    Source.Pin.resolve_pins ~fs:env.fs ~sys:env.sys
      ?project_root:req.project_root req.pins
  in
  let pin_dir =
    Source.Pin.materialize ~fs:env.fs ~sys:env.sys ~cache:env.cache
      ~refresh:req.refresh pins
  in
  let _extra_pkg_dirs : string list =
    Source.Repo.ensure_many ~fs:env.fs ~data_dir:env.data_dir
      ~refresh:req.refresh req.extra_repos
  in
  let base_pkgs_dirs =
    match toolchain with
    | None ->
        Source.Reporepo.ensure_base ~fs:env.fs ~sys:env.sys
          ~data_dir:env.data_dir ()
    | Some i -> i.packages_dirs
  in
  let global_handles =
    let toks = List.sort_uniq String.compare token_handles in
    List.filter (fun h -> not (List.mem h toks)) req.with_repos
  in
  let cache_root = Cache.root_s env.cache in
  let build_prefix = cache_root / "build" / "prefix" in
  let d10 =
    Pipeline.make_d10 ~sys:env.sys ~fs:env.fs
      ~clock:(env.clock :> D10.Config.clk)
      ~cache:env.cache ~os_key:env.os_key
  in
  let token_groups =
    expand_targets ~fs:env.fs ~sys:env.sys ~reporepo_path ~reporepo_url
      req.targets
  in
  let n_groups = List.length token_groups in
  reporter.Build_progress.event
    (Phase_started
       { phase = Solving; label = Fmt.str "Solving %d group(s)" n_groups });
  reporter.event (Aggregate { phase = Solving; total = n_groups; current = 0 });
  let solved_count = ref 0 in
  let groups =
    List.map
      (fun ((tokens, _) as g) ->
        let label = String.concat ", " tokens in
        reporter.event (Solve_started { label });
        let res =
          solve_group ~env ~conf ~toolchain_override:req.toolchain_override
            ~global_handles ~base_pkgs_dirs ~pin_dir
            ?local_packages_dir:req.local_packages_dir ~reporepo_path
            ~base_constraints:req.constraints ~build_prefix ~cache_root ~d10
            ~force_source:req.force_source toolchain g
        in
        reporter.event (Solve_finished { label });
        incr solved_count;
        reporter.event
          (Aggregate
             { phase = Solving; total = n_groups; current = !solved_count });
        res)
      token_groups
  in
  reporter.event (Phase_done Solving);
  let recipes = List.filter_map (fun (gr : group_result) -> gr.recipe) groups in
  let merged =
    match recipes with
    | [] -> None
    | _ -> (
        match D10ir.Plan.merge recipes with
        | Ok r -> Some r
        | Error msg ->
            Error.config_error
              "merging %d recipes failed: %s. This is a bug in \
               D10ir.Plan.merge: every recipe was produced by the same \
               toolchain in the same batch."
              (List.length recipes) msg)
  in
  { groups; merged; toolchain }

(* Cache-wrapped public entry point. [request.refresh = true]
   forces a fresh solve (skips lookup) but still updates the cache
   so the next run is fast. Lookup is skipped entirely when
   [cache_key] returns [None] (no git HEAD to anchor staleness on)
   — there's nothing safe to store against, so storing is also
   skipped in that case. *)
let solve env ?(reporter = Build_progress.null) (req : request) : solved =
  let reporepo_path = Source.Reporepo.env_path () in
  let cache_root = Cache.root_s env.cache in
  let key_opt = cache_key ~reporepo_path req in
  let cached =
    match key_opt with
    | None -> None
    | Some _ when req.refresh -> None
    | Some key -> cache_lookup ~cache_root ~key
  in
  match cached with
  | Some s ->
      let key = Stdlib.Option.get key_opt in
      Log.info (fun m ->
          m "solve cache hit %s (%d groups)" (String.sub key 0 12)
            (List.length s.groups));
      reporter.Build_progress.event
        (Status (Fmt.str "Solve cache hit (%d groups)" (List.length s.groups)));
      s
  | None ->
      let s = solve_uncached env ~reporter req in
      (* Only cache successful solves. A solve where every group
         failed produces a [merged = None] struct that's not worth
         re-loading. *)
      let any_ok =
        List.exists (fun (gr : group_result) -> Result.is_ok gr.error) s.groups
      in
      (match key_opt with
      | Some key when any_ok -> cache_store ~fs:env.fs ~cache_root ~key s
      | _ -> ());
      s

(* -- Post-merge fetch / archive prefetch / Direct.run --------------------- *)

type build_inputs = {
  solved : solved;
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
  jobs : int option;
}

(* Provenance is written here rather than inside [D10ir.Direct] because the
   richer [Plan.package_plan] (opam_path, pkgs_dir, source, depexts, dep_layers
   with Identity.dep shape) only exists at the oi level — the d10ir runtime
   sees a stripped [Plan.node]. Idempotent: skips when the file already exists,
   so re-runs (and previously cached layers seen across [oi build] passes)
   converge to a written sidecar without re-encoding on every invocation. *)
let source_kind_of_url url =
  if url = "" then ""
  else if String.starts_with ~prefix:"git+" url then "git"
  else if String.starts_with ~prefix:"git://" url then "git"
  else if String.starts_with ~prefix:"hg+" url then "hg"
  else if String.starts_with ~prefix:"darcs+" url then "darcs"
  else if String.starts_with ~prefix:"file:" url then "local"
  else if String.starts_with ~prefix:"/" url then "local"
  else "tar"

let provenance_of_package_plan ~os_key ~ocaml_version ~built_at
    (pp : Plan.package_plan) : Provenance.t =
  let pkg = Identity.of_string pp.pkg in
  let origin =
    match (pp.opam_path, pp.pkgs_dir) with
    | Some _opam_path, Some pkgs_dir ->
        Origin.of_packages_dir ~pkgs_dir ~name:pkg.name ~full:pp.pkg
    | _ -> { Origin.kind = Local; overlay = pp.overlay; path_in_repo = "" }
  in
  let opam_sha256 =
    match pp.opam_path with
    | Some p -> Provenance.hash_opam_file ~path:p
    | None -> ""
  in
  let source =
    Option.map
      (fun (s : Plan.source_info) : Provenance.source_info ->
        {
          url = s.url;
          kind = source_kind_of_url s.url;
          checksums = s.checksums;
        })
      pp.source
  in
  {
    schema = 1;
    layer_hash = pp.layer_hash;
    os_key;
    pkg;
    method_ = pp.method_;
    built_at;
    duration_s = 0.;
    phases = { fetch = None; build = None; install = None; restore = None };
    opam = { sha256 = opam_sha256; origin };
    source;
    deps = pp.dep_layers;
    depexts_declared = pp.depexts;
    build_env = { ocaml_version };
  }

let write_provenance_for_solved ~fs ~cache_root ~os_key ~(d10 : D10.Config.t)
    (s : solved) =
  let now = Unix.gettimeofday () in
  List.iter
    (fun (gr : group_result) ->
      match gr.exec_plan with
      | None -> ()
      | Some (ep : Plan.t) ->
          List.iter
            (fun (pp : Plan.package_plan) ->
              if D10.Layer.succeeded d10 ~hash:pp.layer_hash then
                let dst =
                  Provenance.path ~cache_root ~os_key ~hash:pp.layer_hash
                in
                if not (Sys.file_exists dst) then
                  let r =
                    provenance_of_package_plan ~os_key
                      ~ocaml_version:ep.ocaml_version ~built_at:now pp
                  in
                  Provenance.write ~fs ~cache_root r)
            ep.packages)
    s.groups

let build env ?(reporter = Build_progress.null) (inp : build_inputs) :
    D10ir.Direct.result option =
  match inp.solved.merged with
  | None -> None
  | Some (merged : D10ir.Plan.t) ->
      let cache_root = Cache.root_s env.cache in
      let d10 =
        Pipeline.make_d10 ~sys:env.sys ~fs:env.fs
          ~clock:(env.clock :> D10.Config.clk)
          ~cache:env.cache ~os_key:env.os_key
      in
      (* Probe the registry index once; the byte-denominator on the
         agg bar and the fetch list below share one source of truth. *)
      let merged_layer_index =
        match inp.layer_remote with
        | None -> None
        | Some r ->
            Some
              (r, D10.Remote_index.fetch d10 ~session:env.http_session ~remote:r)
      in
      let needed_fetches, needed_fetch_bytes =
        match merged_layer_index with
        | None -> ([], 0L)
        | Some (_, index) ->
            List.fold_left
              (fun (hs, bytes) (n : D10ir.Plan.node) ->
                let h = D10ir.Layer_hash.to_string n.layer_hash in
                match Hashtbl.find_opt index h with
                | Some (e : D10.Layer.index_entry)
                  when not (D10.Layer.succeeded d10 ~hash:h) ->
                    (h :: hs, Int64.add bytes e.size)
                | _ -> (hs, bytes))
              ([], 0L) merged.nodes
      in
      let post_total = List.length merged.nodes in
      reporter.Build_progress.event
        (Total_estimate
           {
             fetches = List.length needed_fetches;
             builds = post_total;
             fetch_bytes = needed_fetch_bytes;
           });
      let d10_cfg : D10.Config.t =
        {
          sys = env.sys;
          fs = env.fs;
          clock = (env.clock :> D10.Config.clk);
          root = Eio.Path.(env.fs / cache_root);
          os_key = env.os_key;
        }
      in
      let direct_cfg =
        let base = D10ir.Config.default in
        let base = D10ir.Config.with_env_overrides base in
        {
          base with
          build_parallelism =
            (match inp.jobs with Some j -> j | None -> base.build_parallelism);
        }
      in
      let plan_dir = Eio.Path.native_exn d10_cfg.root in
      let direct_reporter : D10ir.Direct.reporter =
        { event = (fun e -> reporter.event (Build e)) }
      in
      reporter.event
        (Phase_started { phase = Fetching; label = "build inputs" });
      (match merged_layer_index with
      | None -> ()
      | Some (r, index) ->
          if needed_fetches <> [] then begin
            let pkg_of : (string, string) Hashtbl.t =
              Hashtbl.create (List.length merged.nodes)
            in
            List.iter
              (fun (n : D10ir.Plan.node) ->
                let h = D10ir.Layer_hash.to_string n.layer_hash in
                let label = Fmt.str "%s.%s" n.package.name n.package.version in
                Hashtbl.replace pkg_of h label)
              merged.nodes;
            Pipeline.fetch_layer_hashes ~reporter ?jobs:inp.jobs
              ~session:env.http_session ~remote:r ~d10:d10_cfg ~index
              ~hashes:needed_fetches ~pkg_of ()
          end);
      (* Archive prefetch from the [d10ir-archives/] tree on the
         remote (if any), then a hard-error for anything that's still
         missing locally. The previous inline-bake fallback (running
         opam fetch + Archive_builder for each missing sha) is gone:
         every source archive must come from a [oi repo bump] pass.
         Users who hit "missing archive" should re-bump the offending
         overlay so the [d10ir-archives/<sha>.tar.zst] entry exists. *)
      let archive_dir_for sha =
        Filename.concat
          (Filename.concat (Filename.concat cache_root "d10ir") "archives")
          (sha ^ ".tar.zst")
      in
      let needs_archive (n : D10ir.Plan.node) =
        let h = D10ir.Layer_hash.to_string n.layer_hash in
        (not (D10.Layer.succeeded d10_cfg ~hash:h))
        && not (Sys.file_exists (archive_dir_for n.archive.sha256))
      in
      let missing_locally = List.filter needs_archive merged.nodes in
      if missing_locally <> [] then begin
        (match inp.source_remote with
        | Some (`Http_remote registry) ->
            let shas =
              List.map
                (fun (n : D10ir.Plan.node) -> n.archive.sha256)
                missing_locally
            in
            let _ : D10ir.Registry.prefetch_summary =
              D10ir.Registry.prefetch
                ~clock:(env.clock :> _ Eio.Time.clock_ty Eio.Resource.t)
                ~fs:env.fs ~session:env.http_session ~cache_root
                ~remote:(`Http_remote registry) shas
            in
            ()
        | None -> ());
        let still_missing = List.filter needs_archive missing_locally in
        if still_missing <> [] then
          let pkg_summary =
            List.map
              (fun (n : D10ir.Plan.node) ->
                Fmt.str "%s.%s (%s)" n.package.name n.package.version
                  (String.sub n.archive.sha256 0
                     (min 12 (String.length n.archive.sha256))))
              still_missing
            |> String.concat ", "
          in
          Error.config_error
            "%d source archive(s) missing locally and not on the registry: %s.\n\
             Run [oi repo bump] on the offending overlay to bake the archives, \
             or point [--registry] at a registry that publishes \
             [d10ir-archives/<sha>.tar.zst] for these shas."
            (List.length still_missing)
            pkg_summary
      end;
      reporter.event (Plan_ready merged);
      reporter.event (Phase_started { phase = Building; label = "build" });
      (* Pre-flight validation: same check as [oi ir run] / [oi ir lint],
         hoisted here so [oi run] / [oi build] surface plan structure
         bugs (e.g. a dep_layer_hash with no producer and no d10 entry —
         what a non-relocatable toolchain looked like before
         [external_layers] was added) as a clear error instead of
         letting [Direct.run] cascade-skip every node and bubble up as
         a misleading "solved but does not install bin/<X>". *)
      (match D10ir.Plan.validate ~d10:d10_cfg ~fs:env.fs ~plan_dir merged with
      | Ok () -> ()
      | Error err ->
          Error.config_error "d10ir recipe failed validation before build: %a"
            D10ir.Plan.pp_validate_error err);
      let result =
        D10ir.Direct.run ~config:direct_cfg ~d10:d10_cfg ~fs:env.fs
          ~proc_mgr:env.proc_mgr
          ~clock:(env.clock :> D10.Config.clk)
          ~reporter:direct_reporter ~plan_dir merged
      in
      write_provenance_for_solved ~fs:env.fs ~cache_root ~os_key:env.os_key
        ~d10:d10_cfg inp.solved;
      reporter.event (Build_summary result);
      Some result

let layer_hashes (s : solved) : string list =
  match
    List.find_opt (fun (gr : group_result) -> Result.is_ok gr.error) s.groups
  with
  | None -> []
  | Some gr -> (
      match gr.exec_plan with
      | None -> []
      | Some p ->
          List.map (fun (pp : Plan.package_plan) -> pp.layer_hash) p.packages)
