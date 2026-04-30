[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Stage-based parallel build executor. *)

let log_src = Logs.Src.create "oi.execute"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* -- .install file processor (was lib/oi/installer.ml) ------------------ *)

(* Process an opam [.install] file without shelling out to
   [opam-installer]. Parses the file via [OpamFile.Dot_install], resolves
   each source relative to [build_dir] and copies it into the matching
   subdirectory of [prefix], matching the destination and permission
   rules that [opam-installer] applies with default flags. *)
module Installer = struct
  module S = OpamFile.Dot_install

  let installer_log = Logs.Src.create "oi.installer"

  module ILog = (val Logs.src_log installer_log : Logs.LOG)

  let pkgname_of_install_file path =
    let b = Filename.basename path in
    if Filename.check_suffix b ".install" then Filename.chop_suffix b ".install"
    else b

  let is_under ~base p =
    let canon p =
      try Unix.realpath p
      with Unix.Unix_error _ -> (
        try Unix.realpath (Filename.dirname p) / Filename.basename p
        with Unix.Unix_error _ -> p)
    in
    let base = canon base in
    let p = canon p in
    let n = String.length base in
    String.length p = n
    || String.length p > n
       && String.sub p 0 n = base
       && p.[n] = Filename.dir_sep.[0]

  let copy_file ~fs ~pkg ~optional ~exec ~src:src_s ~dst:dst_s =
    let perm = if exec then 0o755 else 0o644 in
    let src_path = Eio.Path.(fs / src_s) in
    let dst_path = Eio.Path.(fs / dst_s) in
    match Eio.Path.stat ~follow:true src_path with
    | exception Eio.Exn.Io _ ->
        if optional then false
        else
          Error.build_failed ~pkg ~cmd:"install"
            ~output:(Fmt.str "required source file not found: %s" src_s)
    | _ ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
          Eio.Path.(fs / Filename.dirname dst_s);
        (try
           Eio.Path.with_open_in src_path @@ fun i ->
           Eio.Path.with_open_out ~create:(`Or_truncate perm) dst_path
           @@ fun o -> Eio.Flow.copy i o
         with Eio.Exn.Io (e, _) ->
           Error.build_failed ~pkg ~cmd:"install"
             ~output:(Fmt.str "%s -> %s: %a" src_s dst_s Eio.Exn.pp_err e));
        (try Unix.chmod dst_s perm with Unix.Unix_error _ -> ());
        true

  let install_entry ~fs ~pkg ~build_dir ~dst_dir ~exec (base, dst_opt) =
    let base_s = OpamFilename.Base.to_string base.OpamTypes.c in
    let src_s = build_dir / base_s in
    let dst_name =
      match dst_opt with
      | Some d -> OpamFilename.Base.to_string d
      | None -> Filename.basename base_s
    in
    let dst_s = dst_dir / dst_name in
    let _ : bool =
      copy_file ~fs ~pkg ~optional:base.OpamTypes.optional ~exec ~src:src_s
        ~dst:dst_s
    in
    ()

  let install ~fs ~prefix ~build_dir ~install_file =
    let pkg = pkgname_of_install_file install_file in
    let inst =
      S.safe_read (OpamFile.make (OpamFilename.of_string install_file))
    in
    let pkg_dir sub = prefix / sub / pkg in
    let global_dir sub = prefix / sub in
    let sections =
      [
        (global_dir "bin", S.bin inst, true);
        (global_dir "sbin", S.sbin inst, true);
        (pkg_dir "lib", S.lib inst, false);
        (pkg_dir "lib", S.libexec inst, true);
        (global_dir "lib", S.lib_root inst, false);
        (global_dir "lib", S.libexec_root inst, true);
        (prefix / "lib" / "toplevel", S.toplevel inst, false);
        (prefix / "lib" / "stublibs", S.stublibs inst, true);
        (global_dir "man", S.man inst, false);
        (pkg_dir "share", S.share inst, false);
        (global_dir "share", S.share_root inst, false);
        (pkg_dir "etc", S.etc inst, false);
        (pkg_dir "doc", S.doc inst, false);
      ]
    in
    List.iter
      (fun (dst_dir, entries, exec) ->
        List.iter (install_entry ~fs ~pkg ~build_dir ~dst_dir ~exec) entries)
      sections;
    List.iter
      (fun (base, dst) ->
        let dst_s = OpamFilename.to_string dst in
        if is_under ~base:prefix dst_s then
          let base_s = OpamFilename.Base.to_string base.OpamTypes.c in
          let src_s = build_dir / base_s in
          let _ : bool =
            copy_file ~fs ~pkg ~optional:base.OpamTypes.optional ~exec:false
              ~src:src_s ~dst:dst_s
          in
          ()
        else
          ILog.warn (fun m ->
              m "%s: skipping misc file outside prefix: %s" pkg dst_s))
      (S.misc inst)
end

(* Cap concurrent package builds. Each in-flight build spawns subprocess
   pipes (2 fds per capture) plus transient file descriptors for fetch
   and patch, and each build then recursively spawns compiler processes
   of its own, so the fd tree fans out fast; large stages would exhaust
   macOS's default 256 soft [rlim]. Resolution order: explicit [?jobs]
   argument to {!run} wins, then [OI_BUILD_PARALLELISM] env var, then
   [min (cpu_count) 4]. *)
let default_build_parallelism () =
  match Sys.getenv_opt "OI_BUILD_PARALLELISM" with
  | Some s -> (
      match int_of_string_opt s with Some n when n > 0 -> n | _ -> 4)
  | None -> min (Domain.recommended_domain_count ()) 4

(* -- Failure logging ----------------------------------------------------- *)

(* Failed package builds get their output written to a file under the
   shared [<cache_root>/build/logs] dir instead of dumped to stderr.
   Keeps the live progress bar / summary readable and gives the user
   a file path to grep without re-running the build. The file name
   mirrors the [build_dir] convention: [build-<pkg>-<short_hash>.log]
   so the same [name.version] built in two different solve contexts
   doesn't collide. *)
let write_failure_log ~fs ~cache_root ~(p : Plan.package_plan) ~exn =
  let path =
    Cache.Logs.path ~cache_root ~kind:"build" ~name:p.pkg ~hash:p.layer_hash
  in
  let body =
    match exn with
    | Error.E (Build_failed { pkg; cmd; output }) ->
        Fmt.str "package: %s\ncommand: %s\n\n%s" pkg cmd output
    | _ -> Printexc.to_string exn
  in
  Cache.Logs.write ~fs ~cache_root path body;
  path

(* -- Command execution --------------------------------------------------- *)

(* Resolve an unqualified executable name against the PATH in [env],
   not the parent process's PATH. Eio.Process.spawn uses execvp-style
   lookups against the caller's PATH, so [ocaml] would otherwise resolve
   to the host opam switch's ocaml rather than the one installed into the
   build prefix. Resolving here forces exec to use our prefix's binary. *)
let path_of_env env =
  Array.find_map
    (fun s ->
      if String.starts_with ~prefix:"PATH=" s then
        Some (String.sub s 5 (String.length s - 5))
      else None)
    env

let is_executable path =
  try
    Unix.access path [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let resolve_in_path ~env exe =
  if String.contains exe '/' then exe
  else
    match path_of_env env with
    | None -> exe
    | Some path ->
        String.split_on_char ':' path
        |> List.find_map (function
          | "" -> None
          | d ->
              let candidate = d / exe in
              if is_executable candidate then Some candidate else None)
        |> Stdlib.Option.value ~default:exe

let find_in_path ~env exe =
  let s = resolve_in_path ~env exe in
  if s = exe && not (String.contains exe '/') then None else Some s

(* Many opam patches use GNU-patch features (e.g. unified context,
   `diff -ruN` of empty files) that BSD /usr/bin/patch on macOS rejects.
   If `gpatch` is on PATH (from Homebrew's gpatch / coreutils) we prefer
   it; otherwise fall back to `patch` and hope for the best. *)
let patch_cmd =
  lazy
    (let env = Unix.environment () in
     match find_in_path ~env "gpatch" with Some p -> p | None -> "patch")

let run_cmd ~proc_mgr ~fs ~env ~cwd ~pkg cmd =
  let cmd_s = String.concat " " cmd in
  Log.debug (fun m -> m "  + %s" cmd_s);
  (* Resolve relative executables (starting with ./) against cwd, and
     resolve bare names against the build env's PATH. *)
  let cmd =
    match cmd with
    | exe :: rest when String.length exe > 0 && exe.[0] = '.' ->
        (cwd / exe) :: rest
    | exe :: rest -> resolve_in_path ~env exe :: rest
    | [] -> cmd
  in
  Eio.Switch.run @@ fun sw ->
  (* Capture stdout+stderr to a single pipe so we can suppress output
     on success and show it in the Build_error on failure. Merging into
     one pipe preserves the interleaved order of the two streams. *)
  let r, w = Eio.Process.pipe ~sw proc_mgr in
  let child =
    Eio.Process.spawn ~sw proc_mgr ~env
      ~cwd:Eio.Path.(fs / cwd)
      ~stdout:w ~stderr:w cmd
  in
  (* Close the parent's copy of the write end so the reader sees EOF
     when the child exits. *)
  Eio.Flow.close w;
  let output =
    try Eio.Buf_read.(parse_exn take_all) r ~max_size:max_int
    with End_of_file -> ""
  in
  Eio.Flow.close r;
  match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n ->
      Error.build_failed ~pkg ~cmd:cmd_s
        ~output:(Fmt.str "exited with code %d\n\n%s" n output)
  | `Signaled n ->
      Error.build_failed ~pkg ~cmd:cmd_s
        ~output:(Fmt.str "killed by signal %d\n\n%s" n output)

(* -- Fetching ------------------------------------------------------------- *)

(* Per-fetch retry log so retry warnings go to a file instead of stderr.
   Retry.with_attempts opens the file in append mode on each retry, so
   the caller just has to supply the path and make sure the parent
   [logs/] directory exists. *)
let fetch_log_path_of ~cache_root ~(p : Plan.package_plan) =
  Cache.Logs.path ~cache_root ~kind:"fetch" ~name:p.pkg ~hash:p.layer_hash

let fetch_source ?(cache_urls = []) ~fs ~cache_root (p : Plan.package_plan) =
  match p.source with
  | None -> ()
  | Some src ->
      if not (Sys.file_exists p.build_dir) then begin
        let url = OpamUrl.parse ~handle_suffix:true src.url in
        let checksums = List.map OpamHash.of_string src.checksums in
        let dst_dir = OpamFilename.Dir.of_string p.build_dir in
        let cache_dir =
          OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
        in
        Log.info (fun m -> m "Fetching %s from %s" p.pkg src.url);
        let error_log_path = fetch_log_path_of ~cache_root ~p in
        Cache.Logs.ensure ~fs ~cache_root;
        try
          Retry.with_attempts ~label:(Fmt.str "fetch %s (%s)" p.pkg src.url)
            ~error_log_path (fun () ->
              let result =
                OpamRepository.pull_tree p.pkg ~cache_dir ~cache_urls dst_dir
                  checksums [ url ]
                |> OpamProcess.Job.run
              in
              match result with
              | OpamTypes.Result _ | OpamTypes.Up_to_date _ ->
                  (* Promote the just-fetched blob into our content-
                     addressed mirror so [oi build --export] picks it up
                     and registry consumers can fetch it offline. *)
                  let _ = Source.Mirror.promote ~fs ~cache_root checksums in
                  ()
              | OpamTypes.Not_available (_, msg) ->
                  Fmt.failwith "Failed to fetch %s: %s" p.pkg msg)
        with Failure msg ->
          (* Re-raise as a typed Fetch_failed so the build loop can
             classify the outcome distinctly from a Build_failed. *)
          Error.raise (Fetch_failed { url = src.url; msg })
      end

let fetch_extra_sources ?(cache_urls = []) ~fs ~cache_root
    (p : Plan.package_plan) =
  List.iter
    (fun (name, (src : Plan.source_info)) ->
      let dst = p.build_dir / name in
      if not (Sys.file_exists dst) then begin
        let url = OpamUrl.parse ~handle_suffix:true src.url in
        let checksums = List.map OpamHash.of_string src.checksums in
        let dst_file = OpamFilename.of_string dst in
        let cache_dir =
          OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
        in
        let error_log_path = fetch_log_path_of ~cache_root ~p in
        Cache.Logs.ensure ~fs ~cache_root;
        try
          Retry.with_attempts
            ~label:(Fmt.str "fetch extra source %s (%s)" name src.url)
            ~error_log_path (fun () ->
              let result =
                OpamRepository.pull_file name ~cache_dir ~cache_urls
                  ~silent_hits:true dst_file checksums [ url ]
                |> OpamProcess.Job.run
              in
              match result with
              | OpamTypes.Result () | OpamTypes.Up_to_date () ->
                  let _ = Source.Mirror.promote ~fs ~cache_root checksums in
                  ()
              | OpamTypes.Not_available (_, msg) -> Fmt.failwith "%s" msg)
        with Failure msg ->
          (* Match the previous semantics: extra sources are
             best-effort, so a hard failure (after retries) downgrades
             to a warning rather than aborting the whole build. *)
          Log.warn (fun m -> m "Failed to fetch extra source %s: %s" name msg)
      end)
    p.extra_sources

(* -- Patches and substitutions -------------------------------------------- *)

(* Copy opam [extra-files:] from their location next to the opam file
   into the package's build dir, where patches and other build steps
   expect to find them. opam itself does this implicitly when the
   recipe has [extra-files:]; oi has to wire it up because we don't
   stage sources through opam. *)
let copy_extra_files (p : Plan.package_plan) =
  List.iter
    (fun (basename, src) ->
      let dst = p.build_dir / basename in
      if Sys.file_exists src && not (Sys.file_exists dst) then begin
        (* Use a portable read+write rather than [cp] so we don't
           depend on shell tools mid-build. Patches are small text
           files; [Eio.Path.load] / [Eio.Path.save] handle them. *)
        let ic = open_in_bin src in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let len = in_channel_length ic in
            let bytes = really_input_string ic len in
            let oc = open_out_bin dst in
            Fun.protect
              ~finally:(fun () -> close_out_noerr oc)
              (fun () -> output_string oc bytes))
      end)
    p.extra_files

let apply_patches ~proc_mgr ~fs (p : Plan.package_plan) =
  List.iter
    (fun (patch : Plan.patch) ->
      let patch_file = p.build_dir / patch.file in
      if Sys.file_exists patch_file then
        run_cmd ~proc_mgr ~fs ~env:(Unix.environment ()) ~cwd:p.build_dir
          ~pkg:p.pkg
          [ Lazy.force patch_cmd; "-p1"; "-i"; patch_file ])
    p.patches

let apply_substs (p : Plan.package_plan) =
  (* Build an OpamFilter.env from the pre-computed subst_vars *)
  let env v =
    let key = OpamVariable.Full.to_string v in
    match List.assoc_opt key p.subst_vars with
    | Some s -> Some (OpamTypes.S s)
    | None -> None
  in
  List.iter
    (fun base ->
      let src = p.build_dir / (base ^ ".in") in
      let dst = p.build_dir / base in
      if Sys.file_exists src then
        OpamFilter.expand_interpolations_in_file_full env
          ~src:(OpamFilename.of_string src)
          ~dst:(OpamFilename.of_string dst))
    p.substs

(* -- Build and install ---------------------------------------------------- *)

(* Split into [fetch_phase] and [build_phase] so [Execute.run] can
   time them independently and attribute exceptions to the right phase
   (fetch vs build). [build_package] is preserved as the combined helper
   for callers that don't need per-phase timing. *)
let fetch_phase ?(cache_urls = []) ~fs ~cache_root (p : Plan.package_plan) =
  fetch_source ~cache_urls ~fs ~cache_root p;
  (* Ensure build_dir exists before fetching extra-sources: pull_tree
     (in fetch_source) creates the directory, but packages with no main
     source (e.g. seq.base) still need the directory to exist so that
     extra-source files can be written into it. *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / p.build_dir);
  fetch_extra_sources ~cache_urls ~fs ~cache_root p

let build_phase ~proc_mgr ~fs (p : Plan.package_plan) =
  copy_extra_files p;
  apply_patches ~proc_mgr ~fs p;
  apply_substs p;
  List.iter
    (fun cmd ->
      run_cmd ~proc_mgr ~fs ~env:p.env ~cwd:p.build_dir ~pkg:p.pkg cmd)
    p.build_commands

let install_package ~proc_mgr ~fs (p : Plan.package_plan) =
  List.iter
    (fun cmd ->
      run_cmd ~proc_mgr ~fs ~env:p.env ~cwd:p.build_dir ~pkg:p.pkg cmd)
    p.install_commands;
  if Sys.file_exists p.install_file then
    Installer.install ~fs ~prefix:p.prefix ~build_dir:p.build_dir
      ~install_file:p.install_file

(* -- Reporter ------------------------------------------------------------- *)

type pkg_event =
  | Started of { pkg : string; stage : int; total_stages : int }
  | Cached of { pkg : string }
  | Built of { pkg : string }
  | Build_failed of { pkg : string; log : string }
  | Dep_failed of { pkg : string; upstream_log : string }
  | Install_failed of { pkg : string; log : string }

type reporter = { pkg_event : pkg_event -> unit }

(* -- Main loop ------------------------------------------------------------ *)

(* Default reporter: internal Progress bar + inline FAIL prints. Used by
   [oi run] / [oi build], which only ever run one [Execute.run] at a time
   and have no outer UI to coordinate with. [oi build --all] supplies
   its own reporter to drive a cross-invocation bar with live counters. *)
let with_default_reporter ~clock ~total_packages ~n_stages f =
  Eio.Switch.run @@ fun sw ->
  Ui.run ~status_lines:0 ~sw ~clock ~total:total_packages ~title:"build"
  @@ fun ui ->
  let stage_s stage = Fmt.str "stage %d/%d" stage n_stages in
  let reporter =
    {
      pkg_event =
        (fun e ->
          match e with
          | Started { pkg; stage; total_stages = _ } ->
              Ui.with_msg ui (Fmt.str "[%s] %s" (stage_s stage) pkg)
          | Cached _ | Built _ -> Ui.tick ui
          | Dep_failed _ -> ()
          | Build_failed { pkg; log } ->
              Ui.suspend ui (fun () ->
                  Fmt.epr "  %a %s → %s@." Style.error_string "FAIL" pkg log)
          | Install_failed { pkg; log } ->
              Ui.suspend ui (fun () ->
                  Fmt.epr "  %a %s (install) → %s@." Style.error_string "FAIL"
                    pkg log));
    }
  in
  f reporter

(* -- Per-package trace for build_log JSON sidecars ----------------------- *)

module SSet = Set.Make (String)

type host_depext_status = {
  installed_set : SSet.t;
  missing_set : SSet.t;
  not_found_set : SSet.t;
}

(* Query the host once for the union of every plan-declared depext.
   Falls back to "everything missing" if the package manager is
   unreachable so the JSON still records a usable status without
   guessing. *)
let host_depext_status_of_plan plan : host_depext_status =
  let declared =
    List.fold_left
      (fun acc (g : Plan.group) ->
        List.fold_left
          (fun acc (p : Plan.package_plan) ->
            List.fold_left
              (fun acc d -> OpamSysPkg.Set.add (OpamSysPkg.of_string d) acc)
              acc p.depexts)
          acc g.packages)
      OpamSysPkg.Set.empty plan.Plan.groups
  in
  let { Depexts.installed; missing; not_found } = Depexts.status declared in
  let to_set s =
    OpamSysPkg.Set.fold
      (fun p acc -> SSet.add (OpamSysPkg.to_string p) acc)
      s SSet.empty
  in
  {
    installed_set = to_set installed;
    missing_set = to_set missing;
    not_found_set = to_set not_found;
  }

(* One [trace] per package attempt. The do_work loop creates it on
   [Started] and turns it into a [Build_log.t] sidecar at every terminal
   event, so success / cached / failure all leave a JSON record next to
   any text log. *)
type trace = {
  pkg : Plan.package_plan;
  started_at : float;
  mutable fetch_dur : float option;
  mutable build_dur : float option;
  mutable install_dur : float option;
  mutable restore_dur : float option;
  mutable text_log : string option; (* path to the .log when one exists *)
}

let new_trace ~now p =
  {
    pkg = p;
    started_at = now ();
    fetch_dur = None;
    build_dur = None;
    install_dur = None;
    restore_dur = None;
    text_log = None;
  }

let dep_to_log (d : Plan.dep_layer) : Build_log.dep =
  let id = Build_log.parse_pkg d.pkg in
  { name = id.name; version = id.version; hash = d.hash }

let plan_to_overlay (p : Plan.package_plan) : Build_log.overlay option =
  match (p.overlay_handle, p.overlay_version) with
  | Some h, Some v -> Some { handle = h; version = v }
  | _ -> None

let plan_to_source (p : Plan.package_plan) : Build_log.source_info option =
  Stdlib.Option.map
    (fun (s : Plan.source_info) : Build_log.source_info ->
      { url = s.url; checksums = s.checksums })
    p.source

let method_to_log = function
  | Plan.Source -> Build_log.Source
  | Plan.Binary -> Build_log.Binary

(* Classify an outer-loop exception into a Build_log.outcome. The
   [phase] argument disambiguates Build vs Install for non-Fetch
   errors (Fetch_failed is already typed). *)
let outcome_of_exn ~phase exn : Build_log.outcome =
  match exn with
  | Error.E (Fetch_failed { url; msg }) ->
      Fetch_failed { url; kind = Build_log.classify_fetch_msg msg }
  | Error.E (Build_failed { cmd; _ }) -> (
      match phase with
      | `Install -> Install_failed { command = cmd; exit_code = None }
      | _ -> Build_failed { command = cmd; exit_code = None })
  | _ ->
      let msg = Printexc.to_string exn in
      if phase = `Install then
        Install_failed { command = msg; exit_code = None }
      else Build_failed { command = msg; exit_code = None }

(* Per-package depexts. [installed]/[missing]/[not_found] are filled
   from a host-status snapshot computed once per [Execute.run] (host
   queries spawn a subprocess; per-package would be wasteful). The
   snapshot is keyed by pkg name; per-package buckets are derived by
   intersecting [declared] with the snapshot's three sets. *)
let per_pkg_depexts ~host_status (p : Plan.package_plan) : Build_log.depexts =
  let declared = p.depexts in
  if declared = [] then Build_log.empty_depexts
  else
    let set_of_strings = List.fold_left (fun s x -> SSet.add x s) SSet.empty in
    let declared_set = set_of_strings declared in
    let inter set = SSet.inter declared_set set |> SSet.elements in
    {
      declared;
      installed = inter host_status.installed_set;
      missing = inter host_status.missing_set;
      not_found = inter host_status.not_found_set;
    }

let write_trace ~fs ~cache_root ~os_key ~host_status ~outcome (t : trace) =
  let p = t.pkg in
  let id = Build_log.parse_pkg p.pkg in
  let now = Unix.gettimeofday () in
  let log =
    match t.text_log with
    | None -> None
    | Some path ->
        let tail =
          match outcome with
          | Build_log.Ok_built | Cached -> None
          | _ -> Build_log.tail_of_file ~path
        in
        Some Build_log.{ text_path = path; tail }
  in
  let record : Build_log.t =
    {
      schema = 1;
      pkg = id;
      layer_hash = p.layer_hash;
      os_key;
      method_ = method_to_log p.method_;
      started_at = t.started_at;
      duration_s = now -. t.started_at;
      outcome;
      deps = List.map dep_to_log p.dep_layers;
      depexts = per_pkg_depexts ~host_status p;
      source = plan_to_source p;
      log;
      overlay = plan_to_overlay p;
      phases =
        {
          fetch = t.fetch_dur;
          build = t.build_dur;
          install = t.install_dur;
          restore = t.restore_dur;
        };
    }
  in
  Build_log.write ~fs ~cache_root record

let run ?(cache_urls = []) ?jobs ?failed_layers ?reporter ~proc_mgr ~fs ~clock
    ~sys ~os_key plan =
  let build_parallelism =
    match jobs with Some n when n > 0 -> n | _ -> default_build_parallelism ()
  in
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Eio.Path.(fs / plan.Plan.cache_root); os_key }
  in
  let prefix = plan.Plan.build_prefix in
  let n_stages = List.length plan.groups in
  (* Track failed layer-hashes rather than package names so
     cross-overlay builds of the same [name.version] (which resolve to
     different layer hashes) stay independent. The optional arg lets
     [oi build --all] thread one tracker through every build
     group, so a failure in group 1 skips dependents in group 2
     rather than retrying the same build. Keyed by layer hash; value
     is the failure-log path (empty string for cascaded failures that
     inherit the log from an upstream dep). *)
  let failed_layers : (string, string) Hashtbl.t =
    match failed_layers with Some t -> t | None -> Hashtbl.create 16
  in
  (* Snapshot the pre-run count so the end-of-run raise only fires for
     failures introduced BY THIS CALL. Without this, every group after
     the first failure in an [--all] run re-reports that failure as
     its own (Execute.run would raise on each subsequent call because
     [failed_layers] still carries the earlier entry). *)
  let failed_count_before = Hashtbl.length failed_layers in
  let mark_failed ~(p : Plan.package_plan) ~log_path =
    if not (Hashtbl.mem failed_layers p.layer_hash) then
      Hashtbl.replace failed_layers p.layer_hash log_path
  in
  let is_dep_failed (d : Plan.dep_layer) = Hashtbl.mem failed_layers d.hash in
  (* First dep's log path, for cascading skip messages. *)
  let cascade_log (p : Plan.package_plan) =
    List.find_map
      (fun (d : Plan.dep_layer) -> Hashtbl.find_opt failed_layers d.hash)
      p.dep_layers
    |> Stdlib.Option.value ~default:""
  in
  (* A plan is "doomed" when every source package in it is already in
     [failed_layers] or has a failed dep, so no Source build will run.
     In that case every Binary in the plan would be restored into the
     prefix purely to emit [Cached] events — pure IO waste. Detect it
     up front and short-circuit: emit Cached / Dep_failed events for
     the reporter's accounting, mark sources as failed, and skip every
     prefix wipe / layer restore. This is the common case when a
     common dep fails early in [--all] and drags hundreds of groups
     with it. *)
  let is_pkg_doomed (p : Plan.package_plan) =
    p.method_ <> Plan.Source
    || Hashtbl.mem failed_layers p.layer_hash
    || List.exists is_dep_failed p.dep_layers
  in
  let plan_doomed =
    List.for_all
      (fun (g : Plan.group) -> List.for_all is_pkg_doomed g.packages)
      plan.groups
  in
  if not plan_doomed then begin
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix);
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / prefix);
    List.iter
      (fun sub ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / prefix / sub))
      [ "bin"; "lib"; "sbin"; "share"; "etc"; "doc"; "man" ];
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / plan.cache_root / "build" / "_build")
  end;
  (* Per-pkg trace store — looked up by layer_hash so phase 2 (build)
     and phase 3 (install) update the same record before the JSON
     sidecar is written at the terminal event. *)
  let traces : (string, trace) Hashtbl.t = Hashtbl.create 64 in
  let now () = Unix.gettimeofday () in
  let trace_for p =
    match Hashtbl.find_opt traces p.Plan.layer_hash with
    | Some t -> t
    | None ->
        let t = new_trace ~now p in
        Hashtbl.replace traces p.layer_hash t;
        t
  in
  let host_status = host_depext_status_of_plan plan in
  let emit_log ~outcome p =
    let t = trace_for p in
    write_trace ~fs ~cache_root:plan.cache_root ~os_key ~host_status ~outcome t
  in
  let do_work (reporter : reporter) =
    if plan_doomed then
      List.iter
        (fun (group : Plan.group) ->
          List.iter
            (fun (p : Plan.package_plan) ->
              match p.method_ with
              | Plan.Binary ->
                  reporter.pkg_event (Cached { pkg = p.pkg });
                  emit_log ~outcome:Cached p
              | Plan.Source ->
                  let upstream_log = cascade_log p in
                  mark_failed ~p ~log_path:upstream_log;
                  reporter.pkg_event (Dep_failed { pkg = p.pkg; upstream_log });
                  let upstream =
                    match
                      List.find_opt
                        (fun (d : Plan.dep_layer) ->
                          Hashtbl.mem failed_layers d.hash)
                        p.dep_layers
                    with
                    | Some d -> dep_to_log d
                    | None -> { name = ""; version = ""; hash = "" }
                  in
                  let t = trace_for p in
                  t.text_log <-
                    (if upstream_log = "" then None else Some upstream_log);
                  emit_log ~outcome:(Dep_failed { upstream }) p)
            group.packages)
        plan.groups
    else
      List.iter
        (fun (group : Plan.group) ->
          (* Phase 1: restore cached layers (Binary packages). Emit a
           [Started] before each restore so the progress bar label
           shows the package currently being restored, not the one
           that just finished. *)
          List.iter
            (fun (p : Plan.package_plan) ->
              if p.method_ = Plan.Binary then begin
                reporter.pkg_event
                  (Started
                     {
                       pkg = p.pkg;
                       stage = group.stage;
                       total_stages = n_stages;
                     });
                let t = trace_for p in
                let t0 = now () in
                D10.Layer.restore d10 ~hash:p.layer_hash ~prefix;
                t.restore_dur <- Some (now () -. t0);
                reporter.pkg_event (Cached { pkg = p.pkg });
                emit_log ~outcome:Cached p
              end)
            group.packages;
          (* Phase 2: parallel fetch+build for Source packages. Skip
           a package when its own layer has already failed in a
           previous build group (persistent tracker), or when any
           of its dep layers is known-failed. In either case we
           mark the package's own layer as failed so its dependents
           propagate the skip downstream and emit a Dep_failed
           event for the reporter. *)
          let to_build =
            List.filter
              (fun (p : Plan.package_plan) ->
                if
                  p.method_ = Plan.Source
                  && (not (Hashtbl.mem failed_layers p.layer_hash))
                  && not (List.exists is_dep_failed p.dep_layers)
                then true
                else begin
                  if p.method_ = Plan.Source then begin
                    let upstream_log = cascade_log p in
                    let upstream =
                      match
                        List.find_opt
                          (fun (d : Plan.dep_layer) ->
                            Hashtbl.mem failed_layers d.hash)
                          p.dep_layers
                      with
                      | Some d -> dep_to_log d
                      | None -> { name = ""; version = ""; hash = "" }
                    in
                    mark_failed ~p ~log_path:upstream_log;
                    reporter.pkg_event
                      (Dep_failed { pkg = p.pkg; upstream_log });
                    let t = trace_for p in
                    t.text_log <-
                      (if upstream_log = "" then None else Some upstream_log);
                    emit_log ~outcome:(Dep_failed { upstream }) p
                  end;
                  false
                end)
              group.packages
          in
          if to_build <> [] then begin
            let active = ref 0 in
            Eio.Fiber.List.iter ~max_fibers:build_parallelism
              (fun (p : Plan.package_plan) ->
                active := !active + 1;
                reporter.pkg_event
                  (Started
                     {
                       pkg = p.pkg;
                       stage = group.stage;
                       total_stages = n_stages;
                     });
                let t = trace_for p in
                (try
                   Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / p.build_dir);
                   let t0 = now () in
                   fetch_phase ~cache_urls ~fs ~cache_root:plan.cache_root p;
                   t.fetch_dur <- Some (now () -. t0);
                   let t1 = now () in
                   build_phase ~proc_mgr ~fs p;
                   t.build_dur <- Some (now () -. t1)
                 with exn ->
                   let log_path =
                     write_failure_log ~fs ~cache_root:plan.cache_root ~p ~exn
                   in
                   t.text_log <- Some log_path;
                   let outcome = outcome_of_exn ~phase:`Build exn in
                   mark_failed ~p ~log_path;
                   reporter.pkg_event
                     (Build_failed { pkg = p.pkg; log = log_path });
                   emit_log ~outcome p);
                active := !active - 1)
              to_build
          end;
          (* Phase 3: serial install for successfully built packages.
           Emit [Started] before each install so the bar label tracks
           the in-flight package. *)
          List.iter
            (fun (p : Plan.package_plan) ->
              if
                p.method_ = Plan.Source
                && not (Hashtbl.mem failed_layers p.layer_hash)
              then begin
                reporter.pkg_event
                  (Started
                     {
                       pkg = p.pkg;
                       stage = group.stage;
                       total_stages = n_stages;
                     });
                let before = D10.Prefix.snapshot ~fs prefix in
                let t = trace_for p in
                try
                  let t0 = now () in
                  install_package ~proc_mgr ~fs p;
                  t.install_dur <- Some (now () -. t0);
                  let files =
                    D10.Prefix.diff ~fs ~prefix ~before |> List.map fst
                  in
                  let dep_hashes =
                    List.filter_map
                      (fun (d : Plan.dep_layer) ->
                        if D10.Layer.succeeded d10 ~hash:d.hash then Some d.hash
                        else None)
                      p.dep_layers
                  in
                  D10.Layer.store d10 ~hash:p.layer_hash ~prefix ~files
                    ~package:p.pkg
                    ~deps:
                      (List.map
                         (fun (d : Plan.dep_layer) -> d.pkg)
                         p.dep_layers)
                    ~parent_hashes:dep_hashes ~exit_status:0
                    ?overlay_handle:p.overlay_handle
                    ?overlay_version:p.overlay_version ();
                  reporter.pkg_event (Built { pkg = p.pkg });
                  emit_log ~outcome:Ok_built p
                with exn ->
                  let log_path =
                    write_failure_log ~fs ~cache_root:plan.cache_root ~p ~exn
                  in
                  t.text_log <- Some log_path;
                  let outcome = outcome_of_exn ~phase:`Install exn in
                  mark_failed ~p ~log_path;
                  reporter.pkg_event
                    (Install_failed { pkg = p.pkg; log = log_path });
                  emit_log ~outcome p
              end)
            group.packages)
        plan.groups
  in
  (match reporter with
  | Some r -> do_work r
  | None ->
      with_default_reporter
        ~clock:(clock :> _ Eio.Time.clock)
        ~total_packages:plan.total_packages ~n_stages do_work);
  let n_failed = Hashtbl.length failed_layers - failed_count_before in
  if n_failed > 0 then Error.msg "%d package(s) failed to build" n_failed
