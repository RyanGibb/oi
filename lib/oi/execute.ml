[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** DAG-driven parallel build executor. *)

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
   of its own, so the fd tree fans out fast; a wide source-build batch
   would exhaust macOS's default 256 soft [rlim]. Resolution order:
   explicit [?jobs] argument to {!run} wins, then [OI_BUILD_PARALLELISM]
   env var, then [min (cpu_count) 4]. *)
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

(* Render a [Source.Mirror.origin] for the "Fetching X from Y" log line.
   We can only tell for sure when the local mirror has the blob (a free
   filesystem check); for anything else opam's [pull_tree] resolves the
   source via [cache_urls] and we fall back to logging the package's
   canonical URL. *)
let describe_origin ~src_url : Source.Mirror.origin -> string = function
  | Local_mirror path -> Fmt.str "local mirror (%s)" path
  | Other -> src_url

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
        let origin = Source.Mirror.source_origin ~cache_urls ~checksums in
        Log.info (fun m ->
            m "Fetching %s from %s" p.pkg
              (describe_origin ~src_url:src.url origin));
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
                  let _ = Source.Mirror.import_from_opam_cache ~fs ~cache_root checksums in
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
        let origin = Source.Mirror.source_origin ~cache_urls ~checksums in
        Log.info (fun m ->
            m "Fetching extra source %s for %s from %s" name p.pkg
              (describe_origin ~src_url:src.url origin));
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
                  let _ = Source.Mirror.import_from_opam_cache ~fs ~cache_root checksums in
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

let apply_substs ~subst_vars (p : Plan.package_plan) =
  (* Build an OpamFilter.env from the (rebased) subst_vars. *)
  let env v =
    let key = OpamVariable.Full.to_string v in
    match List.assoc_opt key subst_vars with
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

(* Per-package rebased view: planning-time prefix (the same path baked
   into every package's commands by [Plan.elaborate]) is replaced with this
   package's staging dir. The substitution is plain string replacement on
   the planning-prefix path — the planning prefix is a deterministic,
   unique path under [<cache_root>/build/prefix] so accidental matches
   inside other strings are vanishingly unlikely. *)
type staged = {
  prefix : string;
  env : string array;
  build_commands : string list list;
  install_commands : string list list;
  subst_vars : (string * string) list;
}

let rebase_string ~from_prefix ~to_prefix s =
  if from_prefix = to_prefix then s
  else
    let n = String.length from_prefix in
    let len = String.length s in
    if n = 0 || n > len then s
    else
      let out = Buffer.create len in
      let i = ref 0 in
      while !i < len do
        if !i + n <= len && String.sub s !i n = from_prefix then begin
          Buffer.add_string out to_prefix;
          i := !i + n
        end
        else begin
          Buffer.add_char out s.[!i];
          incr i
        end
      done;
      Buffer.contents out

let stage_pkg (p : Plan.package_plan) ~staging =
  let r = rebase_string ~from_prefix:p.prefix ~to_prefix:staging in
  {
    prefix = staging;
    env = Array.map r p.env;
    build_commands = List.map (List.map r) p.build_commands;
    install_commands = List.map (List.map r) p.install_commands;
    subst_vars = List.map (fun (k, v) -> (k, r v)) p.subst_vars;
  }

let build_phase ~proc_mgr ~fs ~staged (p : Plan.package_plan) =
  copy_extra_files p;
  apply_patches ~proc_mgr ~fs p;
  apply_substs ~subst_vars:staged.subst_vars p;
  List.iter
    (fun cmd ->
      run_cmd ~proc_mgr ~fs ~env:staged.env ~cwd:p.build_dir ~pkg:p.pkg cmd)
    staged.build_commands

let install_package ~proc_mgr ~fs ~staged (p : Plan.package_plan) =
  List.iter
    (fun cmd ->
      run_cmd ~proc_mgr ~fs ~env:staged.env ~cwd:p.build_dir ~pkg:p.pkg cmd)
    staged.install_commands;
  if Sys.file_exists p.install_file then
    Installer.install ~fs ~prefix:staged.prefix ~build_dir:p.build_dir
      ~install_file:p.install_file

(* -- Reporter ------------------------------------------------------------- *)

type pkg_event =
  | Started of { pkg : string; phase : string }
  | Cached of { pkg : string }
  | Built of { pkg : string }
  | Build_failed of { pkg : string; log : string }
  | Dep_failed of { pkg : string; upstream_log : string }
  | Install_failed of { pkg : string; log : string }

type reporter = { pkg_event : pkg_event -> unit }

(* -- Main loop ------------------------------------------------------------ *)

(* Build-phase UI matching the fetch UI in [Pipeline.fetch_with_display]
   so the [oi build] / [oi run] flow looks like one tool: bold "Build"
   header rpad'd to {!Ui.row_label_width}, then the same {!Ui.row_bar_width}-cell
   bar fetch uses; per-package rows are pkg name fitted to the same
   width plus a spinner and a trailing "<phase> (Xs)" status string. *)

let agg_build_line ~total =
  let open Progress.Line in
  let header =
    constf "%a"
      Fmt.(styled `Bold string)
      (Printf.sprintf "%-*s" Ui.row_label_width "Build")
  in
  list ~sep:(const " ")
    [
      header;
      Ui.Theme.bar ~width:(`Fixed Ui.row_bar_width) total;
      count_to total;
      sum ~pp:(Ui.Theme.pct_pp ~total) ~width:6 ();
    ]

let pkg_build_line ~pkg =
  let open Progress.Line in
  list ~sep:(const " ")
    [ const (Ui.fit_label Ui.row_label_width pkg); Ui.Theme.spinner (); string ]

(* Default reporter: aggregate "Build M/N" bar plus one row per
   in-flight package, with inline FAIL prints when builds bail. Used
   by [oi run] / [oi build], which only ever run one [Execute.run]
   at a time. [oi build --all] supplies its own reporter for a
   cross-invocation bar.

   When [shared_display] is supplied (the unified UI from
   {!Ui.Preflight.with_bar}), attach to it so the overall progress
   bar above stays visible across the build phase. Otherwise own a
   fresh [Display].

   A heartbeat fiber refreshes the per-package row text once a
   second so the elapsed counter ticks even when a long compile (e.g.
   ocaml-base-compiler at >60s) hasn't yielded. *)
let with_default_reporter ?shared_display ~clock ~total_packages f =
  if not (Tty.is_tty ()) then begin
    let log_event = function
      | Started { pkg; phase } ->
          Logs.info (fun m -> m "build: %s %s" phase pkg)
      | Cached { pkg } -> Logs.info (fun m -> m "build: cached %s" pkg)
      | Built { pkg } -> Logs.info (fun m -> m "build: built %s" pkg)
      | Dep_failed _ -> ()
      | Build_failed { pkg; log } ->
          Fmt.epr "  %a %s → %s@." Style.error_string "FAIL" pkg log
      | Install_failed { pkg; log } ->
          Fmt.epr "  %a %s (install) → %s@." Style.error_string "FAIL" pkg log
    in
    f { pkg_event = log_event }
  end
  else begin
    Eio.Switch.run @@ fun sw ->
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
      Progress.Display.add_line display (agg_build_line ~total:total_packages)
    in
    let pkg_handles : (string, string Progress.Reporter.t) Hashtbl.t =
      Hashtbl.create 8
    in
    let pkg_started_at : (string, float) Hashtbl.t = Hashtbl.create 8 in
    let pkg_phase : (string, string) Hashtbl.t = Hashtbl.create 8 in
    let lock = Mutex.create () in
    let with_lock f = Mutex.protect lock f in
    let now () = Unix.gettimeofday () in
    let stopped = ref false in
    let render_pkg pkg =
      match Hashtbl.find_opt pkg_handles pkg with
      | None -> ()
      | Some r -> (
          let phase =
            Hashtbl.find_opt pkg_phase pkg |> Stdlib.Option.value ~default:""
          in
          let elapsed =
            match Hashtbl.find_opt pkg_started_at pkg with
            | Some t0 -> now () -. t0
            | None -> 0.0
          in
          try Progress.Reporter.report r (Fmt.str "%s (%.0fs)" phase elapsed)
          with _ -> ())
    in
    (* Heartbeat at 100ms: animates spinner ([Display.tick]) when we
       own the display, plus refreshes per-package row text every 10
       ticks (1s) so [(Xs)] advances even when compilers are silent.
       Shared mode skips the [Display.tick] (the owner already runs
       one). *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        let n = ref 0 in
        let rec loop () =
          Eio.Time.sleep clock 0.1;
          if !stopped then `Stop_daemon
          else begin
            (if owns_display then
               try Progress.Display.tick display with _ -> ());
            incr n;
            if !n mod 10 = 0 then
              with_lock (fun () ->
                  Hashtbl.iter (fun pkg _ -> render_pkg pkg) pkg_handles);
            loop ()
          end
        in
        loop ());
    let on_started pkg phase =
      with_lock @@ fun () ->
      if not (Hashtbl.mem pkg_handles pkg) then begin
        let r = Progress.Display.add_line display (pkg_build_line ~pkg) in
        Hashtbl.add pkg_handles pkg r;
        Hashtbl.add pkg_started_at pkg (now ())
      end;
      Hashtbl.replace pkg_phase pkg phase;
      render_pkg pkg
    in
    let on_done pkg =
      with_lock @@ fun () ->
      match Hashtbl.find_opt pkg_handles pkg with
      | None -> ()
      | Some r ->
          (try Progress.Reporter.finalise r with _ -> ());
          (try Progress.Display.remove_line display r with _ -> ());
          Hashtbl.remove pkg_handles pkg;
          Hashtbl.remove pkg_started_at pkg;
          Hashtbl.remove pkg_phase pkg
    in
    let report_event = function
      | Started { pkg; phase } -> on_started pkg phase
      | Cached { pkg } -> (
          on_done pkg;
          try Progress.Reporter.report agg_handle 1 with _ -> ())
      | Built { pkg } -> (
          on_done pkg;
          try Progress.Reporter.report agg_handle 1 with _ -> ())
      | Dep_failed { pkg; _ } -> on_done pkg
      | Build_failed { pkg; log } -> (
          on_done pkg;
          (try Progress.Display.pause display with _ -> ());
          Fmt.epr "  %a %s → %s@." Style.error_string "FAIL" pkg log;
          try Progress.Display.resume display with _ -> ())
      | Install_failed { pkg; log } -> (
          on_done pkg;
          (try Progress.Display.pause display with _ -> ());
          Fmt.epr "  %a %s (install) → %s@." Style.error_string "FAIL" pkg log;
          try Progress.Display.resume display with _ -> ())
    in
    Fun.protect
      ~finally:(fun () ->
        stopped := true;
        (try Progress.Reporter.finalise agg_handle with _ -> ());
        (try Progress.Display.remove_line display agg_handle with _ -> ());
        if owns_display then
          try Progress.Display.finalise display with _ -> ())
      (fun () -> f { pkg_event = report_event })
  end

(* -- Per-package trace for Audit + Provenance writes -------------------- *)

(* One [trace] per package attempt. The do_work loop creates it on
   [Started] and emits an [Audit] event at every terminal event (success /
   cached / failure all leave a JSON line in the audit log). On a fresh
   [Ok] it also writes a [Provenance.json] beside the just-committed layer.
   The two writes are split because a layer's content provenance is
   immutable — only the audit log records the per-caller view. *)
type trace = {
  pkg : Plan.package_plan;
  started_at : float;
  mutable fetch_dur : float option;
  mutable build_dur : float option;
  mutable install_dur : float option;
  restore_dur : float option;
      (** Always [None] in the staging-based scheduler — Binary packages resolve
          immediately without a separate restore phase, since dependents
          hardlink the dep's [fs/] tree directly out of the cache when they
          materialise their own staging dir. Kept on [trace] for
          [Provenance.phases] backward compatibility. *)
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

(* Classify an outer-loop exception into an Outcome.t. The [phase] argument
   disambiguates Build vs Install for non-Fetch errors. *)
let outcome_of_exn ~phase exn : Outcome.t =
  match exn with
  | Error.E (Fetch_failed { url; msg }) ->
      Fetch_failed { url; kind = Outcome.classify_fetch_msg msg }
  | Error.E (Build_failed { cmd; _ }) -> (
      match phase with
      | `Install -> Install_failed { command = cmd; exit_code = None }
      | _ -> Build_failed { command = cmd; exit_code = None })
  | _ ->
      let msg = Printexc.to_string exn in
      if phase = `Install then
        Install_failed { command = msg; exit_code = None }
      else Build_failed { command = msg; exit_code = None }

(* Build the Audit + (optional) Provenance for a terminal package event. The
   [audit_base] carries trigger / project / toolchain / host; per-pkg overlay
   is folded in here. Provenance writes only fire on [Ok], because that's
   the only outcome that committed a layer dir to disk. *)
let phases_of_trace (t : trace) : Provenance.phases =
  {
    fetch = t.fetch_dur;
    build = t.build_dur;
    install = t.install_dur;
    restore = t.restore_dur;
  }

let opam_info_of_plan (p : Plan.package_plan) ~name ~version :
    Provenance.opam_info =
  match (p.opam_path, p.pkgs_dir) with
  | Some path, Some pkgs_dir ->
      let full = if version = "" then name else Fmt.str "%s.%s" name version in
      {
        sha256 = Provenance.hash_opam_file ~path;
        origin = Origin.of_packages_dir ~pkgs_dir ~name ~full;
      }
  | _ ->
      {
        sha256 = "";
        origin = { kind = Local; overlay = None; path_in_repo = "" };
      }

let provenance_source_of_plan (p : Plan.package_plan) :
    Provenance.source_info option =
  Option.map
    (fun (s : Plan.source_info) : Provenance.source_info ->
      {
        url = s.url;
        kind = Provenance.url_kind s.url;
        checksums = s.checksums;
      })
    p.source

let emit_event ~fs ~cache_root ~os_key ~ocaml_version ~audit_base ~outcome
    (t : trace) =
  let p = t.pkg in
  let id = Identity.of_string p.pkg in
  let now = Unix.gettimeofday () in
  let duration_s = now -. t.started_at in
  let log =
    Option.map
      (fun log_path ->
        let tail =
          match (outcome : Outcome.t) with
          | Ok | Cached | Restored -> None
          | _ -> Audit.tail_of_file ~path:log_path
        in
        { Audit.text_path = log_path; tail })
      t.text_log
  in
  let context = { audit_base with Audit.overlay = p.overlay } in
  let event : Audit.event =
    {
      schema = 1;
      event_id = Audit.ulid ();
      invocation_id = Audit.invocation_id ();
      ts = now;
      os_key;
      target = Layer p.layer_hash;
      pkg = id;
      outcome;
      duration_s;
      context;
      log;
    }
  in
  Audit.append ~fs ~cache_root event;
  if outcome = Outcome.Ok then begin
    let prov : Provenance.t =
      {
        schema = 1;
        layer_hash = p.layer_hash;
        os_key;
        pkg = id;
        method_ = p.method_;
        built_at = now;
        duration_s;
        phases = phases_of_trace t;
        opam = opam_info_of_plan p ~name:id.name ~version:id.version;
        source = provenance_source_of_plan p;
        deps = p.dep_layers;
        depexts_declared = p.depexts;
        build_env = { ocaml_version };
      }
    in
    Provenance.write ~fs ~cache_root prov
  end

let run ?(cache_urls = []) ?jobs ?failed_layers ?reporter ?audit_base
    ?shared_display ~proc_mgr ~fs ~clock ~sys ~os_key plan =
  let build_parallelism =
    match jobs with Some n when n > 0 -> n | _ -> default_build_parallelism ()
  in
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Eio.Path.(fs / plan.Plan.cache_root); os_key }
  in
  (* Track failed layer-hashes rather than package names so
     cross-overlay builds of the same [name.version] (which resolve to
     different layer hashes) stay independent. The optional arg lets
     [oi build --all] thread one tracker through every solve group, so
     a failure in one solve skips dependents in later solves rather
     than retrying the same build. Keyed by layer hash; value is the
     failure-log path (empty string for cascaded failures that inherit
     the log from an upstream dep). *)
  let failed_layers : (string, string) Hashtbl.t =
    match failed_layers with Some t -> t | None -> Hashtbl.create 16
  in
  (* Snapshot the pre-run count so the end-of-run raise only fires for
     failures introduced BY THIS CALL. Without this, every solve after
     the first failure in an [--all] run re-reports that failure as
     its own ([Execute.run] would raise on each subsequent call because
     [failed_layers] still carries the earlier entry). *)
  let failed_count_before = Hashtbl.length failed_layers in
  let mark_failed ~hash ~log_path =
    if not (Hashtbl.mem failed_layers hash) then
      Hashtbl.replace failed_layers hash log_path
  in
  let cascade_log (p : Plan.package_plan) =
    List.find_map
      (fun (d : Identity.dep) -> Hashtbl.find_opt failed_layers d.hash)
      p.dep_layers
    |> Stdlib.Option.value ~default:""
  in
  let cascade_upstream (p : Plan.package_plan) : Identity.dep =
    match
      List.find_opt
        (fun (d : Identity.dep) -> Hashtbl.mem failed_layers d.hash)
        p.dep_layers
    with
    | Some d -> d
    | None -> { id = { name = ""; version = "" }; hash = "" }
  in
  (* A plan is "doomed" when no Source package can run: every Source pkg
     either already failed (its layer hash is in [failed_layers]) or has
     a failed dep. In that case the only useful outcome is to emit
     synthetic events so the reporter's counters stay correct, and skip
     all I/O. *)
  let is_pkg_doomed (p : Plan.package_plan) =
    p.method_ <> Identity.Source
    || Hashtbl.mem failed_layers p.layer_hash
    || List.exists
         (fun (d : Identity.dep) -> Hashtbl.mem failed_layers d.hash)
         p.dep_layers
  in
  let plan_doomed = List.for_all is_pkg_doomed plan.packages in
  if not plan_doomed then begin
    (* Fresh staging root per run — we keep [<cache>/build/staging/]
       around between runs so an interrupted build leaves its dir for
       inspection, but we don't wipe other packages' dirs at start. *)
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / plan.cache_root / "build" / "staging");
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / plan.cache_root / "build" / "_build")
  end;
  (* Per-pkg trace store — looked up by layer_hash so the fetch / build
     / install phases all update the same record before the JSON
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
  let audit_base =
    match audit_base with Some c -> c | None -> Audit.default_context ()
  in
  let ocaml_version = plan.ocaml_version in
  let emit_log ~outcome p =
    let t = trace_for p in
    emit_event ~fs ~cache_root:plan.cache_root ~os_key ~ocaml_version
      ~audit_base ~outcome t
  in
  let emit_dep_failed (reporter : reporter) (p : Plan.package_plan) =
    let upstream_log = cascade_log p in
    let upstream = cascade_upstream p in
    mark_failed ~hash:p.layer_hash ~log_path:upstream_log;
    reporter.pkg_event (Dep_failed { pkg = p.pkg; upstream_log });
    let t = trace_for p in
    t.text_log <- (if upstream_log = "" then None else Some upstream_log);
    emit_log ~outcome:(Dep_failed { upstream }) p
  in
  let do_work (reporter : reporter) =
    if plan_doomed then
      List.iter
        (fun (p : Plan.package_plan) ->
          match p.method_ with
          | Identity.Binary ->
              reporter.pkg_event (Cached { pkg = p.pkg });
              emit_log ~outcome:Restored p
          | Source -> emit_dep_failed reporter p)
        plan.packages
    else begin
      (* DAG scheduler with per-package staging dirs.

         Every package gets its own [staging] dir keyed by [layer_hash].
         Dep layers are hardlink-restored into [staging] from the d10
         cache; the package's build / install commands run with planning
         prefix → staging rebased in env and argv; install captures only
         this package's files via snapshot/diff against the dep-layer
         baseline. No shared prefix means no global mutex — restores and
         installs both parallelise across packages.

         Promises are resolved as [`Ok] once the package's layer is
         committed (Source) or known to be in the cache (Binary —
         immediately, since the layer dir already exists). Dependents
         await dep promises and then hardlink the dep layers' [fs/]
         trees out of the cache directly. *)
      let promises :
          ( string,
            [ `Ok | `Failed ] Eio.Promise.t * [ `Ok | `Failed ] Eio.Promise.u
          )
          Hashtbl.t =
        Hashtbl.create (List.length plan.packages)
      in
      List.iter
        (fun (p : Plan.package_plan) ->
          if not (Hashtbl.mem promises p.layer_hash) then
            Hashtbl.replace promises p.layer_hash (Eio.Promise.create ()))
        plan.packages;
      let resolve hash status =
        match Hashtbl.find_opt promises hash with
        | Some (_, u) -> Eio.Promise.resolve u status
        | None -> ()
      in
      let await_dep (d : Identity.dep) : [ `Ok | `Failed ] =
        match Hashtbl.find_opt promises d.hash with
        | Some (prom, _) -> Eio.Promise.await prom
        | None ->
            (* Dep is outside this plan: either already cached and not
               re-listed, or pre-failed in [failed_layers]. Trust
               [failed_layers] as the source of truth. *)
            if Hashtbl.mem failed_layers d.hash then `Failed else `Ok
      in
      let build_sem = Eio.Semaphore.make build_parallelism in
      let started phase (p : Plan.package_plan) =
        reporter.pkg_event (Started { pkg = p.pkg; phase })
      in
      (* Precompute the transitive in-plan dep closure for every package.
         Each [package_plan.dep_layers] only carries direct deps, but the
         build needs every transitive dep's [fs/] tree (e.g. cohttp's
         [dune build] reads [Makefile.config] from the OCaml compiler
         layer, which sits behind several intermediate deps). Walking
         [plan.packages] in topo order lets us build each closure
         incrementally — by the time we resolve P, every dep's closure
         is already in [closures]. *)
      let closures : (string, string list) Hashtbl.t =
        Hashtbl.create (List.length plan.packages)
      in
      List.iter
        (fun (p : Plan.package_plan) ->
          let acc = Hashtbl.create 16 in
          List.iter
            (fun (d : Identity.dep) ->
              Hashtbl.replace acc d.hash ();
              match Hashtbl.find_opt closures d.hash with
              | None -> ()
              | Some ds -> List.iter (fun h -> Hashtbl.replace acc h ()) ds)
            p.dep_layers;
          let hashes = Hashtbl.fold (fun h () acc -> h :: acc) acc [] in
          Hashtbl.replace closures p.layer_hash hashes)
        plan.packages;
      let transitive_dep_hashes (p : Plan.package_plan) =
        Stdlib.Option.value (Hashtbl.find_opt closures p.layer_hash) ~default:[]
      in
      (* Materialise this package's staging dir: an empty directory
         seeded with the standard prefix subdirs and the [fs/] tree of
         every successfully-cached transitive dep layer hardlinked in.
         The result is what the package's commands see when [%{prefix}%]
         resolves (after [stage_pkg] rebases the planning sentinel).
         Failures here surface as a layer-restore exception per dep —
         same semantics as the old shared-prefix [restore]. *)
      let materialise_staging (p : Plan.package_plan) =
        let staging = Plan.staging_dir plan p in
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / staging);
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / staging);
        List.iter
          (fun sub ->
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
              Eio.Path.(fs / staging / sub))
          [ "bin"; "lib"; "sbin"; "share"; "etc"; "doc"; "man" ];
        List.iter
          (fun hash ->
            if D10.Layer.succeeded d10 ~hash then
              D10.Layer.restore d10 ~hash ~prefix:staging)
          (transitive_dep_hashes p);
        staging
      in
      let cleanup_staging staging =
        try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / staging)
        with _ -> ()
      in
      let pkg_fiber (p : Plan.package_plan) =
        let dep_status =
          List.fold_left
            (fun acc d ->
              match (acc, await_dep d) with
              | `Failed, _ | _, `Failed -> `Failed
              | `Ok, `Ok -> `Ok)
            `Ok p.dep_layers
        in
        let pre_failed = Hashtbl.mem failed_layers p.layer_hash in
        if dep_status = `Failed || pre_failed then begin
          if p.method_ = Identity.Source then emit_dep_failed reporter p;
          resolve p.layer_hash `Failed
        end
        else
          match p.method_ with
          | Identity.Binary ->
              (* Binary: the layer dir already exists in the d10 cache.
                 Dependents will hardlink it from there at their own
                 [materialise_staging] step; we have nothing to do. *)
              reporter.pkg_event (Cached { pkg = p.pkg });
              emit_log ~outcome:Restored p;
              resolve p.layer_hash `Ok
          | Source -> (
              let phase = ref `Build in
              let t = trace_for p in
              let with_build_slot f =
                Eio.Semaphore.acquire build_sem;
                Fun.protect
                  ~finally:(fun () -> Eio.Semaphore.release build_sem)
                  f
              in
              try
                started "fetch" p;
                Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / p.build_dir);
                let t0 = now () in
                fetch_phase ~cache_urls ~fs ~cache_root:plan.cache_root p;
                t.fetch_dur <- Some (now () -. t0);
                let staging = materialise_staging p in
                let staged = stage_pkg p ~staging in
                Fun.protect
                  ~finally:(fun () -> cleanup_staging staging)
                  (fun () ->
                    started "build" p;
                    with_build_slot (fun () ->
                        let t1 = now () in
                        build_phase ~proc_mgr ~fs ~staged p;
                        t.build_dur <- Some (now () -. t1));
                    phase := `Install;
                    started "install" p;
                    let before = D10.Prefix.snapshot ~fs staging in
                    let t0 = now () in
                    install_package ~proc_mgr ~fs ~staged p;
                    t.install_dur <- Some (now () -. t0);
                    let files =
                      D10.Prefix.diff ~fs ~prefix:staging ~before
                      |> List.map fst
                    in
                    let dep_hashes =
                      List.filter_map
                        (fun (d : Identity.dep) ->
                          if D10.Layer.succeeded d10 ~hash:d.hash then
                            Some d.hash
                          else None)
                        p.dep_layers
                    in
                    D10.Layer.store d10 ~hash:p.layer_hash ~prefix:staging
                      ~files ~package:p.pkg
                      ~deps:
                        (List.map
                           (fun (d : Identity.dep) -> Identity.to_string d.id)
                           p.dep_layers)
                      ~parent_hashes:dep_hashes ~exit_status:0);
                reporter.pkg_event (Built { pkg = p.pkg });
                emit_log ~outcome:Ok p;
                resolve p.layer_hash `Ok
              with exn ->
                let log_path =
                  write_failure_log ~fs ~cache_root:plan.cache_root ~p ~exn
                in
                t.text_log <- Some log_path;
                let outcome = outcome_of_exn ~phase:!phase exn in
                mark_failed ~hash:p.layer_hash ~log_path;
                (match !phase with
                | `Install ->
                    reporter.pkg_event
                      (Install_failed { pkg = p.pkg; log = log_path })
                | _ ->
                    reporter.pkg_event
                      (Build_failed { pkg = p.pkg; log = log_path }));
                emit_log ~outcome p;
                resolve p.layer_hash `Failed)
      in
      Eio.Switch.run @@ fun sw ->
      List.iter
        (fun p -> Eio.Fiber.fork ~sw (fun () -> pkg_fiber p))
        plan.packages
    end
  in
  (match reporter with
  | Some r -> do_work r
  | None ->
      with_default_reporter ?shared_display
        ~clock:(clock :> _ Eio.Time.clock)
        ~total_packages:(List.length plan.packages)
        do_work);
  let n_failed = Hashtbl.length failed_layers - failed_count_before in
  if n_failed > 0 then Error.msg "%d package(s) failed to build" n_failed
