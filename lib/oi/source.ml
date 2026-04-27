[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

(* -- Opam repository clones (was lib/oi/repo.ml) ------------------------- *)

module Repo = struct
  let log_src = Logs.Src.create "oi.source.repo"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  let pull_repo ~label ~url_s ~dst =
    let url = OpamUrl.parse ~handle_suffix:true url_s in
    let url = { url with OpamUrl.backend = `git } in
    let dst_dir = OpamFilename.Dir.of_string dst in
    OpamFilename.mkdir dst_dir;
    let repo_name = OpamRepositoryName.of_string label in
    let module B =
      (val OpamRepository.find_backend_by_kind url.OpamUrl.backend
          : OpamRepositoryBackend.S)
    in
    Retry.with_attempts ~label:(Fmt.str "fetch %s (%s)" label url_s) (fun () ->
        let result =
          B.fetch_repo_update repo_name dst_dir url |> OpamProcess.Job.run
        in
        match result with
        | OpamRepositoryBackend.Update_full tmp_dir ->
            if
              OpamFilename.Dir.to_string tmp_dir
              <> OpamFilename.Dir.to_string dst_dir
            then begin
              OpamFilename.rmdir dst_dir;
              OpamFilename.move_dir ~src:tmp_dir ~dst:dst_dir
            end
        | OpamRepositoryBackend.Update_patch _ ->
            B.repo_update_complete dst_dir url |> OpamProcess.Job.run
        | OpamRepositoryBackend.Update_empty -> ()
        | OpamRepositoryBackend.Update_err exn ->
            Fmt.failwith "Failed to fetch repo %s: %s" label
              (Printexc.to_string exn))

  let touch_dir dir =
    try
      let now = Unix.time () in
      Unix.utimes dir now now
    with Unix.Unix_error _ -> ()

  let repo_dir ~data_dir name = data_dir / "repos" / name

  let dir_needs_refresh dir =
    try
      let st = Unix.stat dir in
      Unix.time () -. st.Unix.st_mtime > Cache.refresh_max_age
    with Unix.Unix_error _ -> true

  let ensure_one ~fs ~refresh ~label ~url ~dir =
    let pkg_dir = dir / "packages" in
    if not (Sys.file_exists pkg_dir) then begin
      if Sys.file_exists dir then begin
        Log.info (fun m ->
            m "Re-cloning %s (existing clone at %s has no packages/)" label dir);
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir)
      end;
      Log.info (fun m -> m "Cloning %s from %s..." label url);
      pull_repo ~label ~url_s:url ~dst:dir;
      touch_dir dir
    end
    else if refresh || dir_needs_refresh dir then begin
      Log.info (fun m -> m "Updating %s..." label);
      try
        pull_repo ~label ~url_s:url ~dst:dir;
        touch_dir dir
      with exn ->
        Log.warn (fun m ->
            m "Failed to update %s: %s" label (Printexc.to_string exn))
    end

  let ensure_extra ~fs ~data_dir ?(refresh = false) extras =
    List.map
      (fun (e : Project.extra_repo) ->
        let dir = repo_dir ~data_dir e.name in
        ensure_one ~fs ~refresh ~label:e.name ~url:e.url ~dir;
        dir / "packages")
      extras
end

(* -- Reporepo (was lib/oi/reporepo.ml) ----------------------------------- *)

module Reporepo = struct
  let log_src = Logs.Src.create "oi.source.reporepo"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  let default_path =
    let data_base =
      match Sys.getenv_opt "OI_DATA_DIR" with
      | Some v when v <> "" -> v
      | _ -> (
          match Sys.getenv_opt "XDG_DATA_HOME" with
          | Some v when v <> "" -> v / "oi"
          | _ -> (
              match Sys.getenv_opt "HOME" with
              | Some h -> h / ".local" / "share" / "oi"
              | None -> "/tmp/oi"))
    in
    data_base / "reporepo"

  let env_path () =
    match Sys.getenv_opt "OI_REPOREPO" with
    | Some v when v <> "" -> v
    | _ -> default_path

  let default_url = "https://git.recoil.org/anil.recoil.org/reporepo"

  let env_url () =
    match Sys.getenv_opt "OI_REPOREPO_URL" with
    | Some v when v <> "" -> v
    | _ -> default_url

  type entry = {
    handle : string;
    version : string;
    url : string;
    commit : string;
    ref_ : string option;
    toolchain : string option;
    toolchain_name : string option;
    toolchain_compiler : string option;
    relocatable : bool option;
    toolchain_roots : string list list;
    toolchain_tools : string list;
    default_toolchain : bool;
    depends : (string * string option) list;
    root_packages : string list list;
    opam_path : string;
  }

  type root = { handle : string; version : string option }

  let mkdir_p ~fs d =
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / d)

  let write_file ~fs path content =
    mkdir_p ~fs (Filename.dirname path);
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) content

  let ensure_clone ~fs ~sys ~refresh ~path ~url =
    let dot_git = path / ".git" in
    if Sys.file_exists dot_git then
      begin if refresh then begin
        Log.info (fun m -> m "Refreshing reporepo at %s" path);
        try
          Retry.with_attempts ~label:(Fmt.str "git pull reporepo at %s" path)
            (fun () ->
              D10.Sysops.Cmd.run sys [ "git"; "-C"; path; "pull"; "--ff-only" ])
        with _ ->
          Log.warn (fun m ->
              m
                "Failed to refresh reporepo at %s — continuing with existing \
                 state. Resolve local changes and retry if this matters."
                path)
      end
      end
    else if
      Sys.file_exists path && Sys.is_directory path
      && Array.length (Sys.readdir path) > 0
    then
      Error.config_error
        "reporepo path %s exists but is not a git clone; move or remove it and \
         retry"
        path
    else begin
      mkdir_p ~fs (Filename.dirname path);
      Log.info (fun m -> m "Cloning reporepo from %s to %s" url path);
      try
        Retry.with_attempts ~label:(Fmt.str "git clone %s" url) (fun () ->
            D10.Sysops.Cmd.run sys [ "git"; "clone"; url; path ])
      with _ ->
        Error.config_error "failed to clone reporepo from %s into %s" url path
    end

  let git_at ~sys ~path args =
    D10.Sysops.Cmd.run sys ("git" :: "-C" :: path :: args)

  let git_at_out ~sys ~path args =
    D10.Sysops.Cmd.run_out sys ("git" :: "-C" :: path :: args)

  let git_at_inherit ~sys ~path args =
    D10.Sysops.Cmd.run_inherit sys ("git" :: "-C" :: path :: args)

  let assert_clone path =
    if not (Sys.file_exists (path / ".git")) then
      Error.config_error
        "reporepo at %s is not a git working copy — run an [oi repo] \
         subcommand first to bootstrap the clone"
        path

  let set_push_url ~sys ~path url =
    assert_clone path;
    git_at ~sys ~path [ "remote"; "set-url"; "--push"; "origin"; url ]

  type push_step =
    | Step_commit of { files : string list }
    | Step_pull of { commits : int }
    | Step_push of { commits : int }

  type push_outcome = push_step list

  let commit_count ~sys ~path ~base ~head =
    if base = "" || head = "" || base = head then 0
    else
      try
        int_of_string
          (git_at_out ~sys ~path [ "rev-list"; "--count"; base ^ ".." ^ head ])
      with _ -> 0

  let push ?(on_step_start = fun _ _ -> ()) ~sys ~path () =
    assert_clone path;
    let porcelain = git_at_out ~sys ~path [ "status"; "--porcelain" ] in
    let dirty_paths =
      porcelain |> String.split_on_char '\n'
      |> List.filter_map (fun line ->
          if String.length line < 4 then None
          else Some (String.sub line 3 (String.length line - 3)))
    in
    on_step_start 1 "commit local changes";
    let commit_step =
      if dirty_paths = [] then Step_commit { files = [] }
      else begin
        git_at ~sys ~path [ "add"; "-A" ];
        let summary =
          if List.length dirty_paths = 1 then List.hd dirty_paths
          else Fmt.str "%d files" (List.length dirty_paths)
        in
        let msg =
          Fmt.str
            "oi repo push: local edits (%s)\n\n\
             Auto-committed by `oi repo push`. Files:\n\
             %s"
            summary
            (String.concat "\n" (List.map (fun p -> "  " ^ p) dirty_paths))
        in
        git_at_inherit ~sys ~path [ "commit"; "-m"; msg ];
        Step_commit { files = dirty_paths }
      end
    in
    on_step_start 2 "pull --rebase from upstream";
    let head_before = git_at_out ~sys ~path [ "rev-parse"; "HEAD" ] in
    git_at_inherit ~sys ~path [ "pull"; "--rebase" ];
    let head_after = git_at_out ~sys ~path [ "rev-parse"; "HEAD" ] in
    let pull_step =
      Step_pull
        { commits = commit_count ~sys ~path ~base:head_before ~head:head_after }
    in
    on_step_start 3 "push to origin";
    let push_step =
      let local = git_at_out ~sys ~path [ "rev-parse"; "@" ] in
      let remote =
        try git_at_out ~sys ~path [ "rev-parse"; "@{u}" ] with _ -> ""
      in
      let ahead = commit_count ~sys ~path ~base:remote ~head:local in
      if ahead = 0 then Step_push { commits = 0 }
      else begin
        git_at_inherit ~sys ~path [ "push" ];
        Step_push { commits = ahead }
      end
    in
    [ commit_step; pull_step; push_step ]

  let split_url_commit s =
    match String.index_opt s '#' with
    | None -> (s, "")
    | Some i ->
        (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

  let is_overlay_extension extensions =
    match OpamStd.String.Map.find_opt Keys.overlay extensions with
    | None -> false
    | Some v -> (
        match v.OpamParserTypes.FullPos.pelem with
        | OpamParserTypes.FullPos.Bool true -> true
        | _ -> false)

  let read_string_extension extensions name =
    match OpamStd.String.Map.find_opt name extensions with
    | None -> None
    | Some v -> (
        match v.OpamParserTypes.FullPos.pelem with
        | OpamParserTypes.FullPos.String s -> Some s
        | _ -> None)

  let read_bool_extension ~path extensions name =
    match OpamStd.String.Map.find_opt name extensions with
    | None -> None
    | Some v -> (
        match v.OpamParserTypes.FullPos.pelem with
        | OpamParserTypes.FullPos.Bool b -> Some b
        | _ -> Error.config_error "%s: %s must be a boolean" path name)

  let read_string_list_extension ~path extensions name =
    match OpamStd.String.Map.find_opt name extensions with
    | None -> []
    | Some v -> (
        match v.OpamParserTypes.FullPos.pelem with
        | OpamParserTypes.FullPos.String s -> [ s ]
        | OpamParserTypes.FullPos.List { pelem = items; _ } ->
            List.map
              (fun (it : OpamParserTypes.FullPos.value) ->
                match it.pelem with
                | OpamParserTypes.FullPos.String s -> s
                | _ ->
                    Error.config_error
                      "%s: %s items must be strings" path name)
              items
        | _ ->
            Error.config_error
              "%s: %s must be a string or a list of strings" path name)

  let read_root_packages_extension ~path extensions name =
    let as_string_item (v : OpamParserTypes.FullPos.value) =
      match v.pelem with
      | OpamParserTypes.FullPos.String s -> s
      | _ -> Error.config_error "%s: %s nested item must be a string" path name
    in
    match OpamStd.String.Map.find_opt name extensions with
    | None -> []
    | Some v -> (
        match v.OpamParserTypes.FullPos.pelem with
        | OpamParserTypes.FullPos.String s -> [ [ s ] ]
        | OpamParserTypes.FullPos.List { pelem = items; _ } ->
            List.map
              (fun (it : OpamParserTypes.FullPos.value) ->
                match it.pelem with
                | OpamParserTypes.FullPos.String s -> [ s ]
                | OpamParserTypes.FullPos.List { pelem = sub; _ } ->
                    List.map as_string_item sub
                | _ ->
                    Error.config_error
                      "%s: %s item must be a string or a list of strings" path
                      name)
              items
        | _ ->
            Error.config_error
              "%s: %s must be a string, a list of strings, or a list of lists"
              path name)

  let pin_version_of_condition cond =
    OpamFormula.fold_left
      (fun acc foc ->
        match (acc, foc) with
        | Some _, _ -> acc
        | None, OpamTypes.Constraint (`Eq, filter) -> (
            match filter with OpamTypes.FString v -> Some v | _ -> None)
        | None, _ -> None)
      None cond

  let parse_depends_formula formula =
    let out = ref [] in
    OpamFormula.iter
      (fun (name, cond) ->
        out :=
          (OpamPackage.Name.to_string name, pin_version_of_condition cond)
          :: !out)
      formula;
    List.rev !out

  let parse_entry_file path : entry option =
    try
      let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) in
      if not (is_overlay_extension (OpamFile.OPAM.extensions opam)) then None
      else
        let handle =
          match OpamFile.OPAM.name_opt opam with
          | Some n -> OpamPackage.Name.to_string n
          | None -> Error.config_error "%s: missing name field" path
        in
        let version =
          match OpamFile.OPAM.version_opt opam with
          | Some v -> OpamPackage.Version.to_string v
          | None -> Error.config_error "%s: missing version field" path
        in
        let extensions = OpamFile.OPAM.extensions opam in
        let url_bare, commit =
          match OpamFile.OPAM.url opam with
          | Some u -> split_url_commit (OpamUrl.to_string (OpamFile.URL.url u))
          | None -> ("", "")
        in
        let toolchain_name =
          read_string_extension extensions Keys.toolchain_name
        in
        if url_bare = "" && toolchain_name = None then
          Error.config_error
            "%s: entry needs either a [url:] block or [%s]"
            path Keys.toolchain_name;
        let depends = parse_depends_formula (OpamFile.OPAM.depends opam) in
        let ref_ = read_string_extension extensions Keys.ref in
        let toolchain = read_string_extension extensions Keys.toolchain in
        let toolchain_compiler =
          read_string_extension extensions Keys.toolchain_compiler
        in
        if toolchain_name <> None && toolchain_compiler = None then
          Error.config_error
            "%s: %s is set but %s is missing — every toolchain definition \
             must declare its compiler package (e.g. \
             \"ocaml-base-compiler.5.4.1\")"
            path Keys.toolchain_name Keys.toolchain_compiler;
        let relocatable =
          read_bool_extension ~path extensions Keys.relocatable
        in
        let toolchain_roots =
          read_root_packages_extension ~path extensions
            Keys.toolchain_roots
        in
        let toolchain_tools =
          read_string_list_extension ~path extensions Keys.toolchain_tools
        in
        let default_toolchain =
          match
            read_bool_extension ~path extensions Keys.default_toolchain
          with
          | Some b -> b
          | None -> false
        in
        if default_toolchain && toolchain_name = None then
          Error.config_error
            "%s: %s is only meaningful on toolchain definitions (entries \
             with %s set)"
            path Keys.default_toolchain Keys.toolchain_name;
        let root_packages =
          read_root_packages_extension ~path extensions Keys.root_packages
        in
        Some
          {
            handle;
            version;
            url = url_bare;
            commit;
            ref_;
            toolchain;
            toolchain_name;
            toolchain_compiler;
            relocatable;
            toolchain_roots;
            toolchain_tools;
            default_toolchain;
            depends;
            root_packages;
            opam_path = path;
          }
    with
    | Error.E _ as e -> raise e
    | exn ->
        Log.warn (fun m -> m "Skipping %s: %s" path (Printexc.to_string exn));
        None

  let version_compare a b =
    OpamPackage.Version.compare
      (OpamPackage.Version.of_string a)
      (OpamPackage.Version.of_string b)

  let load ~path =
    let packages_dir = path / "packages" in
    if not (Sys.file_exists packages_dir) then []
    else
      let entries =
        let handles = Sys.readdir packages_dir |> Array.to_list in
        List.sort String.compare handles
        |> List.concat_map (fun h ->
            let handle_dir = packages_dir / h in
            if not (Sys.is_directory handle_dir) then []
            else
              let versions = Sys.readdir handle_dir |> Array.to_list in
              List.sort String.compare versions
              |> List.filter_map (fun v ->
                  let opam_path = handle_dir / v / "opam" in
                  if Sys.file_exists opam_path then parse_entry_file opam_path
                  else None))
      in
      (* Default-flag check: ask "for each handle's LATEST version, is
         it flagged?". Older versions don't count — bumping a
         toolchain to clear the flag must not be defeated by the
         original-flagged entry still on disk. *)
      let latest_per_handle =
        entries
        |> List.fold_left
            (fun acc (e : entry) ->
              match List.assoc_opt e.handle acc with
              | None -> (e.handle, e) :: acc
              | Some prev when version_compare e.version prev.version > 0 ->
                  (e.handle, e) :: List.remove_assoc e.handle acc
              | Some _ -> acc)
            []
      in
      let defaults =
        latest_per_handle
        |> List.filter_map (fun (_, (e : entry)) ->
            if e.default_toolchain then Some e.handle else None)
        |> List.sort String.compare
      in
      (match defaults with
      | [] | [ _ ] -> ()
      | many ->
          Error.config_error
            "reporepo at %s has %d toolchains marked as default \
             (%s: true): %s. Exactly one toolchain may be the default — \
             clear the flag on the others."
            path (List.length many) Keys.default_toolchain
            (String.concat ", " many));
      entries

  let latest entries ~handle =
    entries
    |> List.filter (fun (e : entry) -> e.handle = handle)
    |> List.sort (fun (a : entry) (b : entry) ->
        version_compare b.version a.version)
    |> function
    | x :: _ -> Some x
    | [] -> None

  let find entries ~handle ~version =
    List.find_opt
      (fun (e : entry) -> e.handle = handle && e.version = version)
      entries

  let default_toolchain entries =
    let candidates =
      entries
      |> List.filter (fun (e : entry) -> e.toolchain_name <> None)
      |> List.fold_left
          (fun acc (e : entry) ->
            match List.assoc_opt e.handle acc with
            | None -> (e.handle, e) :: acc
            | Some prev when version_compare e.version prev.version > 0 ->
                (e.handle, e) :: List.remove_assoc e.handle acc
            | Some _ -> acc)
          []
    in
    candidates
    |> List.filter_map (fun (_, (e : entry)) ->
        if e.default_toolchain then Some e else None)
    |> function
    | [] -> None
    | e :: _ -> Some e

  let topo_sort entries =
    let by_handle = Hashtbl.create 16 in
    List.iter (fun (e : entry) -> Hashtbl.replace by_handle e.handle e) entries;
    let visited = Hashtbl.create 16 in
    let out_rev = ref [] in
    let rec visit (e : entry) =
      if not (Hashtbl.mem visited e.handle) then begin
        Hashtbl.add visited e.handle ();
        List.iter
          (fun (h, _v) ->
            match Hashtbl.find_opt by_handle h with
            | Some dep -> visit dep
            | None -> ())
          e.depends;
        out_rev := e :: !out_rev
      end
    in
    List.iter visit entries;
    List.rev !out_rev

  module Inst = Opam_0install.Solver.Make (Opam_0install.Dir_context)

  let resolve entries ~roots =
    if roots = [] then []
    else
      let path = env_path () in
      let packages_dir = path / "packages" in
      if not (Sys.file_exists packages_dir) then
        Error.config_error "reporepo at %s has no packages/ tree" path;
      let constraints =
        List.fold_left
          (fun m (r : root) ->
            match r.version with
            | None -> m
            | Some v ->
                OpamPackage.Name.Map.add
                  (OpamPackage.Name.of_string r.handle)
                  (`Eq, OpamPackage.Version.of_string v)
                  m)
          OpamPackage.Name.Map.empty roots
      in
      let env _ = None in
      let ctx =
        Opam_0install.Dir_context.create ~constraints ~env packages_dir
      in
      let names =
        List.map (fun (r : root) -> OpamPackage.Name.of_string r.handle) roots
      in
      Log.debug (fun m ->
          m "0install solve over %s for roots: %s" packages_dir
            (String.concat ", " (List.map OpamPackage.Name.to_string names)));
      match Inst.solve ctx names with
      | Error diag ->
          Error.config_error "overlay resolution failed: %s"
            (Inst.diagnostics diag)
      | Ok sels ->
          let pkgs = Inst.packages_of_result sels in
          let resolved =
            List.filter_map
              (fun pkg ->
                find entries
                  ~handle:(OpamPackage.Name.to_string (OpamPackage.name pkg))
                  ~version:
                    (OpamPackage.Version.to_string (OpamPackage.version pkg)))
              pkgs
          in
          let sorted = topo_sort resolved in
          List.iter
            (fun (e : entry) ->
              let short =
                if String.length e.commit >= 7 then String.sub e.commit 0 7
                else e.commit
              in
              Log.debug (fun m ->
                  m "  resolved %s.%s@%s via %s" e.handle e.version short e.url))
            sorted;
          sorted

  let clone_dir_name ~handle ~version = "overlay-" ^ handle ^ "-" ^ version

  let materialize_one ~fs ~refresh ~data_dir ~handle ~version ~url ~commit =
    let name = clone_dir_name ~handle ~version in
    let dir = data_dir / "repos" / name in
    let url = if commit = "" then url else url ^ "#" ^ commit in
    Repo.ensure_one ~fs ~refresh ~label:name ~url ~dir;
    dir / "packages"

  let materialize ~fs ~sys:_ ~data_dir ?(refresh = false) entries =
    List.filter_map
      (fun (e : entry) ->
        if e.url = "" then None
        else
          Some
            (materialize_one ~fs ~refresh ~data_dir ~handle:e.handle
               ~version:e.version ~url:e.url ~commit:e.commit))
      entries

  let base_entries () =
    let path = env_path () in
    if not (Sys.file_exists path) then []
    else
      let entries = load ~path in
      match latest entries ~handle:"relocatable" with
      | None -> []
      | Some _ ->
          resolve entries ~roots:[ { handle = "relocatable"; version = None } ]
          |> List.rev

  let ensure_base ~fs ~sys ~data_dir ?(refresh = false) () =
    let path = env_path () in
    ensure_clone ~fs ~sys ~refresh ~path ~url:(env_url ());
    Log.debug (fun m -> m "ensure_base: reading reporepo %s" path);
    let entries = try load ~path with Error.E _ -> [] in
    match latest entries ~handle:"relocatable" with
    | None ->
        Error.config_error
          "reporepo at %s has no 'relocatable' overlay; run 'oi repo add \
           relocatable <URL>' to bootstrap the base repositories"
          path
    | Some _ ->
        let base =
          resolve entries ~roots:[ { handle = "relocatable"; version = None } ]
          |> List.rev
        in
        Log.debug (fun m ->
            m "base overlays (highest priority first): %s"
              (String.concat ", "
                 (List.map (fun (e : entry) -> e.handle ^ "." ^ e.version) base)));
        List.map
          (fun (e : entry) ->
            materialize_one ~fs ~refresh ~data_dir ~handle:e.handle
              ~version:e.version ~url:e.url ~commit:e.commit)
          base

  let parse_ls_remote_output out =
    String.split_on_char '\n' out
    |> List.filter_map (fun line ->
        let line = String.trim line in
        if line = "" then None
        else
          match String.split_on_char '\t' line with
          | sha :: ref_name :: _ when String.length sha = 40 ->
              Some (sha, ref_name)
          | _ -> None)

  let try_ls_remote ~sys url args =
    try
      Retry.with_attempts ~label:(Fmt.str "git ls-remote %s" url) (fun () ->
          D10.Sysops.Cmd.run_out_quiet sys ("git" :: "ls-remote" :: url :: args))
    with exn ->
      Error.config_error "git ls-remote %s failed: %s" url
        (Printexc.to_string exn)

  let ls_remote_sha ~sys ?ref_ url =
    match ref_ with
    | Some r -> (
        let out = try_ls_remote ~sys url [ r ] in
        match parse_ls_remote_output out with
        | (sha, _) :: _ -> sha
        | [] -> Error.config_error "git ls-remote %s %s: ref not found" url r)
    | None -> (
        let head = try_ls_remote ~sys url [ "HEAD" ] in
        match parse_ls_remote_output head with
        | (sha, _) :: _ -> sha
        | [] -> (
            let all = try_ls_remote ~sys url [] in
            let refs = parse_ls_remote_output all in
            let prefer name =
              List.find_map
                (fun (sha, r) -> if r = name then Some sha else None)
                refs
            in
            match prefer "refs/heads/main" with
            | Some sha -> sha
            | None -> (
                match prefer "refs/heads/master" with
                | Some sha -> sha
                | None -> (
                    match refs with
                    | (sha, _) :: _ -> sha
                    | [] ->
                        Error.config_error "git ls-remote %s returned no refs"
                          url))))

  let today_yyyymmdd () =
    let tm = Unix.gmtime (Unix.time ()) in
    Fmt.str "%04d%02d%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

  let next_version entries ~handle =
    let today = today_yyyymmdd () in
    let prefix = today ^ "." in
    let max_seq =
      entries
      |> List.filter (fun (e : entry) -> e.handle = handle)
      |> List.filter_map (fun (e : entry) ->
          if String.starts_with ~prefix e.version then
            let suffix =
              String.sub e.version (String.length prefix)
                (String.length e.version - String.length prefix)
            in
            int_of_string_opt suffix
          else None)
      |> List.fold_left max (-1)
    in
    Fmt.str "%s.%d" today (max_seq + 1)

  let escape_string s =
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string buf "\\\""
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\n' -> Buffer.add_string buf "\\n"
        | c -> Buffer.add_char buf c)
      s;
    Buffer.add_char buf '"';
    Buffer.contents buf

  let render_root_groups buf field groups =
    match groups with
    | [] -> ()
    | groups ->
        Printf.bprintf buf "%s: [\n" field;
        List.iter
          (fun group ->
            match group with
            | [] -> ()
            | [ one ] -> Printf.bprintf buf "  %s\n" (escape_string one)
            | multi ->
                Buffer.add_string buf "  [";
                List.iter
                  (fun p -> Printf.bprintf buf " %s" (escape_string p))
                  multi;
                Buffer.add_string buf " ]\n")
          groups;
        Buffer.add_string buf "]\n"

  type toolchain_def = {
    td_name : string;
    td_compiler : string option;
    td_relocatable : bool option;
    td_roots : string list list;
    td_tools : string list;
    td_default : bool;
  }

  let render_opam ~synopsis ~url ~commit ~ref_ ~toolchain ?toolchain_def
      ~depends ~root_packages () =
    let buf = Buffer.create 512 in
    Printf.bprintf buf "opam-version: \"2.0\"\n";
    Printf.bprintf buf "synopsis: %s\n" (escape_string synopsis);
    if url <> "" then
      Printf.bprintf buf "url {\n  src: %s\n}\n"
        (escape_string (if commit = "" then url else url ^ "#" ^ commit));
    (match depends with
    | [] -> ()
    | ds ->
        Buffer.add_string buf "depends: [\n";
        List.iter
          (fun (h, v) ->
            match v with
            | Some ver ->
                Printf.bprintf buf "  %s { = %s }\n" (escape_string h)
                  (escape_string ver)
            | None -> Printf.bprintf buf "  %s\n" (escape_string h))
          ds;
        Buffer.add_string buf "]\n");
    Printf.bprintf buf "%s: true\n" Keys.overlay;
    Stdlib.Option.iter
      (fun s ->
        Printf.bprintf buf "%s: %s\n" Keys.ref (escape_string s))
      ref_;
    Stdlib.Option.iter
      (fun s ->
        Printf.bprintf buf "%s: %s\n" Keys.toolchain (escape_string s))
      toolchain;
    Stdlib.Option.iter
      (fun (td : toolchain_def) ->
        Printf.bprintf buf "%s: %s\n" Keys.toolchain_name
          (escape_string td.td_name);
        Stdlib.Option.iter
          (fun s ->
            Printf.bprintf buf "%s: %s\n" Keys.toolchain_compiler
              (escape_string s))
          td.td_compiler;
        Stdlib.Option.iter
          (fun b ->
            Printf.bprintf buf "%s: %b\n" Keys.relocatable b)
          td.td_relocatable;
        if td.td_default then
          Printf.bprintf buf "%s: true\n" Keys.default_toolchain;
        render_root_groups buf Keys.toolchain_roots td.td_roots;
        (match td.td_tools with
        | [] -> ()
        | tools ->
            Printf.bprintf buf "%s: [\n" Keys.toolchain_tools;
            List.iter
              (fun t -> Printf.bprintf buf "  %s\n" (escape_string t))
              tools;
            Buffer.add_string buf "]\n"))
      toolchain_def;
    render_root_groups buf Keys.root_packages root_packages;
    Buffer.contents buf

  let write_entry ~fs ~path ~handle ~version content =
    let pkg_dir = path / "packages" / handle / (handle ^ "." ^ version) in
    let opam_path = pkg_dir / "opam" in
    write_file ~fs opam_path content;
    opam_path

  let ensure_repo_marker ~fs ~path =
    let marker = path / "repo" in
    if not (Sys.file_exists marker) then
      write_file ~fs marker "opam-version: \"2.0\"\n"

  let default_synopsis handle =
    "Overlay: " ^ handle ^ " — pinned opam repository"

  let is_base_handle h = h = "default" || h = "relocatable"
  let default_base_handles = [ "relocatable"; "default" ]

  let auto_base_depends entries ~handle ~base_handles =
    if is_base_handle handle then []
    else
      List.filter_map
        (fun h ->
          match latest entries ~handle:h with
          | Some e -> Some (h, Some e.version)
          | None -> None)
        base_handles

  let add ~fs ~sys ~path ~handle ~url ?ref_ ?toolchain ?base_handles ?depends
      ?(root_packages = []) ?synopsis ?(force = false) () =
    let entries = load ~path in
    let handle_exists =
      List.exists (fun (e : entry) -> e.handle = handle) entries
    in
    if handle_exists && not force then
      Error.config_error
        "overlay %s already exists in reporepo; use 'oi repo bump' to add a \
         new version, or pass --force to register a new entry with a different \
         URL"
        handle;
    let base_handles =
      Stdlib.Option.value base_handles ~default:default_base_handles
    in
    let depends =
      match depends with
      | Some d -> d
      | None -> auto_base_depends entries ~handle ~base_handles
    in
    let commit = ls_remote_sha ~sys ?ref_ url in
    let version =
      if handle_exists then next_version entries ~handle
      else today_yyyymmdd () ^ ".0"
    in
    let synopsis =
      Stdlib.Option.value synopsis ~default:(default_synopsis handle)
    in
    let content =
      render_opam ~synopsis ~url ~commit ~ref_ ~toolchain ~depends
        ~root_packages ()
    in
    ensure_repo_marker ~fs ~path;
    let opam_path = write_entry ~fs ~path ~handle ~version content in
    {
      handle;
      version;
      url;
      commit;
      ref_;
      toolchain;
      toolchain_name = None;
      toolchain_compiler = None;
      relocatable = None;
      toolchain_roots = [];
      toolchain_tools = [];
      default_toolchain = false;
      depends;
      root_packages;
      opam_path;
    }

  let bump ~fs ~sys ~path ~handle ?url ?ref_ ?toolchain ?base_handles ?depends
      ?root_packages ?default () =
    let entries = load ~path in
    let prev =
      match latest entries ~handle with
      | Some e -> e
      | None ->
          Error.config_error
            "overlay %s not in reporepo; use 'oi repo add' to create it" handle
    in
    if default <> None && prev.toolchain_name = None then
      Error.config_error
        "overlay %s is not a toolchain definition (no %s) — the --default \
         flag only applies to toolchain entries"
        handle Keys.toolchain_name;
    let default_toolchain =
      Stdlib.Option.value default ~default:prev.default_toolchain
    in
    let url = Stdlib.Option.value url ~default:prev.url in
    let ref_ = match ref_ with Some _ -> ref_ | None -> prev.ref_ in
    let toolchain =
      match toolchain with Some _ -> toolchain | None -> prev.toolchain
    in
    let commit =
      if url = "" then prev.commit else ls_remote_sha ~sys ?ref_ url
    in
    let depends =
      match depends with
      | Some d -> d
      | None -> (
          if is_base_handle handle then prev.depends
          else
            match base_handles with
            | Some bh -> auto_base_depends entries ~handle ~base_handles:bh
            | None ->
                let auto =
                  auto_base_depends entries ~handle
                    ~base_handles:default_base_handles
                in
                if auto = [] then prev.depends else auto)
    in
    let root_packages =
      match root_packages with Some r -> r | None -> prev.root_packages
    in
    let eq_as_set a b = List.sort compare a = List.sort compare b in
    if
      url = prev.url && commit = prev.commit && ref_ = prev.ref_
      && toolchain = prev.toolchain
      && default_toolchain = prev.default_toolchain
      && eq_as_set depends prev.depends
      && eq_as_set root_packages prev.root_packages
    then `Unchanged prev
    else
      let version = next_version entries ~handle in
      let toolchain_def =
        match prev.toolchain_name with
        | None -> None
        | Some name ->
            Some
              {
                td_name = name;
                td_compiler = prev.toolchain_compiler;
                td_relocatable = prev.relocatable;
                td_roots = prev.toolchain_roots;
                td_tools = prev.toolchain_tools;
                td_default = default_toolchain;
              }
      in
      let content =
        render_opam ~synopsis:(default_synopsis handle) ~url ~commit ~ref_
          ~toolchain ?toolchain_def ~depends ~root_packages ()
      in
      let opam_path = write_entry ~fs ~path ~handle ~version content in
      `Bumped
        {
          handle;
          version;
          url;
          commit;
          ref_;
          toolchain;
          toolchain_name = prev.toolchain_name;
          toolchain_compiler = prev.toolchain_compiler;
          relocatable = prev.relocatable;
          toolchain_roots = prev.toolchain_roots;
          toolchain_tools = prev.toolchain_tools;
          default_toolchain;
          depends;
          root_packages;
          opam_path;
        }

  let remove ~fs ~path ~handle ?version () =
    let entries = load ~path in
    let matches =
      List.filter
        (fun (e : entry) ->
          e.handle = handle
          && match version with None -> true | Some v -> e.version = v)
        entries
    in
    if matches = [] then
      Error.config_error "no such overlay %s%s in reporepo" handle
        (match version with None -> "" | Some v -> "." ^ v);
    List.iter
      (fun (e : entry) ->
        let pkg_dir = Filename.dirname e.opam_path in
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / pkg_dir))
      matches;
    let handle_dir = path / "packages" / handle in
    if Sys.file_exists handle_dir && Sys.readdir handle_dir |> Array.length = 0
    then Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / handle_dir)
end

(* -- Pin-depends realisation (was lib/oi/pin.ml) ------------------------- *)

module Pin = struct
  let log_src = Logs.Src.create "oi.source.pin"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  let url_key (url : OpamUrl.t) =
    Digest.to_hex (Digest.string (OpamUrl.to_string url))

  let sentinel_path src_dir = src_dir / ".oi-pin-ok"

  let fetch_pin ~fs ~cache ~refresh (pin : Project.pin) =
    let root = Cache.pins_dir cache in
    let src_dir = root / "sources" / url_key pin.url in
    let sentinel = sentinel_path src_dir in
    if Cache.fresh ~refresh ~sentinel ~max_age:Cache.refresh_max_age then
      src_dir
    else begin
      Log.info (fun m ->
          m "Fetching pin %s from %s"
            (OpamPackage.to_string pin.pkg)
            (OpamUrl.to_string pin.url));
      if Sys.file_exists src_dir then
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / src_dir);
      let dst = OpamFilename.Dir.of_string src_dir in
      OpamFilename.mkdir dst;
      let cache_dir =
        OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
      in
      let result =
        OpamRepository.pull_tree
          (OpamPackage.to_string pin.pkg)
          ~cache_dir dst [] [ pin.url ]
        |> OpamProcess.Job.run
      in
      match result with
      | OpamTypes.Result _ | OpamTypes.Up_to_date _ ->
          Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / sentinel) "";
          src_dir
      | OpamTypes.Not_available (_, msg) ->
          Error.config_error "pin %s: fetch failed (%s): %s"
            (OpamPackage.to_string pin.pkg)
            (OpamUrl.to_string pin.url)
            msg
    end

  let resolved_revision ~sys (pin : Project.pin) ~src_dir =
    match pin.url.OpamUrl.backend with
    | `git -> (
        try
          D10.Sysops.Cmd.run_out sys
            [ "git"; "-C"; src_dir; "rev-parse"; "HEAD" ]
        with Failure _ -> url_key pin.url)
    | _ -> url_key pin.url

  let locate_opam_file ~src_dir (pin : Project.pin) =
    let name = OpamPackage.Name.to_string (OpamPackage.name pin.pkg) in
    let candidates = [ src_dir / (name ^ ".opam"); src_dir / "opam" ] in
    match List.find_opt Sys.file_exists candidates with
    | Some p -> p
    | None ->
        Error.config_error "pin %s: no %s.opam or opam file at the root of %s"
          (OpamPackage.to_string pin.pkg)
          name src_dir

  let rewrite_opam ~src_dir ~opam_path ~revision (pin : Project.pin) =
    let opam =
      try OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))
      with exn ->
        Error.config_error "pin: failed to read %s: %s" opam_path
          (Printexc.to_string exn)
    in
    let local_url =
      OpamUrl.of_string (Fmt.str "file://%s#%s" src_dir revision)
    in
    let new_url = OpamFile.URL.create local_url in
    opam
    |> OpamFile.OPAM.with_url new_url
    |> OpamFile.OPAM.with_name (OpamPackage.name pin.pkg)
    |> OpamFile.OPAM.with_version (OpamPackage.version pin.pkg)

  let write_repo_marker ~fs ~dir =
    let repo_path = dir / "repo" in
    let path = Eio.Path.(fs / repo_path) in
    Eio.Path.save ~create:(`If_missing 0o644) path "opam-version: \"2.0\"\n"

  let write_pin_opam ~packages_dir (pin : Project.pin) opam =
    let name = OpamPackage.Name.to_string (OpamPackage.name pin.pkg) in
    let pkg_s = OpamPackage.to_string pin.pkg in
    let dir = packages_dir / name / pkg_s in
    OpamFilename.mkdir (OpamFilename.Dir.of_string dir);
    let target = dir / "opam" in
    OpamFile.OPAM.write (OpamFile.make (OpamFilename.raw target)) opam

  type resolved = {
    pin : Project.pin;
    opam : OpamFile.OPAM.t;
    revision : string;
  }

  let materialize ~fs ~sys ~cache ?(refresh = false) pins =
    match pins with
    | [] -> None
    | _ ->
        let resolved =
          List.map
            (fun (pin : Project.pin) ->
              let src_dir = fetch_pin ~fs ~cache ~refresh pin in
              let opam_path = locate_opam_file ~src_dir pin in
              let revision = resolved_revision ~sys pin ~src_dir in
              let opam = rewrite_opam ~src_dir ~opam_path ~revision pin in
              { pin; opam; revision })
            pins
        in
        let triples =
          List.map
            (fun r ->
              let name =
                OpamPackage.Name.to_string (OpamPackage.name r.pin.pkg)
              in
              let version = OpamPackage.version_to_string r.pin.pkg in
              (name, version, r.revision))
            resolved
          |> List.sort compare
        in
        let set_hash =
          let s =
            String.concat "\n"
              (List.map
                 (fun (n, v, r) -> Printf.sprintf "%s\t%s\t%s" n v r)
                 triples)
          in
          Digest.to_hex (Digest.string s)
        in
        let root = Cache.pins_dir cache in
        let set_root = root / "sets" / set_hash in
        let packages_dir = set_root / "packages" in
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / packages_dir);
        write_repo_marker ~fs ~dir:set_root;
        List.iter (fun r -> write_pin_opam ~packages_dir r.pin r.opam) resolved;
        Log.info (fun m ->
            m "Materialized %d pin(s) at %s" (List.length pins) packages_dir);
        Some packages_dir
end

(* -- Source mirror (was lib/oi/source_mirror.ml) ------------------------- *)

module Mirror = struct
  let log_src = Logs.Src.create "oi.source.mirror"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  let dir ~cache = Cache.root_s cache / "mirror"
  let db_path ~cache = dir ~cache / "index.db"
  let url ~cache = OpamUrl.of_string ("file://" ^ dir ~cache)

  let remote_url ~registry =
    let trimmed =
      let n = String.length registry in
      if n > 0 && registry.[n - 1] = '/' then String.sub registry 0 (n - 1)
      else registry
    in
    OpamUrl.of_string (trimmed ^ "/sources")

  let path_of_checksum ~cache ck =
    List.fold_left ( / ) (dir ~cache) (OpamHash.to_path ck)

  let schema =
    {|
    CREATE TABLE IF NOT EXISTS sources (
      sha256    TEXT PRIMARY KEY,
      size      INTEGER NOT NULL,
      added_at  REAL NOT NULL
    );

    CREATE TABLE IF NOT EXISTS source_checksums (
      algo    TEXT NOT NULL,
      value   TEXT NOT NULL,
      sha256  TEXT NOT NULL REFERENCES sources(sha256) ON DELETE CASCADE,
      PRIMARY KEY (algo, value)
    );
    CREATE INDEX IF NOT EXISTS idx_source_checksums_sha256
      ON source_checksums(sha256);

    CREATE TABLE IF NOT EXISTS source_refs (
      sha256          TEXT NOT NULL REFERENCES sources(sha256) ON DELETE CASCADE,
      package_name    TEXT NOT NULL,
      package_version TEXT NOT NULL,
      url             TEXT NOT NULL,
      kind            TEXT NOT NULL CHECK (kind IN ('main','extra')),
      extra_name      TEXT NOT NULL DEFAULT '',
      PRIMARY KEY (sha256, package_name, package_version, kind, extra_name)
    );
    CREATE INDEX IF NOT EXISTS idx_source_refs_pkg
      ON source_refs(package_name, package_version);
  |}

  let exec db sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        Fmt.failwith "source_mirror sqlite: %s: %s\nSQL: %s"
          (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db) sql

  let mkdir_p ~fs d =
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / d)

  let open_db ~cache =
    mkdir_p ~fs:(Cache.fs cache) (dir ~cache);
    let db = Sqlite3.db_open (db_path ~cache) in
    exec db schema;
    exec db "PRAGMA journal_mode=WAL";
    exec db "PRAGMA synchronous=NORMAL";
    exec db "PRAGMA foreign_keys=ON";
    db

  let close_db db = ignore (Sqlite3.db_close db)

  let with_db ~cache f =
    let db = open_db ~cache in
    Fun.protect ~finally:(fun () -> close_db db) (fun () -> f db)

  let quote s =
    let s = String.split_on_char '\'' s |> String.concat "''" in
    Fmt.str "'%s'" s

  let file_size path =
    try Int64.of_int (Unix.stat path).Unix.st_size
    with Unix.Unix_error _ -> 0L

  let resolve_symlink path =
    try
      if (Unix.lstat path).Unix.st_kind = Unix.S_LNK then Unix.realpath path
      else path
    with Unix.Unix_error _ -> path

  let dst_is_valid dst =
    try
      let _ = Unix.stat dst in
      true
    with Unix.Unix_error _ -> false

  let link_or_copy ~fs ~src ~dst =
    let src = resolve_symlink src in
    if dst_is_valid dst then ()
    else begin
      mkdir_p ~fs (Filename.dirname dst);
      (try Unix.unlink dst with Unix.Unix_error _ -> ());
      let tmp = dst ^ ".tmp" in
      (try Unix.unlink tmp with Unix.Unix_error _ -> ());
      let linked =
        try
          Unix.link src tmp;
          true
        with
        | Unix.Unix_error
            ((Unix.EXDEV | Unix.EMLINK | Unix.EPERM | Unix.EOPNOTSUPP), _, _)
        ->
          false
      in
      if not linked then begin
        let ic = open_in_bin src in
        Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
        let oc = open_out_bin tmp in
        Fun.protect ~finally:(fun () -> close_out_noerr oc) @@ fun () ->
        let buf = Bytes.create 65536 in
        let rec loop () =
          let n = input ic buf 0 (Bytes.length buf) in
          if n > 0 then begin
            output oc buf 0 n;
            loop ()
          end
        in
        loop ()
      end;
      try Unix.rename tmp dst
      with Unix.Unix_error _ -> ( try Sys.remove tmp with Sys_error _ -> ())
    end

  let opam_cache_root () =
    OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
    |> OpamFilename.Dir.to_string

  let opam_cache_file checksums =
    let root = opam_cache_root () in
    List.find_map
      (fun ck ->
        let path = List.fold_left ( / ) root (OpamHash.to_path ck) in
        if Sys.file_exists path then Some path else None)
      checksums

  let record ~sys:_ ~cache ~package ?overlay ~kind ~url ~checksums () =
    let provenance =
      match overlay with
      | None -> ""
      | Some (h, v) -> Fmt.str " (from %s.%s)" h v
    in
    let kind_tag =
      match kind with `Main -> "" | `Extra name -> Fmt.str " [extra:%s]" name
    in
    match checksums with
    | [] ->
        Log.debug (fun m ->
            m "no checksums for %s%s%s, nothing to mirror"
              (OpamPackage.to_string package)
              kind_tag provenance)
    | _ -> (
        match opam_cache_file checksums with
        | None ->
            Log.debug (fun m ->
                m "no cached blob for %s%s%s, skipping mirror"
                  (OpamPackage.to_string package)
                  kind_tag provenance)
        | Some src_path ->
            let sha256_hash = OpamHash.compute ~kind:`SHA256 src_path in
            let sha256 = OpamHash.contents sha256_hash in
            let size = file_size src_path in
            let fs = Cache.fs cache in
            with_db ~cache @@ fun db ->
            exec db "BEGIN TRANSACTION";
            exec db
              (Fmt.str
                 "INSERT OR IGNORE INTO sources (sha256, size, added_at) \
                  VALUES (%s, %Ld, %f)"
                 (quote sha256) size (Unix.time ()));
            let declared = ref [] in
            List.iter
              (fun ck ->
                let dst = path_of_checksum ~cache ck in
                link_or_copy ~fs ~src:src_path ~dst;
                let algo = OpamHash.(string_of_kind (kind ck)) in
                let value = OpamHash.contents ck in
                declared := (algo, value) :: !declared;
                exec db
                  (Fmt.str
                     "INSERT OR IGNORE INTO source_checksums (algo, value, \
                      sha256) VALUES (%s, %s, %s)"
                     (quote algo) (quote value) (quote sha256)))
              checksums;
            if not (List.exists (fun (a, _) -> a = "sha256") !declared) then begin
              let dst = path_of_checksum ~cache sha256_hash in
              link_or_copy ~fs ~src:src_path ~dst;
              exec db
                (Fmt.str
                   "INSERT OR IGNORE INTO source_checksums (algo, value, \
                    sha256) VALUES ('sha256', %s, %s)"
                   (quote sha256) (quote sha256))
            end;
            let extra_name, kind_s =
              match kind with `Main -> ("", "main") | `Extra n -> (n, "extra")
            in
            exec db
              (Fmt.str
                 "INSERT OR IGNORE INTO source_refs (sha256, package_name, \
                  package_version, url, kind, extra_name) VALUES (%s, %s, %s, \
                  %s, %s, %s)"
                 (quote sha256)
                 (quote (OpamPackage.Name.to_string (OpamPackage.name package)))
                 (quote
                    (OpamPackage.Version.to_string
                       (OpamPackage.version package)))
                 (quote (OpamUrl.to_string url))
                 (quote kind_s) (quote extra_name));
            exec db "COMMIT")

  type stats = { count : int; total_size : int64 }

  let stats ~cache =
    if not (Sys.file_exists (db_path ~cache)) then
      { count = 0; total_size = 0L }
    else
      with_db ~cache @@ fun db ->
      let stmt =
        Sqlite3.prepare db
          "SELECT COUNT(*), COALESCE(SUM(size), 0) FROM sources"
      in
      let r =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let count = Sqlite3.Data.to_int_exn (Sqlite3.column stmt 0) in
            let total_size =
              match Sqlite3.column stmt 1 with
              | Sqlite3.Data.INT n -> n
              | Sqlite3.Data.FLOAT f -> Int64.of_float f
              | _ -> 0L
            in
            { count; total_size }
        | _ -> { count = 0; total_size = 0L }
      in
      ignore (Sqlite3.finalize stmt);
      r

  type entry = {
    sha256 : string;
    size : int64;
    package_name : string;
    package_version : string;
    kind : [ `Main | `Extra of string ];
    url : string;
  }

  let list ~cache ?package () =
    if not (Sys.file_exists (db_path ~cache)) then []
    else
      with_db ~cache @@ fun db ->
      let out = ref [] in
      let cb row =
        match row with
        | [| sha256; size; name; version; url; kind_s; extra_name |] ->
            let kind =
              match kind_s with
              | "main" -> `Main
              | "extra" -> `Extra extra_name
              | other -> `Extra other
            in
            let size =
              Int64.of_string_opt size |> Stdlib.Option.value ~default:0L
            in
            out :=
              {
                sha256;
                size;
                package_name = name;
                package_version = version;
                kind;
                url;
              }
              :: !out
        | _ -> ()
      in
      let where =
        match package with
        | None -> ""
        | Some p -> Fmt.str " WHERE r.package_name = %s" (quote p)
      in
      ignore
        (Sqlite3.exec_not_null_no_headers db ~cb
           (Fmt.str
              "SELECT s.sha256, s.size, r.package_name, r.package_version, \
               r.url, r.kind, r.extra_name FROM source_refs r JOIN sources s \
               ON s.sha256 = r.sha256%s ORDER BY r.package_name, \
               r.package_version, r.kind, r.extra_name"
              where));
      List.rev !out

  let checksums_for_sha256 db sha256 =
    let out = ref [] in
    let cb row =
      match row with
      | [| algo; value |] -> out := (algo, value) :: !out
      | _ -> ()
    in
    ignore
      (Sqlite3.exec_not_null_no_headers db ~cb
         (Fmt.str "SELECT algo, value FROM source_checksums WHERE sha256 = %s"
            (quote sha256)));
    !out

  let gc ~cache =
    if not (Sys.file_exists (db_path ~cache)) then 0
    else
      with_db ~cache @@ fun db ->
      let orphans = ref [] in
      let cb row =
        match row with [| sha |] -> orphans := sha :: !orphans | _ -> ()
      in
      ignore
        (Sqlite3.exec_not_null_no_headers db ~cb
           "SELECT s.sha256 FROM sources s LEFT JOIN source_refs r ON r.sha256 \
            = s.sha256 WHERE r.sha256 IS NULL");
      let removed = ref 0 in
      List.iter
        (fun sha ->
          let cks = checksums_for_sha256 db sha in
          List.iter
            (fun (algo, value) ->
              let shard =
                if String.length value >= 2 then String.sub value 0 2 else value
              in
              let path = dir ~cache / algo / shard / value in
              try
                Unix.unlink path;
                incr removed
              with Unix.Unix_error _ -> ())
            cks;
          exec db (Fmt.str "DELETE FROM sources WHERE sha256 = %s" (quote sha)))
        !orphans;
      !removed

  let verify ~sys:_ ~cache =
    if not (Sys.file_exists (db_path ~cache)) then []
    else
      with_db ~cache @@ fun db ->
      let bad = ref [] in
      let cb row =
        match row with
        | [| sha256; algo; value |] ->
            let shard =
              if String.length value >= 2 then String.sub value 0 2 else value
            in
            let path = dir ~cache / algo / shard / value in
            if not (Sys.file_exists path) then
              bad := (sha256, Fmt.str "missing: %s" path) :: !bad
            else begin
              let kind =
                match algo with
                | "md5" -> `MD5
                | "sha256" -> `SHA256
                | "sha512" -> `SHA512
                | other ->
                    bad := (sha256, Fmt.str "unknown algo: %s" other) :: !bad;
                    `SHA256
              in
              let got = OpamHash.contents (OpamHash.compute ~kind path) in
              if got <> value then
                bad :=
                  (sha256, Fmt.str "%s mismatch at %s: %s" algo path got)
                  :: !bad
            end
        | _ -> ()
      in
      ignore
        (Sqlite3.exec_not_null_no_headers db ~cb
           "SELECT sha256, algo, value FROM source_checksums");
      !bad

  let merge_remote ~fs ~index_path ~remote_path =
    mkdir_p ~fs (Filename.dirname index_path);
    let db = Sqlite3.db_open index_path in
    Fun.protect ~finally:(fun () -> close_db db) @@ fun () ->
    exec db schema;
    exec db "PRAGMA journal_mode=DELETE";
    exec db "PRAGMA synchronous=NORMAL";
    exec db "PRAGMA foreign_keys=ON";
    exec db (Fmt.str "ATTACH DATABASE %s AS remote" (quote remote_path));
    (try
       exec db "BEGIN TRANSACTION";
       exec db "INSERT OR IGNORE INTO sources SELECT * FROM remote.sources";
       exec db
         "INSERT OR IGNORE INTO source_checksums SELECT * FROM \
          remote.source_checksums";
       exec db
         "INSERT OR IGNORE INTO source_refs SELECT * FROM remote.source_refs";
       exec db "COMMIT"
     with e ->
       (try exec db "ROLLBACK" with _ -> ());
       raise e);
    exec db "DETACH DATABASE remote"

  let export ~cache ~dst =
    let src_dir = dir ~cache in
    if not (Sys.file_exists src_dir) then 0
    else
      let fs = Cache.fs cache in
      let dst_root = Eio.Path.(dst / "sources") in
      let dst_s = Eio.Path.native_exn dst_root in
      mkdir_p ~fs dst_s;
      if Sys.file_exists (db_path ~cache) then
        link_or_copy ~fs ~src:(db_path ~cache) ~dst:(dst_s / "index.db");
      let algo_dirs = [ "md5"; "sha256"; "sha512" ] in
      let count = ref 0 in
      List.iter
        (fun algo ->
          let algo_src = src_dir / algo in
          if Sys.file_exists algo_src then begin
            let shards = Sys.readdir algo_src in
            Array.iter
              (fun shard ->
                let shard_src = algo_src / shard in
                if Sys.is_directory shard_src then begin
                  let files = Sys.readdir shard_src in
                  Array.iter
                    (fun hash ->
                      let sp = shard_src / hash in
                      if not (Sys.is_directory sp) then begin
                        let dp = dst_s / algo / shard / hash in
                        link_or_copy ~fs ~src:sp ~dst:dp;
                        incr count
                      end)
                    files
                end)
              shards
          end)
        algo_dirs;
      !count
end
