[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Opam repository clones. Re-exported as [Source.Repo]. *)

let ( / ) = Filename.concat
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

let dir ~data_dir name = data_dir / "repos" / name

let dir_needs_refresh dir =
  try
    let st = Unix.stat dir in
    Unix.time () -. st.Unix.st_mtime > Cache.refresh_max_age
  with Unix.Unix_error _ -> true

let ensure ?(reporter = Build_progress.null) ~fs ~refresh ~label ~url ~dir () =
  let pkg_dir = dir / "packages" in
  if not (Sys.file_exists pkg_dir) then begin
    if Sys.file_exists dir then begin
      Log.info (fun m ->
          m "Re-cloning %s (existing clone at %s has no packages/)" label dir);
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir)
    end;
    Log.info (fun m -> m "Cloning %s from %s..." label url);
    Fmt.kstr
      (fun s -> reporter.Build_progress.event (Status s))
      "Cloning %s" label;
    pull_repo ~label ~url_s:url ~dst:dir;
    touch_dir dir
  end
  else if refresh || dir_needs_refresh dir then begin
    Log.info (fun m -> m "Updating %s..." label);
    Fmt.kstr
      (fun s -> reporter.Build_progress.event (Status s))
      "Updating %s" label;
    try
      pull_repo ~label ~url_s:url ~dst:dir;
      touch_dir dir
    with exn ->
      Log.warn (fun m ->
          m "Failed to update %s: %s" label (Printexc.to_string exn))
  end

let ensure_many ?(reporter = Build_progress.null) ~fs ~data_dir
    ?(refresh = false) extras =
  List.map
    (fun (e : Project.extra_repo) ->
      match e.local_packages_dir with
      | Some d -> d
      | None ->
          let path = dir ~data_dir e.name in
          ensure ~reporter ~fs ~refresh ~label:e.name ~url:e.url ~dir:path ();
          path / "packages")
    extras
