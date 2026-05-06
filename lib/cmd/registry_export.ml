let ( / ) = Filename.concat

(* Sqlite in WAL mode leaves [<path>-wal] and [<path>-shm] sidecars
   next to the main [.db] on close. Plain [Sys.remove path] doesn't
   touch them. *)
let unlink_sqlite_sidecars path =
  List.iter
    (fun s -> try Sys.remove (path ^ s) with Sys_error _ -> ())
    [ "-wal"; "-shm"; "-journal" ]

let remove_sqlite_scratch path =
  unlink_sqlite_sidecars path;
  try Sys.remove path with Sys_error _ -> ()

(* Collapse any WAL/SHM sidecars into [path] so the published index.db
   is self-contained. *)
let finalize_sqlite_for_publish path =
  if Sys.file_exists path then begin
    (try
       let db = Sqlite3.db_open path in
       Fun.protect
         ~finally:(fun () -> ignore (Sqlite3.db_close db))
         (fun () -> ignore (Sqlite3.exec db "PRAGMA journal_mode=DELETE"))
     with _ -> ());
    unlink_sqlite_sidecars path
  end

let fetch_remote_to ~sys ~fs ~registry ~rel ~dst =
  if registry = "" then false
  else begin
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / Filename.dirname dst);
    D10.Sysops.Http.fetch sys
      ~url:(Layer_index.url_join registry rel)
      ~dst:Eio.Path.(fs / dst)
  end

(* Layer-cache + sources publish helper, called by [oi build --export DIR]. *)
let run ~fs ~clock ~sys ~os_key ~cache ~registry ~output =
  let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
  let dst = Eio.Path.(fs / output) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let count = D10.Layer.export_all d10 ~dst in
  Oi.Say.step "Exported %d layer(s) to %s" count output;
  if Eio.Path.is_directory Eio.Path.(fs / output / os_key) then begin
    let index_path = output / os_key / "index.db" in
    (try Sys.remove index_path with Sys_error _ -> ());
    let db = D10.Index.open_ ~path:index_path in
    let cache_root = Oi.Cache.root_s cache in
    let overlay_for ~hash =
      Oi.Provenance.overlay_of_layer ~fs ~cache_root ~os_key ~hash
    in
    D10.Index.rebuild d10 ~overlay_for db;
    if registry <> "" then begin
      let scratch = output / os_key / ".remote-index.db" in
      if
        fetch_remote_to ~sys ~fs ~registry ~rel:(os_key / "index.db")
          ~dst:scratch
      then begin
        (try D10.Index.merge_remote db ~remote_path:scratch
         with Failure msg ->
           Logs.warn (fun m -> m "Failed to merge remote layer index: %s" msg));
        remove_sqlite_scratch scratch
      end
      else
        Logs.info (fun m ->
            m "No remote layer index at %s/%s/index.db (skipping merge)"
              registry os_key)
    end;
    let nl, nb, _ = D10.Index.stats db ~os_key in
    D10.Index.close db;
    finalize_sqlite_for_publish index_path;
    Oi.Say.field "index" "%s: %d layers, %d binaries" os_key nl nb
  end;
  let n_sources = Oi.Source.Mirror.export ~cache ~dst in
  if n_sources > 0 then
    Oi.Say.field "sources" "%d blob(s) at %s/sources/" n_sources output;
  (* Manifest = Provenance ⨝ Audit. Provenance gives us one entry per
     successfully committed layer with its content fields; the audit log
     gives us a [callers[]] history per layer. Failed-build events that
     have no corresponding layer surface as separate entries. *)
  let cache_root = Oi.Cache.root_s cache in
  let provs = Oi.Provenance.read_all ~fs ~cache_root ~os_key in
  let events = Oi.Audit.read_all ~fs ~cache_root ~os_key in
  if provs <> [] || events <> [] then begin
    let manifest =
      Oi.Manifest.from_logs ~os_key ~exported_at:(Unix.gettimeofday ()) provs
        events
    in
    let logs_dir = output / os_key / "logs" in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / logs_dir);
    let path = logs_dir / "manifest.json" in
    (match
       Jsont_bytesrw.encode_string ~format:Jsont.Indent Oi.Manifest.codec
         manifest
     with
    | Ok s ->
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) s;
        Oi.Say.field "manifest" "%d entry(ies) at %s" manifest.n_packages path
    | Error e -> Logs.warn (fun m -> m "manifest encode failed: %s" e));
    (* Ship the per-os audit slice so multi-host registry merges can
       union events. Sorted by event_id (ULID = monotonic) so the file is
       deterministic. *)
    if events <> [] then begin
      Oi.Audit.write_per_os ~fs ~output_dir:output ~os_key events;
      Oi.Say.field "audit" "%d event(s) at %s/audit.jsonl" (List.length events)
        (output / os_key)
    end
  end;
  (* Emit per-overlay-handle markdown reports aggregated across every distro
     currently staged under [output]. Re-run from each export so a freshly
     synced sibling distro picks up the latest view. *)
  let handles_written =
    Oi.Handle_report.write_all ~fs ~output_dir:output
      ~generated_at:(Unix.gettimeofday ())
  in
  match handles_written with
  | [] -> ()
  | hs ->
      Oi.Say.field "handles" "%d failure report(s) at %s/handles/"
        (List.length hs) output
