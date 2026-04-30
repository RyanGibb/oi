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
    D10.Sysops.Curl.fetch sys
      ~url:(Layer_index.url_join registry rel)
      ~dst:Eio.Path.(fs / dst)
  end

(* Layer-cache + sources publish helper, called by [oi build --export DIR]. *)
let do_registry_export ~fs ~clock ~sys ~os_key ~cache ~registry ~output =
  let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
  let dst = Eio.Path.(fs / output) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let count = D10.Layer.export_all d10 ~dst in
  Fmt.pr "Exported %d layer(s) to %s@." count output;
  if Eio.Path.is_directory Eio.Path.(fs / output / os_key) then begin
    let index_path = output / os_key / "index.db" in
    (try Sys.remove index_path with Sys_error _ -> ());
    let db = D10.Index.open_ ~path:index_path in
    D10.Index.rebuild d10 db;
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
    Fmt.pr "  %s: %d layers, %d binaries@." os_key nl nb
  end;
  let n_sources = Oi.Source.Mirror.export ~cache ~dst in
  if n_sources > 0 then
    Fmt.pr "  sources: %d blob(s) at %s/sources/@." n_sources output;
  (* Build-log manifest: one bundled JSON file with every per-package
     record from this cache, summarised by outcome. *)
  let cache_root = Oi.Cache.root_s cache in
  let records = Oi.Build_log.Manifest.read_sidecars ~fs ~cache_root in
  if records <> [] then begin
    let manifest =
      Oi.Build_log.Manifest.of_records ~os_key
        ~exported_at:(Unix.gettimeofday ()) records
    in
    let logs_dir = output / os_key / "logs" in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / logs_dir);
    let path = logs_dir / "manifest.json" in
    match
      Jsont_bytesrw.encode_string ~format:Jsont.Indent
        Oi.Build_log.Manifest.codec manifest
    with
    | Ok s ->
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) s;
        Fmt.pr "  logs: %d record(s) at %s@." manifest.n_packages path
    | Error e -> Logs.warn (fun m -> m "manifest encode failed: %s" e)
  end
