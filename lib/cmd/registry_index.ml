open Cmdliner

let ( / ) = Filename.concat

[@@@warning "-32"]

let cmd =
  let run () cache_dir =
    Harness.run @@ fun env ->
    let {
      Harness.proc_mgr = _proc_mgr;
      fs;
      clock;
      sys;
      platform = _platform;
      os_key = _os_key;
      cache = _cache;
    } =
      Harness.bootstrap env cache_dir
    in
    let layers_root = cache_dir / "layers" in
    let total_layers = ref 0 in
    let total_bins = ref 0 in
    let total_files = ref 0 in
    if Sys.file_exists layers_root then
      Array.iter
        (fun entry ->
          let dir = layers_root / entry in
          if Sys.is_directory dir && entry.[0] <> '.' then begin
            let index_path = dir / "index.db" in
            let db = D10.Index.open_ ~path:index_path in
            D10.Index.rebuild
              {
                D10.Config.sys;
                fs;
                clock :> D10.Config.clk;
                root = Eio.Path.(fs / cache_dir);
                os_key = entry;
              }
              db;
            let nl, nb, nf = D10.Index.stats db ~os_key:entry in
            D10.Index.close db;
            Fmt.pr "  %s: %d layers, %d binaries, %d files@." entry nl nb nf;
            total_layers := !total_layers + nl;
            total_bins := !total_bins + nb;
            total_files := !total_files + nf
          end)
        (Sys.readdir layers_root);
    Fmt.pr "Total: %d layers, %d binaries, %d files@." !total_layers !total_bins
      !total_files
  in
  let info =
    Cmd.info "index" ~doc:"Rebuild the fast-lookup index over the local cache"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi) maintains a small SQLite database next to the cache that \
             maps every installed binary and cached package back to the layer \
             that provides it. The database is what makes $(b,oi search) and \
             the binary-name lookup in $(b,oi run) fast. This command rebuilds \
             the database from scratch by walking every cached package.";
          `P
            "A rebuild is not needed in normal use. Run it if $(b,oi search) \
             starts missing a result that you know is cached, or after editing \
             the cache directory by hand.";
        ]
  in
  Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)

(* -- registry ------------------------------------------------------------ *)

(* Remove a sqlite scratch file together with its WAL/SHM siblings.
   sqlite in WAL journal_mode leaves [-wal] and [-shm] files next to
   the main [.db] on close, and plain [Sys.remove] on just the [.db]
   leaves orphans behind — visible in the published sources/ tree. *)
let remove_sqlite_scratch path =
  List.iter
    (fun p -> try Sys.remove p with Sys_error _ -> ())
    [ path; path ^ "-wal"; path ^ "-shm"; path ^ "-journal" ]

(* Collapse any WAL/SHM sidecars next to [path] into the main database.
   Runs [PRAGMA journal_mode=DELETE], which checkpoints outstanding WAL
   pages into the main file and removes the [-wal]/[-shm] files. Used
   at the tail of [registry export] so the published index.db files
   are self-contained — rsync'ing the sources/ tree doesn't need to
   copy or create WAL siblings on the remote. *)
let finalize_sqlite_for_publish path =
  if Sys.file_exists path then begin
    (try
       let db = Sqlite3.db_open path in
       Fun.protect
         ~finally:(fun () -> ignore (Sqlite3.db_close db))
         (fun () -> ignore (Sqlite3.exec db "PRAGMA journal_mode=DELETE"))
     with _ -> ());
    (* sqlite's WAL→DELETE transition truncates the [-wal] but may
       leave the zero-byte [-shm] sidecar behind. At this point both
       are orphans — the main db owns no WAL state — so unlink any
       leftovers directly. *)
    List.iter
      (fun suffix -> try Sys.remove (path ^ suffix) with Sys_error _ -> ())
      [ "-wal"; "-shm"; "-journal" ]
  end

(* Fetch [registry]/<rel> to [dst] via curl. Returns true on success,
   false otherwise (404, network error, empty response). The caller
   decides how to react (typically: skip the remote merge). *)
let fetch_remote_to ~sys ~fs ~registry ~rel ~dst =
  if registry = "" then false
  else begin
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / Filename.dirname dst);
    D10.Sysops.Curl.fetch sys
      ~url:(Layer_index.url_join registry rel)
      ~dst:Eio.Path.(fs / dst)
  end

(* Body of [oi registry export]; kept as its own function so other
   callers (tests, future commands) can drive it without going
   through cmdliner. *)
let do_registry_export ~fs ~clock ~sys ~os_key ~cache ~registry ~output =
  let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
  let dst = Eio.Path.(fs / output) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let count = D10.Layer.export_all d10 ~dst in
  Fmt.pr "Exported %d layer(s) to %s@." count output;
  (* Rebuild the index.db only for this container's os_key. Sibling os_key
     subdirs may exist alongside ours when [dst] is a shared volume (e.g.
     docker-compose bind mount) — leave their indices alone. *)
  if Sys.file_exists (output / os_key) then begin
    let index_path = output / os_key / "index.db" in
    (try Sys.remove index_path with Sys_error _ -> ());
    let db = D10.Index.open_ ~path:index_path in
    D10.Index.rebuild d10 db;
    (* If a remote registry is configured, fetch its current
       <os_key>/index.db into a scratch file and merge those rows
       in. This keeps rows for layers that live on the remote but
       haven't been rebuilt locally this run — important for rsync:
       without it, the published index would shrink to just what the
       caller happens to have cached. *)
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
  (* Sources are OS-independent — publish them once at the registry
     top level (sources/), not per os_key. A sibling [oi registry
     export] from a different arch/distro will merge into the same
     tree: blobs are content-addressed so collisions are correctness-
     preserving. *)
  let n_sources = Oi.Source.Mirror.export ~cache ~dst in
  if n_sources > 0 then
    Fmt.pr "  sources: %d blob(s) at %s/sources/@." n_sources output
