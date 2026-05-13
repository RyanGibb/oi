[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Fetches the published [index.db] from a remote and returns the
   [hash -> {sha256; size}] map for whichever layers have a
   tarball published (the [layers.tarball_sha256] column).

   Lives outside [Layer] because reading [index.db] requires
   [Index] and [Index.rebuild] depends on [Layer.load_meta] — so a
   straight [Layer.fetch_remote_index] body that imports [Index]
   would close a dependency cycle. The cycle break: [Layer] holds
   the [remote_index] type only; this module supplies the
   IO-touching helper that materialises one. *)

let log_src = Logs.Src.create "d10.remote_index"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* Process-wide memo: an [oi build --all] solve fires
   [fetch_remote_index] once per group; second-and-later calls hit
   this cache and skip the HTTP fetch + sqlite open entirely. *)
let cache : (string, Layer.remote_index) Hashtbl.t = Hashtbl.create 4

let fetch (c : Config.t) ~session ~remote =
  let url =
    match remote with
    | `Http_remote base -> Fmt.str "%s/%s/index.db" base c.os_key
  in
  match Hashtbl.find_opt cache url with
  | Some cached -> cached
  | None ->
      let os_layer_dir = Eio.Path.(c.root / "layers" / c.os_key) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_layer_dir;
      let tmp = Eio.Path.(os_layer_dir / ".remote-index.db.tmp") in
      let idx : Layer.remote_index = Hashtbl.create 64 in
      if Sysops.Http.fetch_session session ~url ~dst:tmp then begin
        let path = Eio.Path.native_exn tmp in
        (try
           let db = Index.open_ ~fs:c.fs ~path in
           Fun.protect
             ~finally:(fun () -> Index.close db)
             (fun () ->
               let rows = Index.all_tarballs db ~os_key:c.os_key in
               List.iter
                 (fun (hash, sha, size) ->
                   Hashtbl.replace idx hash { Layer.sha256 = sha; size })
                 rows;
               Log.info (fun m ->
                   m "Loaded %d tarball entries from %s" (List.length rows) url))
         with exn ->
           Log.info (fun m ->
               m "Could not read %s: %s" url (Printexc.to_string exn)));
        try Eio.Path.unlink tmp with _ -> ()
      end;
      Hashtbl.replace cache url idx;
      idx
