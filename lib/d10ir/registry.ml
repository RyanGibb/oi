let ( / ) = Filename.concat
let log_src = Logs.Src.create "d10ir.registry"

module Log = (val Logs.src_log log_src : Logs.LOG)

type remote = [ `Http_remote of string ]

let archives_dir ~cache_root = cache_root / "d10ir" / "archives"
let archive_path ~cache_root ~sha = archives_dir ~cache_root / (sha ^ ".tar.zst")

let trim_trailing_slash s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

let url_of ~remote ~sha =
  let (`Http_remote base) = remote in
  Fmt.str "%s/d10ir-archives/%s.tar.zst" (trim_trailing_slash base) sha

let unique_tmp dst = Fmt.str "%s.%d.tmp" dst (Unix.getpid ())

let sha256_of_file path =
  OpamHash.contents (OpamHash.compute ~kind:`SHA256 path)

let pull ~fs ~session ~cache_root ~remote ~sha =
  let dst = archive_path ~cache_root ~sha in
  if Sys.file_exists dst then true
  else begin
    let dst_dir = archives_dir ~cache_root in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dst_dir);
    let tmp = unique_tmp dst in
    let url = url_of ~remote ~sha in
    Log.debug (fun m -> m "fetch %s -> %s" url dst);
    if D10.Sysops.Http.fetch_session session ~url ~dst:Eio.Path.(fs / tmp) then
      let actual = try sha256_of_file tmp with _ -> "" in
      if actual = sha then begin
        (try Sys.rename tmp dst
         with _ -> ( try Unix.unlink tmp with _ -> ()));
        true
      end
      else begin
        Log.warn (fun m ->
            m "sha mismatch for %s: declared %s, got %s" url sha actual);
        (try Unix.unlink tmp with _ -> ());
        false
      end
    else begin
      (try Unix.unlink tmp with _ -> ());
      false
    end
  end

type prefetch_summary = {
  fetched : int;
  cached : int;
  failed : int;
  missing : string list;
}

let prefetch ~clock ~fs ~session ~cache_root ~remote ?(max_fibers = 16) shas =
  let _ = clock in
  let fetched = Atomic.make 0 in
  let cached = Atomic.make 0 in
  let failed = Atomic.make 0 in
  let missing = ref [] in
  let mutex = Mutex.create () in
  let do_one sha =
    let dst = archive_path ~cache_root ~sha in
    if Sys.file_exists dst then Atomic.incr cached
    else if pull ~fs ~session ~cache_root ~remote ~sha then Atomic.incr fetched
    else begin
      Atomic.incr failed;
      Mutex.lock mutex;
      missing := sha :: !missing;
      Mutex.unlock mutex
    end
  in
  Eio.Fiber.List.iter ~max_fibers do_one shas;
  {
    fetched = Atomic.get fetched;
    cached = Atomic.get cached;
    failed = Atomic.get failed;
    missing = List.rev !missing;
  }
