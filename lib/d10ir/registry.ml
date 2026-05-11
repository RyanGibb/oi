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

(* In-process single-flight tracker keyed by sha. Two fibers asking for
   the same archive used to race on a shared tmp path: the loser would
   [sha256_of_file] a tmp the winner had already renamed away, get [""],
   and emit a spurious "sha mismatch ... got " warning even though the
   bytes at [dst] were correct.

   The first caller for a given sha "claims" it (registers a promise
   and runs the actual fetch); concurrent callers "join" by awaiting
   the claimant's promise and consume the same result. The table entry
   is removed in [Fun.protect ~finally] so an exception in the body
   still wakes joiners (with [false]) instead of leaking a stuck
   promise. *)
module In_flight = struct
  type action = Claim of bool Eio.Promise.u | Join of bool Eio.Promise.t

  let table : (string, bool Eio.Promise.t) Hashtbl.t = Hashtbl.create 16
  let mu = Mutex.create ()

  let acquire sha =
    Mutex.lock mu;
    let r =
      match Hashtbl.find_opt table sha with
      | Some p -> Join p
      | None ->
          let p, u = Eio.Promise.create () in
          Hashtbl.replace table sha p;
          Claim u
    in
    Mutex.unlock mu;
    r

  let release sha u result =
    Mutex.lock mu;
    Hashtbl.remove table sha;
    Mutex.unlock mu;
    Eio.Promise.resolve u result
end

let sha256_of_file path =
  OpamHash.contents (OpamHash.compute ~kind:`SHA256 path)

let do_fetch ~fs ~session ~cache_root ~remote ~sha ~dst =
  let dst_dir = archives_dir ~cache_root in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dst_dir);
  let tmp = unique_tmp dst in
  let url = url_of ~remote ~sha in
  Log.debug (fun m -> m "fetch %s -> %s" url dst);
  if D10.Sysops.Http.fetch_session session ~url ~dst:Eio.Path.(fs / tmp) then
    let actual = try sha256_of_file tmp with _ -> "" in
    if actual = sha then begin
      (try Sys.rename tmp dst with _ -> ( try Unix.unlink tmp with _ -> ()));
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

let pull ~fs ~session ~cache_root ~remote ~sha =
  let dst = archive_path ~cache_root ~sha in
  if Sys.file_exists dst then true
  else
    match In_flight.acquire sha with
    | Join p -> Eio.Promise.await p
    | Claim u ->
        let ok = ref false in
        Fun.protect
          ~finally:(fun () -> In_flight.release sha u !ok)
          (fun () ->
            let r = do_fetch ~fs ~session ~cache_root ~remote ~sha ~dst in
            ok := r;
            r)

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
