[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Publishing helper for the consolidated source archives that live
   under [<cache>/d10ir/archives/<sha>.tar.zst]. Used by:
   - [oi build --export DIR]: ships every locally-baked archive
     alongside the layer tarballs.
   - [oi repo bake [HANDLE] --to=DIR]: bakes a handle's archives
     and ships just those to [DIR/d10ir-archives/].

   Hardlink-first; falls back to a streamed copy when the dst is on
   a different filesystem. The destination path matches what
   {!D10ir.Registry.pull} expects:
     [<base_url>/d10ir-archives/<sha>.tar.zst] *)

let ( / ) = Filename.concat

let local_dir ~cache =
  Cache.root_s cache / "d10ir" / "archives"

let dst_dir ~output = output / "d10ir-archives"

(* Single-file publish: hardlink, falling back to a streamed copy.
   Already-present (same name) at [dst] short-circuits. *)
let publish_one ~src ~dst =
  if Sys.file_exists dst then false
  else
    try
      Unix.link src dst;
      true
    with Unix.Unix_error _ -> (
      try
        let ic = open_in_bin src in
        let oc = open_out_bin dst in
        let buf = Bytes.create 65536 in
        let rec loop () =
          let n = input ic buf 0 (Bytes.length buf) in
          if n > 0 then begin
            Stdlib.output oc buf 0 n;
            loop ()
          end
        in
        Fun.protect
          ~finally:(fun () ->
            close_in_noerr ic;
            close_out_noerr oc)
          loop;
        true
      with _ -> false)

(* Publish every [<sha>.tar.zst] under [<cache>/d10ir/archives/] into
   [<output>/d10ir-archives/]. Returns the count of newly-linked /
   copied entries. Files that already exist at the destination are
   left alone (idempotent on repeat invocations). *)
let publish_all ~cache ~output =
  let src = local_dir ~cache in
  if not (Sys.file_exists src) then 0
  else begin
    let dst = dst_dir ~output in
    (try Unix.mkdir dst 0o755
     with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let entries =
      try Sys.readdir src |> Array.to_list with Sys_error _ -> []
    in
    List.fold_left
      (fun n name ->
        if Filename.check_suffix name ".tar.zst"
           && publish_one ~src:(src / name) ~dst:(dst / name)
        then n + 1
        else n)
      0 entries
  end

(* Publish only the [<sha>.tar.zst]s named in [shas]. Used by
   [oi repo bake HANDLE --to=DIR] which bakes a focused subset and
   ships just those archives, leaving the rest of [<cache>/d10ir/
   archives/] (perhaps from older runs) out of [DIR]. *)
let publish_shas ~cache ~output shas =
  if shas = [] then 0
  else begin
    let src = local_dir ~cache in
    let dst = dst_dir ~output in
    (try Unix.mkdir dst 0o755
     with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    List.fold_left
      (fun n sha ->
        let name = sha ^ ".tar.zst" in
        let sp = src / name in
        if Sys.file_exists sp && publish_one ~src:sp ~dst:(dst / name)
        then n + 1
        else n)
      0 shas
  end
