let ( / ) = Filename.concat

(* Copy [src] to [dst], dereferencing if [src] is a symlink. The dune
   install tree (and the per-layer [fs/bin] dirs in the d10 cache) hand
   us symlinks pointing back to a [.exe]; a naive [cp] / [Sys.rename]
   would carry the link out and break CI artifact uploads.
   [Unix.openfile] follows symlinks by default.

   Atomic publish: write to [<dst>.<pid>.tmp] then [rename] over [dst].
   Direct in-place [O_TRUNC] of [dst] fails with [ETXTBSY] ("Text file
   busy") when [dst] is an executable currently running — typical case
   is [oi install] overwriting [~/.local/bin/oi] from a build kicked
   off by that same binary. [rename] swaps inodes, leaving the
   running process attached to the old one until it exits. The pid
   suffix on the tmp name keeps concurrent installs from clobbering
   each other's staging files.

   Perms are normalised on write: dune / opam install trees are
   typically read-only ([555] for binaries, [444] for data), which makes
   the resulting [dist/] awkward for downstream consumers that want to
   strip, sign, or replace artefacts. We keep the executable bit (if
   any of u/g/o-x was set on the source) and write [0o755], otherwise
   [0o644]. *)
let copy_resolved ~src ~dst =
  let buf_size = 65536 in
  let buf = Bytes.create buf_size in
  let src_perm = (Unix.stat src).st_perm in
  let perm = if src_perm land 0o111 <> 0 then 0o755 else 0o644 in
  let tmp = Fmt.str "%s.%d.tmp" dst (Unix.getpid ()) in
  let ic = Unix.openfile src [ O_RDONLY ] 0 in
  let oc = Unix.openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC ] perm in
  let rec loop () =
    let n = Unix.read ic buf 0 buf_size in
    if n > 0 then begin
      let _ : int = Unix.write oc buf 0 n in
      loop ()
    end
  in
  let committed = ref false in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.close ic with _ -> ());
      (try Unix.close oc with _ -> ());
      if not !committed then try Unix.unlink tmp with _ -> ())
    (fun () ->
      loop ();
      Unix.chmod tmp perm;
      Unix.rename tmp dst;
      committed := true)

(* List the regular files (or symlinks to regular files) under [dir],
   sorted by basename. Missing [dir] yields []. *)
let list_files dir =
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
    |> List.filter_map (fun name ->
        let src = dir / name in
        match (Unix.stat src).st_kind with
        | S_REG -> Some (name, src)
        | _ -> None
        | exception Unix.Unix_error _ -> None)

(* Mirror [src] into [dst], creating directories on demand and resolving
   any symlink encountered into a regular-file copy. [files] accumulates
   [(relative-path, full-dst-path)] for every regular file we wrote.
   Other unix file kinds (sockets, fifos, …) are silently skipped — none
   appear in an opam install tree in practice. *)
let rec copy_tree_resolved ~src ~dst ~rel ~files =
  (try Unix.mkdir dst 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  let entries =
    try Sys.readdir src |> Array.to_list |> List.sort String.compare
    with Sys_error _ -> []
  in
  List.iter
    (fun name ->
      let s = src / name in
      let d = dst / name in
      let r = if rel = "" then name else rel / name in
      match (Unix.stat s).st_kind with
      | S_REG ->
          copy_resolved ~src:s ~dst:d;
          files := (r, d) :: !files
      | S_DIR -> copy_tree_resolved ~src:s ~dst:d ~rel:r ~files
      | _ | (exception Unix.Unix_error _) -> ())
    entries

(* Copy the opam install layout under [root] into [dst], mirroring the
   sub-tree per directory:
   - [bin/]   -> [<dst>/bin/<name>]
   - [sbin/]  -> [<dst>/sbin/<name>]
   - [share/] -> [<dst>/share/<rel>]   (recursive)
   Returns the [(subdir, relative-name, written-path)] triples for every
   regular file written, in [bin, sbin, share] order. Missing source
   sub-dirs are silently skipped. *)
let collect_install ~root ~dst =
  (try Unix.mkdir dst 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  let flat sub =
    let src_dir = root / sub in
    if not (Sys.file_exists src_dir) then []
    else begin
      let sub_dst = dst / sub in
      (try Unix.mkdir sub_dst 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
      List.map
        (fun (name, src) ->
          let d = sub_dst / name in
          copy_resolved ~src ~dst:d;
          (sub, name, d))
        (list_files src_dir)
    end
  in
  let bin = flat "bin" in
  let sbin = flat "sbin" in
  let share =
    let src_dir = root / "share" in
    if not (Sys.file_exists src_dir) then []
    else begin
      let files = ref [] in
      copy_tree_resolved ~src:src_dir ~dst:(dst / "share") ~rel:"" ~files;
      List.rev_map (fun (rel, d) -> ("share", rel, d)) !files
    end
  in
  bin @ sbin @ share
