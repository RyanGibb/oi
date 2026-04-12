[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

let hash_opam_file packages_dirs pkg =
  let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let pkg_s = OpamPackage.to_string pkg in
  let path =
    List.find_map
      (fun dir ->
        let p = dir / name_s / pkg_s / "opam" in
        if Sys.file_exists p then Some p else None)
      packages_dirs
  in
  match path with
  | Some p ->
      OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw p))
      |> OpamFile.OPAM.effective_part |> OpamFile.OPAM.write_to_string
      |> OpamHash.compute_from_string |> OpamHash.to_string
  | None -> Digest.string (OpamPackage.to_string pkg) |> Digest.to_hex

let hash ~packages_dirs pkgs =
  let hashes = List.map (hash_opam_file packages_dirs) pkgs in
  String.concat " " hashes |> Digest.string |> Digest.to_hex

type meta = {
  package : string;
  exit_status : int;
  deps : string list;
  hashes : string list;
  created : float;
}

let meta_jsont : meta Jsont.t =
  let open Jsont in
  Object.map ~kind:"layer meta" (fun package exit_status deps hashes created ->
      { package; exit_status; deps; hashes; created })
  |> Object.mem "package" string ~enc:(fun i -> i.package)
  |> Object.mem "exit_status" int ~enc:(fun i -> i.exit_status)
  |> Object.mem "deps" (list string) ~dec_absent:[] ~enc:(fun i -> i.deps)
  |> Object.mem "hashes" (list string) ~dec_absent:[] ~enc:(fun i -> i.hashes)
  |> Object.mem "created" number ~enc:(fun i -> i.created)
  |> Object.finish

let save_meta path meta =
  let dir = Eio.Path.split path |> Option.map fst in
  Option.iter (Eio.Path.mkdirs ~exists_ok:true ~perm:0o755) dir;
  match Jsont_bytesrw.encode_string meta_jsont meta with
  | Ok s -> Eio.Path.save ~create:(`Or_truncate 0o644) path s
  | Error e -> Fmt.failwith "layer.json encode: %s" e

let load_meta path =
  try
    let s = Eio.Path.load path in
    let file =
      match Eio.Path.native path with Some s -> s | None -> "<layer.json>"
    in
    match Jsont_bytesrw.decode_string ~locs:true ~file meta_jsont s with
    | Ok meta -> Some meta
    | Error e ->
        Logs.warn (fun m -> m "Bad layer.json: %s" e);
        None
  with Eio.Exn.Io _ -> None

let dir (c : Config.t) ~hash = Eio.Path.(c.root / "layers" / c.os_key / hash)
let json_path c ~hash = Eio.Path.(dir c ~hash / "layer.json")
let fs_path c ~hash = Eio.Path.(dir c ~hash / "fs")
let native p = Eio.Path.native_exn p
let dir_s c ~hash = native (dir c ~hash)
let exists c ~hash = Sysops.file_exists (json_path c ~hash)

let succeeded c ~hash =
  match load_meta (json_path c ~hash) with
  | Some m -> m.exit_status = 0
  | None -> false

let store (c : Config.t) ~hash ~prefix ~files ~package ~deps ~parent_hashes
    ~exit_status =
  let d = dir_s c ~hash in
  let fs_dir = d / "fs" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(c.fs / fs_dir);
  let replace_existing f dst =
    try f dst
    with Unix.Unix_error (Unix.EEXIST, _, _) ->
      Sys.remove dst;
      f dst
  in
  List.iter
    (fun rel_path ->
      let src = prefix / rel_path in
      let dst = fs_dir / rel_path in
      match
        try Some (Unix.lstat src).Unix.st_kind with Unix.Unix_error _ -> None
      with
      | Some Unix.S_LNK ->
          let target = Unix.readlink src in
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
            Eio.Path.(c.fs / Filename.dirname dst);
          replace_existing (Unix.symlink target) dst
      | Some Unix.S_REG ->
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
            Eio.Path.(c.fs / Filename.dirname dst);
          replace_existing (Unix.link src) dst
      | _ -> ())
    files;
  save_meta (json_path c ~hash)
    {
      package;
      exit_status;
      deps;
      hashes = parent_hashes;
      created = Eio.Time.now c.clock;
    }

let restore (c : Config.t) ~hash ~prefix =
  let fs_dir = fs_path c ~hash in
  if Sysops.file_exists fs_dir then
    Sysops.link_tree c.sys ~src:fs_dir ~dst:Eio.Path.(c.fs / prefix)
