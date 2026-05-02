let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.provenance"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Types --------------------------------------------------------------- *)

type pkg_id = { name : string; version : string }
type method_ = Source | Binary

type opam_origin_kind = Reporepo | Pin | Url_project | Local

type opam_origin = {
  kind : opam_origin_kind;
  handle : string option;
  path_in_repo : string;
}

type opam_info = { sha256 : string; origin : opam_origin }
type source_info = { url : string; kind : string; checksums : string list }
type dep = { name : string; version : string; hash : string }

type phases = {
  fetch : float option;
  build : float option;
  install : float option;
  restore : float option;
}

type build_env = { ocaml_version : string }

type t = {
  schema : int;
  layer_hash : string;
  os_key : string;
  pkg : pkg_id;
  method_ : method_;
  built_at : float;
  duration_s : float;
  phases : phases;
  opam : opam_info;
  source : source_info option;
  deps : dep list;
  depexts_declared : string list;
  build_env : build_env;
}

let empty_phases =
  { fetch = None; build = None; install = None; restore = None }

(* -- Origin / URL classification ----------------------------------------- *)

let classify_url url =
  if url = "" then ""
  else if String.starts_with ~prefix:"git+" url then "git"
  else if String.starts_with ~prefix:"file://" url then "local"
  else
    let ends ext = String.ends_with ~suffix:ext url in
    if List.exists ends [ ".tar.gz"; ".tar.bz2"; ".tar.xz"; ".tar.zst";
                          ".tgz"; ".zip" ]
    then "tar"
    else if url.[0] = '/' then "local"
    else ""

(* Mirror of [Plan.overlay_of_packages_dir]'s path-shape rules, but
   returning a structured [opam_origin] rather than just a (handle, version)
   pair. [path_in_repo] is anchored at whatever segment the detected layout
   uses as its root (reporepo paths start with [v1/...], pin paths with
   [packages/...]). *)
let origin_of_pkgs_dir ~pkgs_dir ~name ~full =
  (* [pkgs_dir] is the [packages/] directory itself; walk upwards three
     levels to disambiguate between the reporepo and pin-set layouts. *)
  let base = Filename.basename (Filename.dirname pkgs_dir) in
  let one_up = Filename.dirname (Filename.dirname pkgs_dir) in
  let parent = Filename.basename one_up in
  let grandparent = Filename.basename (Filename.dirname one_up) in
  let leaf = Fmt.str "%s/%s/opam" name full in
  let path_in_repo prefix = Fmt.str "%s/%s" prefix leaf in
  match (parent, base) with
  | "v1", "reporepo" ->
      {
        kind = Reporepo;
        handle = Some "reporepo";
        path_in_repo = path_in_repo "v1/reporepo/packages";
      }
  | "v1", handle ->
      {
        kind = Reporepo;
        handle = Some handle;
        path_in_repo = path_in_repo (Fmt.str "v1/%s/packages" handle);
      }
  | "sets", _ when grandparent = "pins" ->
      { kind = Pin; handle = None; path_in_repo = path_in_repo "packages" }
  | _ ->
      { kind = Local; handle = None; path_in_repo = path_in_repo "packages" }

(* -- Opam content hash --------------------------------------------------- *)

let hash_opam_file ~path =
  if not (Sys.file_exists path) then ""
  else
    try
      OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path))
      |> OpamFile.OPAM.effective_part
      |> OpamFile.OPAM.write_to_string
      |> (fun s -> Digest.string s)
      |> Digest.to_hex
    with exn ->
      Log.debug (fun m ->
          m "hash_opam_file %s: %s" path (Printexc.to_string exn));
      ""

(* -- Codec --------------------------------------------------------------- *)

let pkg_id_codec : pkg_id Jsont.t =
  let open Jsont in
  Object.map ~kind:"pkg_id" (fun name version : pkg_id -> { name; version })
  |> Object.mem "name" string ~enc:(fun (p : pkg_id) -> p.name)
  |> Object.mem "version" string ~enc:(fun (p : pkg_id) -> p.version)
  |> Object.finish

let method_codec =
  Jsont.enum ~kind:"method" [ ("source", Source); ("binary", Binary) ]

let opam_origin_kind_codec =
  Jsont.enum ~kind:"opam_origin_kind"
    [
      ("reporepo", Reporepo);
      ("pin", Pin);
      ("url-project", Url_project);
      ("local", Local);
    ]

let opam_origin_codec : opam_origin Jsont.t =
  let open Jsont in
  Object.map ~kind:"opam_origin"
    (fun kind handle path_in_repo : opam_origin ->
      { kind; handle; path_in_repo })
  |> Object.mem "kind" opam_origin_kind_codec
       ~enc:(fun (o : opam_origin) -> o.kind)
  |> Object.opt_mem "handle" string ~enc:(fun (o : opam_origin) -> o.handle)
  |> Object.mem "path_in_repo" string
       ~enc:(fun (o : opam_origin) -> o.path_in_repo)
  |> Object.finish

let opam_info_codec =
  let open Jsont in
  Object.map ~kind:"opam" (fun sha256 origin -> { sha256; origin })
  |> Object.mem "sha256" string ~enc:(fun o -> o.sha256)
  |> Object.mem "origin" opam_origin_codec ~enc:(fun o -> o.origin)
  |> Object.finish

let source_info_codec =
  let open Jsont in
  Object.map ~kind:"source" (fun url kind checksums ->
      { url; kind; checksums })
  |> Object.mem "url" string ~enc:(fun s -> s.url)
  |> Object.mem "kind" string ~enc:(fun s -> s.kind)
  |> Object.mem "checksums" (list string) ~dec_absent:[]
       ~enc:(fun s -> s.checksums)
       ~enc_omit:(( = ) [])
  |> Object.finish

let dep_codec =
  let open Jsont in
  Object.map ~kind:"dep" (fun name version hash : dep ->
      { name; version; hash })
  |> Object.mem "name" string ~enc:(fun (d : dep) -> d.name)
  |> Object.mem "version" string ~enc:(fun (d : dep) -> d.version)
  |> Object.mem "hash" string ~enc:(fun (d : dep) -> d.hash)
  |> Object.finish

let phases_codec =
  let open Jsont in
  Object.map ~kind:"phases" (fun fetch build install restore ->
      { fetch; build; install; restore })
  |> Object.opt_mem "fetch" number ~enc:(fun p -> p.fetch)
  |> Object.opt_mem "build" number ~enc:(fun p -> p.build)
  |> Object.opt_mem "install" number ~enc:(fun p -> p.install)
  |> Object.opt_mem "restore" number ~enc:(fun p -> p.restore)
  |> Object.finish

let build_env_codec =
  let open Jsont in
  Object.map ~kind:"build_env" (fun ocaml_version -> { ocaml_version })
  |> Object.mem "ocaml_version" string ~enc:(fun e -> e.ocaml_version)
  |> Object.finish

let codec =
  let open Jsont in
  Object.map ~kind:"provenance"
    (fun
      schema
      layer_hash
      os_key
      pkg
      method_
      built_at
      duration_s
      phases
      opam
      source
      deps
      depexts_declared
      build_env
    ->
      {
        schema;
        layer_hash;
        os_key;
        pkg;
        method_;
        built_at;
        duration_s;
        phases;
        opam;
        source;
        deps;
        depexts_declared;
        build_env;
      })
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "layer_hash" string ~enc:(fun r -> r.layer_hash)
  |> Object.mem "os_key" string ~enc:(fun r -> r.os_key)
  |> Object.mem "pkg" pkg_id_codec ~enc:(fun r -> r.pkg)
  |> Object.mem "method" method_codec ~enc:(fun r -> r.method_)
  |> Object.mem "built_at" number ~enc:(fun r -> r.built_at)
  |> Object.mem "duration_s" number ~enc:(fun r -> r.duration_s)
  |> Object.mem "phases" phases_codec ~dec_absent:empty_phases
       ~enc:(fun r -> r.phases)
       ~enc_omit:(fun p -> p = empty_phases)
  |> Object.mem "opam" opam_info_codec ~enc:(fun r -> r.opam)
  |> Object.opt_mem "source" source_info_codec ~enc:(fun r -> r.source)
  |> Object.mem "deps" (list dep_codec) ~dec_absent:[]
       ~enc:(fun r -> r.deps)
       ~enc_omit:(( = ) [])
  |> Object.mem "depexts_declared" (list string) ~dec_absent:[]
       ~enc:(fun r -> r.depexts_declared)
       ~enc_omit:(( = ) [])
  |> Object.mem "build_env" build_env_codec ~enc:(fun r -> r.build_env)
  |> Object.finish

(* -- Storage ------------------------------------------------------------- *)

let path ~cache_root ~os_key ~hash =
  cache_root / "layers" / os_key / hash / "provenance.json"

let write ~fs ~cache_root r =
  let layer_dir =
    cache_root / "layers" / r.os_key / r.layer_hash
  in
  if Sys.file_exists layer_dir then
    let dst = path ~cache_root ~os_key:r.os_key ~hash:r.layer_hash in
    match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec r with
    | Ok s -> (
        try
          Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / dst) s
        with exn ->
          Log.warn (fun m ->
              m "provenance write %s: %s" dst (Printexc.to_string exn)))
    | Error e ->
        Log.warn (fun m -> m "provenance encode %s: %s" dst e)

let try_decode ~fs ~path : t option =
  try
    let s = Eio.Path.load Eio.Path.(fs / path) in
    match Jsont_bytesrw.decode_string ~locs:true ~file:path codec s with
    | Ok r -> Some r
    | Error e ->
        Log.debug (fun m -> m "provenance bad %s: %s" path e);
        None
  with exn ->
    Log.debug (fun m ->
        m "provenance read %s: %s" path (Printexc.to_string exn));
    None

let load ~fs ~cache_root ~os_key ~hash =
  let p = path ~cache_root ~os_key ~hash in
  if Eio.Path.is_file Eio.Path.(fs / p) then try_decode ~fs ~path:p else None

let read_all ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~cache_root ~os_key =
  let layers_dir = cache_root / "layers" / os_key in
  match Eio.Path.read_dir Eio.Path.(fs / layers_dir) with
  | exception Eio.Exn.Io _ -> []
  | hashes ->
      List.filter_map
        (fun hash ->
          (* Skip files mixed in with the hash-named subdirs (e.g. the
             OINDEX.txt manifest) — every real layer dir is a flat hex
             string with no extension. *)
          if String.contains hash '.' then None
          else load ~fs ~cache_root ~os_key ~hash)
        hashes
