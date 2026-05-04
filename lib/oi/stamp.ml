let log_src = Logs.Src.create "oi.stamp"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* Bump policy lives in [stamp.mli]. *)
let cache_schema = 1
let data_schema = 1
let toolchains_schema = 1
let solver_cache_schema = "v8"
let json_schema_version = "1.0"
let stamp_filename = ".oi-stamp"

(* On-disk format is a tiny [key value] text file:

     schema 1
     written_at 1714838400

   Plain text rather than JSON so a [grep] in [<cache>] doesn't trip
   over a JSON object. Unknown lines are ignored, so future fields can
   be added without invalidating existing stamps. *)

let parse_schema content =
  String.split_on_char '\n' content
  |> List.find_map (fun line ->
      match String.trim line |> String.split_on_char ' ' with
      | "schema" :: v :: _ -> int_of_string_opt v
      | _ -> None)

let render ~schema =
  Fmt.str "schema %d\nwritten_at %.0f\n" schema (Unix.gettimeofday ())

let read ~fs ~root =
  let path = Filename.concat root stamp_filename in
  if not (Sys.file_exists path) then None
  else try parse_schema (Eio.Path.load Eio.Path.(fs / path)) with _ -> None

let write ~fs ~root ~schema =
  let path = Filename.concat root stamp_filename in
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / root);
    Eio.Path.save ~create:(`Or_truncate 0o644)
      Eio.Path.(fs / path)
      (render ~schema)
  with exn ->
    Log.warn (fun m ->
        m "stamp write failed at %s: %s" path (Printexc.to_string exn))

(* "Items have content" rather than "root is non-empty" because the data
   store shares its root with the user-authored reporepo — a populated
   reporepo would otherwise look "unstamped" and trip a sweep. *)
let any_present (items : Cache.item list) =
  List.exists
    (fun (item : Cache.item) ->
      try Sys.file_exists (Eio.Path.native_exn item.path) with _ -> false)
    items

let check ~fs ~root ~current ~items =
  match read ~fs ~root with
  | Some n when n >= current -> `Up_to_date
  | Some n -> `Stale n
  | None when any_present items -> `Unstamped
  | None -> `Fresh

let sweep ~fs ~label ~root ~current ~previous items =
  let detail =
    match previous with
    | `Stale n -> Fmt.str "%d \xe2\x86\x92 %d" n current
    | `Unstamped -> Fmt.str "unstamped \xe2\x86\x92 %d" current
  in
  Say.step "Upgrading oi %s schema (%s); clearing stale data" label detail;
  Cache.purge_items items;
  write ~fs ~root ~schema:current

let ensure ~fs ~(cache : Cache.t) ~data_dir =
  let go ~label ~root ~current ~items =
    match check ~fs ~root ~current ~items with
    | `Up_to_date -> ()
    | `Fresh -> write ~fs ~root ~schema:current
    | (`Stale _ | `Unstamped) as previous ->
        sweep ~fs ~label ~root ~current ~previous items
  in
  go ~label:"cache" ~root:(Cache.root_s cache) ~current:cache_schema
    ~items:(Cache.cache_items cache);
  go ~label:"data" ~root:data_dir ~current:data_schema
    ~items:(Cache.data_items cache ~data_dir);
  go ~label:"toolchain" ~root:(Cache.toolchains_root ())
    ~current:toolchains_schema
    ~items:(Cache.toolchain_items cache)
