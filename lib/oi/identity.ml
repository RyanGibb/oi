type t = { name : string; version : string }

let of_string s =
  match OpamPackage.of_string_opt s with
  | Some p ->
      {
        name = OpamPackage.Name.to_string (OpamPackage.name p);
        version = OpamPackage.Version.to_string (OpamPackage.version p);
      }
  | None -> { name = s; version = "" }

let to_string { name; version } =
  if version = "" then name else name ^ "." ^ version

let of_opam p =
  {
    name = OpamPackage.Name.to_string (OpamPackage.name p);
    version = OpamPackage.Version.to_string (OpamPackage.version p);
  }

type dep = { id : t; hash : string }

let dep_of_opam p ~hash = { id = of_opam p; hash }

type method_ = Source | Binary

let method_to_string = function Source -> "source" | Binary -> "binary"

(* -- Codecs -------------------------------------------------------------- *)

let codec =
  let open Jsont in
  Object.map ~kind:"pkg_id" (fun name version -> { name; version })
  |> Object.mem "name" string ~enc:(fun p -> p.name)
  |> Object.mem "version" string ~enc:(fun p -> p.version)
  |> Object.finish

let dep_codec =
  let open Jsont in
  Object.map ~kind:"dep" (fun name version hash ->
      { id = { name; version }; hash })
  |> Object.mem "name" string ~enc:(fun d -> d.id.name)
  |> Object.mem "version" string ~enc:(fun d -> d.id.version)
  |> Object.mem "hash" string ~enc:(fun d -> d.hash)
  |> Object.finish

let method_codec =
  Jsont.enum ~kind:"method" [ ("source", Source); ("binary", Binary) ]
