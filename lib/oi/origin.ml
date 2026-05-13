type kind = Reporepo | Pin | Local
type t = { kind : kind; overlay : D10.Overlay.t option; path_in_repo : string }

(* Walk three levels up from [pkgs_dir] (which is the [packages/] directory
   itself) to disambiguate the reporepo, pin-set, and local layouts. The
   parent of [packages] is the layout root; its parent is the type marker. *)
let of_packages_dir ~pkgs_dir ~name ~full =
  let base = Filename.basename (Filename.dirname pkgs_dir) in
  let one_up = Filename.dirname (Filename.dirname pkgs_dir) in
  let parent = Filename.basename one_up in
  let grandparent = Filename.basename (Filename.dirname one_up) in
  let leaf = Fmt.str "%s/%s/opam" name full in
  let path_in_repo prefix = Fmt.str "%s/%s" prefix leaf in
  match (parent, base) with
  | "v2", "reporepo" ->
      {
        kind = Reporepo;
        overlay = Some { handle = "reporepo"; version = "" };
        path_in_repo = path_in_repo "v2/reporepo/packages";
      }
  | "v2", handle ->
      {
        kind = Reporepo;
        overlay = Some { handle; version = "" };
        path_in_repo = Fmt.kstr path_in_repo "v2/%s/packages" handle;
      }
  | "sets", _ when grandparent = "pins" ->
      { kind = Pin; overlay = None; path_in_repo = path_in_repo "packages" }
  | _ ->
      { kind = Local; overlay = None; path_in_repo = path_in_repo "packages" }

let pp_kind ppf = function
  | Reporepo -> Fmt.string ppf "reporepo"
  | Pin -> Fmt.string ppf "pin"
  | Local -> Fmt.string ppf "local"

let kind_codec : kind Jsont.t =
  Jsont.enum ~kind:"origin_kind"
    [ ("reporepo", Reporepo); ("pin", Pin); ("local", Local) ]

let codec =
  let open Jsont in
  Object.map ~kind:"origin" (fun kind overlay path_in_repo ->
      { kind; overlay; path_in_repo })
  |> Object.mem "kind" kind_codec ~enc:(fun o -> o.kind)
  |> Object.opt_mem "overlay" D10.Overlay.codec ~enc:(fun o -> o.overlay)
  |> Object.mem "path_in_repo" string ~enc:(fun o -> o.path_in_repo)
  |> Object.finish

let pp ppf o = Fmt.pf ppf "@[<h>%a@%s@]" pp_kind o.kind o.path_in_repo
