[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

type t = { os : string; distro : string; os_version : string; arch : string }

(* CR: avsm. Made this truncate the minor version on macOS since its the major
   that affects everything, I think. *)
let of_platform (p : Osrel.t) =
  let os, distro =
    match p.os.kind with
    | `Linux _ -> ("linux", Osrel.OS.kind_to_string p.os.kind)
    | `MacOS _ -> ("macos", "macos")
    | _ ->
        let s = Osrel.OS.kind_to_string p.os.kind in
        (s, s)
  in
  let os_version =
    match p.os.kind with
    | `MacOS _ -> (
        match String.split_on_char '.' p.os.version with
        | major :: _ -> major
        | [] -> p.os.version)
    | _ -> p.os.version
  in
  let arch = Osrel.Arch.to_string p.arch in
  { os; distro; os_version; arch }

let to_string k = Fmt.str "%s~%s~%s" k.distro k.os_version k.arch

let of_string s =
  match String.split_on_char '~' s with
  | [ distro; os_version; arch ] ->
      let os =
        match distro with
        | "macos" | "homebrew" | "macports" -> "macos"
        | _ -> "linux"
      in
      { os; distro; os_version; arch }
  | _ -> { os = "unknown"; distro = s; os_version = ""; arch = "" }

let pp fmt t = Fmt.string fmt (to_string t)
