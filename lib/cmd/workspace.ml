let ( / ) = Filename.concat

let ocaml_version = "5.4.1"
let app_name = "oi"

let path_exists fs path =
  try
    ignore (Eio.Path.stat ~follow:true Eio.Path.(fs / path));
    true
  with Eio.Exn.Io _ -> false

let resolved_cwd fs =
  let s = Unix.realpath "." in
  (s, Eio.Path.(fs / s))

let tools_dir_for ~cwd =
  let tools = cwd / "_oi" / "tools" in
  match Sys.is_directory (tools / "bin") with
  | true -> Some tools
  | false | (exception Sys_error _) -> None
