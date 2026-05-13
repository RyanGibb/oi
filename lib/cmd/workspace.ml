let ( / ) = Filename.concat

(* Placeholder ocaml-version used to seed Pipeline.conf before
   toolchain resolution overrides it via Pipeline.solver_inputs.
   Every consumer solve goes through resolve_toolchain (which returns
   the x-oi-default-toolchain entry when --toolchain isn't set) and
   the resulting toolchain.ocaml_version replaces this string in the
   conf the solver actually sees. The value here only matters in two
   narrow display paths: oi config's "ocaml: 5.4.1 (relocatable)" line,
   and as the conf seed inside Toolchain.resolve's own solve before
   it picks its compiler version. *)
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
