let ( / ) = Filename.concat

let current () =
  (* /proc/self/exe is the only fully-reliable source on Linux: it
     resolves [argv.(0)] / [PATH] / hardlinks / symlinks to the on-disk
     path of the executing inode. macOS doesn't have it, so we fall
     through to [Sys.executable_name], which the OCaml runtime
     populates via [_NSGetExecutablePath] and runs through [realpath].
     The very last fallback is [argv.(0)] (also via [Sys.executable_name]
     on platforms where the runtime can't do better). *)
  match Unix.readlink "/proc/self/exe" with
  | path -> path
  | exception Unix.Unix_error _ -> (
      let exe = Sys.executable_name in
      try Unix.realpath exe with Unix.Unix_error _ -> exe)

let is_writable path =
  if Sys.file_exists path then
    try
      Unix.access path [ Unix.W_OK ];
      true
    with Unix.Unix_error _ -> false
  else
    let dir = Filename.dirname path in
    try
      Unix.access dir [ Unix.W_OK ];
      true
    with Unix.Unix_error _ -> false

let home () =
  match Sys.getenv_opt "HOME" with
  | Some h when h <> "" -> h
  | _ -> (
      try (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir with Not_found -> "/")

let local_bin () = home () / ".local" / "bin"

type target =
  | In_place of string
  | Fallback of { current : string; install_dir : string }

let resolve_target ?bin_dir () =
  let exe = current () in
  if is_writable exe then In_place exe
  else
    let install_dir = Stdlib.Option.value bin_dir ~default:(local_bin ()) in
    Fallback { current = exe; install_dir }
