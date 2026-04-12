[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "sysops"

module Log = (val Logs.src_log log_src : Logs.LOG)

type tools = { tar : string }
type pm = [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t

type t = {
  proc_mgr : pm;
  fs : Eio.Fs.dir_ty Eio.Path.t; [@warning "-69"]
  tools : tools;
}

(* -- Low-level helpers --------------------------------------------------- *)

let native p = Eio.Path.native_exn p

let run_quiet t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  Eio.Switch.run @@ fun sw ->
  let buf = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink buf in
  let child = Eio.Process.spawn ~sw t.proc_mgr ~stdout:sink ~stderr:sink cmd in
  match Eio.Process.await child with
  | `Exited 0 ->
      let output = String.trim (Buffer.contents buf) in
      if output <> "" then Log.debug (fun m -> m "%s" output)
  | `Exited n ->
      let output = String.trim (Buffer.contents buf) in
      if output <> "" then Log.debug (fun m -> m "%s" output);
      Fmt.failwith "command exited %d: %s" n (String.concat " " cmd)
  | `Signaled n ->
      Fmt.failwith "command killed by signal %d: %s" n (String.concat " " cmd)

let run_capture t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  let out =
    String.trim (Eio.Process.parse_out t.proc_mgr Eio.Buf_read.take_all cmd)
  in
  Log.debug (fun m -> m "%s" out);
  out

let has_cmd t name =
  try
    ignore (run_capture t [ "which"; name ]);
    true
  with _ -> false

(* -- Initialisation ------------------------------------------------------ *)

let resolve_tools t =
  let tar = if has_cmd t "gtar" then "gtar" else "tar" in
  { tar }

let create ~proc_mgr ~fs =
  let t_partial = { proc_mgr :> pm; fs; tools = { tar = "tar" } } in
  let tools = resolve_tools t_partial in
  { t_partial with tools }

(* -- File queries -------------------------------------------------------- *)

let file_exists path =
  try
    ignore (Eio.Path.stat ~follow:true path);
    true
  with Eio.Exn.Io _ -> false

(* -- File copying -------------------------------------------------------- *)

let copy_tree t ~src ~dst =
  let src_s = native src and dst_s = native dst in
  try run_quiet t [ "cp"; "-ac"; src_s; dst_s ]
  with Eio.Exn.Io _ ->
    Eio.Path.rmtree ~missing_ok:true dst;
    run_quiet t [ "cp"; "-a"; src_s; dst_s ]

let link_tree t ~src ~dst =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let src_s = native src and dst_s = native dst in
  try run_quiet t [ "cp"; "-Rflv"; src_s ^ "/."; dst_s ^ "/" ]
  with Failure _ -> ()

(* -- Low-level command execution ----------------------------------------- *)

module Cmd = struct
  let run t cmd = run_quiet t cmd
  let run_out t cmd = run_capture t cmd
end

(* -- Archive operations -------------------------------------------------- *)

module Tar = struct
  let extract t ~archive ~dst ?(strip = 0) () =
    let cmd =
      [ t.tools.tar; "xf"; native archive; "-C"; native dst ]
      @ if strip > 0 then [ Fmt.str "--strip-components=%d" strip ] else []
    in
    run_quiet t cmd
end

(* -- Git operations ------------------------------------------------------ *)

module Git = struct
  let head_short t ~dir =
    run_capture t [ "git"; "-C"; native dir; "rev-parse"; "--short"; "HEAD" ]
end
