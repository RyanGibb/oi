type env = {
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  platform : Osrel.t;
  os_key : string;
  cache : Oi.Cache.t;
}

let pp_one_exn fmt = function
  | Oi.Error.E e -> Oi.Error.pp fmt e
  | Failure msg -> Fmt.pf fmt "%a %s" Oi.Style.error_string "error:" msg
  | e ->
      Fmt.pf fmt "%a %s" Oi.Style.error_string "error:" (Printexc.to_string e)

let rec is_interrupt = function
  | Oi.Signals.Interrupted | Sys.Break -> true
  | Eio.Cancel.Cancelled e -> is_interrupt e
  | Eio.Exn.Io _ -> false
  | _ -> false

let with_error_handling f =
  try f () with
  | exn when is_interrupt exn ->
      Fmt.epr "Interrupted.@.";
      exit 130
  | Eio.Exn.Multiple exns when List.exists (fun (e, _) -> is_interrupt e) exns
    ->
      Fmt.epr "Interrupted.@.";
      exit 130
  | (Oi.Error.E _ | Failure _) as exn ->
      Fmt.epr "%a@." pp_one_exn exn;
      exit 1
  | Eio.Exn.Multiple exns ->
      List.iter (fun (e, _bt) -> Fmt.epr "%a@." pp_one_exn e) exns;
      exit 1

(* Forced to the POSIX backend rather than [Eio_main.run] so builds under
   Linux don't pick up [eio_linux] / io_uring — we want the same syscall
   surface everywhere, and io_uring interacts poorly with some of the
   subprocess / signal paths we rely on. *)
let with_eio_root f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Oi.Signals.install ~sw;
  f env

let run f = with_error_handling @@ fun () -> with_eio_root f

let bootstrap (env : Eio_unix.Stdenv.base) cache_dir =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let stdout = Eio.Stdenv.stdout env in
  let stderr = Eio.Stdenv.stderr env in
  let sys =
    D10.Sysops.create ~stdout ~stderr ~proc_mgr ~fs ~net ~clock ()
  in
  let platform = Osrel.detect ~proc_mgr ~fs in
  let os_key = D10.Os_key.(to_string (of_platform platform)) in
  let cache = Oi.Cache.create ~root:cache_dir fs in
  { proc_mgr; fs; clock; sys; platform; os_key; cache }
