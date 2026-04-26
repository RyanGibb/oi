(** Run an OCaml script via [oi run path/to/script.ml].

    Parses [[\@\@\@opam pkg1 pkg2 …]] on the first line, generates a tiny
    dune project under the cache, solves and builds the deps, compiles the
    script, caches the resulting binary keyed on the script's content hash,
    and execs it with the assembled prefix's env.

    The [oi run] command in {!Run} dispatches here when [TARGET] resolves
    to a file ending in [.ml]. *)

val run :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  os_key:string ->
  prefix:string ->
  conf:Oi.Solver.Ctx.conf ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  ?remote:D10.Layer.remote ->
  string ->
  Oi.Project.Script.dep list ->
  string list ->
  'a
(** [run … script_path cli_deps args] never returns: it [exit]s with the
    script's status code. *)
