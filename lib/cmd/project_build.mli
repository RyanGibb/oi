(** [oi build] project mode: drive the cwd's local [*.opam] build.

    Sync the project's dependency closure into [_oi/prefix/], then run
    [dune build] with that prefix on PATH. Dune handles inter-package
    ordering of sibling [*.opam] files itself. Non-dune projects error
    out for now; a future iteration may resolve opam [build:] commands
    per package. *)

val run :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  sys:D10.Sysops.t ->
  platform:Osrel.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  registry:string ->
  ?refresh:bool ->
  ?with_repos:string list ->
  ?with_deps:string list ->
  ?jobs:int ->
  ?toolchain:string ->
  ?envrc_mode:Sync.envrc_mode ->
  ?dry_run:bool ->
  ?deps_only:bool ->
  cwd:string ->
  unit ->
  int
(** [run ~cwd ()] sync's the project then runs [dune build]. Returns the
    build's exit code (0 on success). Errors out via {!Oi.Error.E} when
    [cwd] has no [*.opam] files or no [dune-project]. *)

val depexts :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  platform:Osrel.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  ?refresh:bool ->
  ?with_repos:string list ->
  ?with_deps:string list ->
  ?toolchain:string ->
  cwd:string ->
  unit ->
  int
(** Solve the project's dep closure (no build), then print the union of
    [depexts:] declared by the solved packages, one per line. Backs
    [oi build --depext] in project mode. *)
