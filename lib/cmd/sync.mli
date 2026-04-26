(** [oi sync]: install a project's dependencies into [_oi/prefix/].

    Solves [*.opam] in the cwd, builds/fetches every layer, hardlink-
    assembles them into [_oi/prefix/], probes for dev tools and installs
    them into [_oi/tools/], and writes [.envrc] for direnv.

    The two helpers exposed below are reused by {!Exec}: [oi exec]
    auto-syncs when the prefix is older than any [*.opam], and shares the
    same staleness check + sync routine. *)

val needs_sync : cwd:string -> prefix:string -> bool
(** [needs_sync ~cwd ~prefix] is [true] when [prefix] is missing or any
    [*.opam] in [cwd] has been modified more recently than [prefix]. *)

val do_sync :
  ?quiet:bool ->
  ?refresh:bool ->
  ?with_repos:string list ->
  ?with_deps:string list ->
  ?jobs:int ->
  ?toolchain:string ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  sys:D10.Sysops.t ->
  platform:Osrel.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  registry:string ->
  cwd:string ->
  unit ->
  string
(** Run a full sync in [cwd] and return the path to the assembled
    [_oi/prefix/]. [quiet] (default [false]) routes narration to
    [Logs.info] instead of stdout. *)

val cmd : unit Cmdliner.Cmd.t
