(** Per-command harness: Eio root, signal handling, error catching, and the
    Eio-capability bundle every command body operates on.

    Idiom every command uses:
    {[
      let run () data_dir cache_dir … =
        Harness.run @@ fun env ->
        let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache } =
          Harness.bootstrap env cache_dir
        in
        … command-specific work …
    ]} *)

type env = {
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  platform : Osrel.t;
  os_key : string;
  cache : Oi.Cache.t;
}
(** What every command body needs after Eio + cache setup. Returned by
    {!bootstrap}. *)

val run : (Eio_unix.Stdenv.base -> 'a) -> 'a
(** [run f] sets up the Eio root, installs the SIGINT/SIGTERM handler under a
    fresh switch, and calls [f env]. Any exception that escapes [f] is
    caught: signal-style exits print "Interrupted." and exit 130; structured
    {!Oi.Error.E} or [Failure] exceptions print a single coloured line and
    exit 1; everything else prints a generic error and exits 1. *)

val bootstrap : Eio_unix.Stdenv.base -> string -> env
(** [bootstrap env cache_dir] reads the proc-mgr / fs / clock / stdio
    capabilities off [env], creates the [Sysops], detects the platform, and
    builds the cache rooted at [cache_dir]. *)
