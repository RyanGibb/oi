(** Publish helpers for [<cache>/d10ir/archives/<sha>.tar.zst] consolidated
    source archives.

    The destination layout matches {!D10ir.Registry.pull}'s expectation:
    [<base_url>/d10ir-archives/<sha>.tar.zst]. Hardlink-first; falls back to a
    streamed copy when the dst is on a different filesystem. *)

val local_dir : cache:Cache.t -> string
(** [local_dir] . Where {!Archive_builder} writes baked archives and where
    {!Source.Pin} / pull paths look them up. *)

val dst_dir : output:string -> string
(** [dst_dir] . The published location. *)

val list : cache:Cache.t -> (string * int) list
(** [list ~cache] enumerates every [<sha>.tar.zst] under {!local_dir} as
    [(sha, size_bytes)] sorted by sha. The size is the on-disk byte count of the
    archive itself; missing files (e.g. a stale entry without a [stat]) report
    [0]. *)

type publish_counts = { linked : int; present : int; missing : int }
(** [linked]: archives newly hardlinked/copied into the destination this
    invocation. [present]: archives already there from a prior run. [missing]:
    shas the caller named (e.g. via a reporepo opam's [x-d10-archive]) for which
    no local cache entry exists — silently skipped during publish but surfaced
    here so callers can warn. [linked + present] is the number of archives [DIR]
    now contains. *)

val publish_all : cache:Cache.t -> output:string -> publish_counts
(** [publish_all ~cache ~output] hardlinks (or copies) every
    [<cache>/d10ir/archives/<sha>.tar.zst] into [<output>/d10ir-archives/].
    Idempotent on repeat invocations. *)

val publish_shas :
  cache:Cache.t -> output:string -> string list -> publish_counts
(** [publish_shas ~cache ~output shas] publishes only the named shas (filenames
    are [<sha>.tar.zst]). Used by handle-scoped commands that publish the
    archives the reporepo overlays reference, leaving stale entries from older
    bakes out of [DIR]. *)
