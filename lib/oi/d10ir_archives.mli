(** Publish helpers for [<cache>/d10ir/archives/<sha>.tar.zst] consolidated
    source archives.

    The destination layout matches {!D10ir.Registry.pull}'s expectation:
    [<base_url>/d10ir-archives/<sha>.tar.zst]. Hardlink-first; falls back to a
    streamed copy when the dst is on a different filesystem. *)

val local_dir : cache:Cache.t -> string
(** [<cache>/d10ir/archives]. Where {!Archive_builder} writes baked archives and
    where {!Source.Pin} / pull paths look them up. *)

val dst_dir : output:string -> string
(** [<output>/d10ir-archives]. The published location. *)

val list : cache:Cache.t -> (string * int) list
(** [list ~cache] enumerates every [<sha>.tar.zst] under {!local_dir} as
    [(sha, size_bytes)] sorted by sha. The size is the on-disk byte count of the
    archive itself; missing files (e.g. a stale entry without a [stat]) report
    [0]. *)

val publish_all : cache:Cache.t -> output:string -> int
(** [publish_all ~cache ~output] hardlinks (or copies) every
    [<cache>/d10ir/archives/<sha>.tar.zst] into [<output>/d10ir-archives/].
    Returns the count of newly-published archives; idempotent on repeat
    invocations. *)

val publish_shas : cache:Cache.t -> output:string -> string list -> int
(** [publish_shas ~cache ~output shas] publishes only the named shas (filenames
    are [<sha>.tar.zst]). Used by handle-scoped commands that bake a focused
    subset and ship just those archives. *)
