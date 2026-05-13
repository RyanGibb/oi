(** Copy opam install trees into a flat [dist/] layout.

    Resolves symlinks (the dune install tree hands out links pointing back to
    [.exe] artefacts), normalises perms, and writes via
    [<dst>.<pid>.tmp + rename] so an in-place install never trips [ETXTBSY] on a
    running executable. *)

val copy_resolved : src:string -> dst:string -> unit
(** [copy_resolved ~src ~dst] copies [src] (dereferencing symlinks) to [dst]
    atomically, preserving the executable bit (writes [0o755] if any [u/g/o-x]
    was set, otherwise [0o644]). *)

val list_files : string -> (string * string) list
(** [list_files dir] returns [(name, full-path)] for every regular file (or
    symlink to a regular file) directly under [dir], sorted by basename. Missing
    [dir] yields []. *)

val collect_install :
  root:string -> dst:string -> (string * string * string) list
(** [collect_install ~root ~dst] mirrors the opam install layout under [root]
    into [dst]:
    - [bin/] → [<dst>/bin/<name>]
    - [sbin/] → [<dst>/sbin/<name>]
    - [share/] → [<dst>/share/<rel>] (recursive)

    Returns [(subdir, relative-name, written-path)] for every regular file
    written, in [bin, sbin, share] order. Missing source subdirs are silently
    skipped. *)
