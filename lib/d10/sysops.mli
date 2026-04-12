(** OS-abstracted system operations.

    Wraps external tools (tar, git) with OS-specific tool selection. Tool paths
    are resolved once at {!create} time: for example, [gtar] is preferred over
    [tar] on macOS.

    All local filesystem paths are {!Eio.Path.t} values. Source fetching,
    checksum verification, and downloads are handled by opam's repository
    libraries (see {!Oi.Fetch}). *)

(** {1 Initialisation} *)

type t
(** System operations context with pre-resolved tool paths. *)

val create : proc_mgr:_ Eio.Process.mgr -> fs:Eio.Fs.dir_ty Eio.Path.t -> t
(** [create ~proc_mgr ~fs] detects tool paths (tar variant) by probing the
    system via [which]. Call once at startup. *)

(** {1 File queries} *)

val file_exists : _ Eio.Path.t -> bool
(** [file_exists path] is [true] if [path] exists (follows symlinks). *)

(** {1 File copying} *)

val copy_tree : t -> src:_ Eio.Path.t -> dst:_ Eio.Path.t -> unit
(** [copy_tree t ~src ~dst] copies a directory tree. Attempts CoW clone
    ([cp -ac]) first (zero-copy on APFS), falling back to [cp -a]. *)

val link_tree : t -> src:_ Eio.Path.t -> dst:_ Eio.Path.t -> unit
(** [link_tree t ~src ~dst] hardlinks all files from [src] into [dst]
    recursively via [cp -Rfl]. Tolerates "identical file" errors. *)

(** {1 Archive operations} *)

module Tar : sig
  val extract :
    t -> archive:_ Eio.Path.t -> dst:_ Eio.Path.t -> ?strip:int -> unit -> unit
  (** [extract t ~archive ~dst ?strip ()] extracts [archive] into [dst]. Uses
      [gtar] on macOS if available. *)
end

(** {1 Low-level command execution} *)

module Cmd : sig
  val run : t -> string list -> unit
  (** [run t args] executes [args] as a subprocess, raising [Failure] on
      non-zero exit. *)

  val run_out : t -> string list -> string
  (** [run_out t args] executes [args] and returns its trimmed stdout. *)
end

(** {1 Git operations} *)

module Git : sig
  val head_short : t -> dir:_ Eio.Path.t -> string
  (** [head_short t ~dir] returns the short abbreviated hash of HEAD. *)
end
