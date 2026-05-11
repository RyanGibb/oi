val cmd : unit Cmdliner.Cmd.t

val opam_set_x_d10_archive : path:string -> sha:string -> [ `Added | `Already ]
(** Set or replace the [x-d10-archive] extension in an opam file in place.
    Returns [`Already] when the value matches the existing one (no write
    happens), [`Added] otherwise. Used by [oi repo bump] — the only place that
    should populate this field. *)

val opam_strip_patches_extras :
  fs:Eio.Fs.dir_ty Eio.Path.t -> opam_path:string -> [ `Stripped | `Already ]
(** Strip [patches:] and [extra-files:] from an opam file in place, and remove
    the sibling [files/] subdirectory holding those blobs. Called after a
    successful bake / restore once [x-d10-archive] is set: the consolidated
    d10ir archive contains the patched + extras-included source tree, so the
    reporepo's copy is dead weight. The [files/] dir typically dominates a baked
    overlay's on-disk size. Returns [`Already] when there was nothing to remove,
    [`Stripped] otherwise. *)
