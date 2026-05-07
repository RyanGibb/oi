val cmd : unit Cmdliner.Cmd.t

val opam_set_x_d10_archive :
  path:string -> sha:string -> [ `Added | `Already ]
(** Set or replace the [x-d10-archive] extension in an opam file in
    place. Returns [`Already] when the value matches the existing one
    (no write happens), [`Added] otherwise. Used by [oi repo bump] —
    the only place that should populate this field. *)

