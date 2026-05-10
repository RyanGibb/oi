(** Per-overlay-handle failure report in markdown.

    Joins {!Manifest} (one per [os_key]) with the {!Audit} event slice for the
    same [os_key] to surface, for each overlay handle that built under
    [@<handle>], the packages that failed and how to reproduce them.

    The generated files are written to [<output_dir>/handles/<handle>.md] — a
    stable, well-known location an LLM agent can fetch from a registry to pick
    up bug reports without having to parse the JSON manifests. *)

type slice = {
  os_key : string;
  manifest : Manifest.t;
  events : Audit.event list;
}
(** A registry-side data slice for one OS key. *)

val handles : slice list -> string list
(** Distinct overlay handles seen across every event's [context.overlay] in
    [slices]. Sorted alphabetically. *)

val write_all :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  output_dir:string ->
  generated_at:float ->
  string list
(** Convenience: read every slice under [output_dir], compute the union of
    handles, and write [<output_dir>/handles/<handle>.md] for each handle that
    has at least one failure. Returns the list of handles for which a file was
    written. *)
