(** Opam repository clones — the low-level [git clone] / refresh
    primitives behind every [packages/] tree the solver consumes.

    Used directly by [Source_reporepo.ensure_clone] (the reporepo
    itself is a git clone) and via {!ensure_many} by every command
    that resolves [--with-repo URL] entries. *)

val dir : data_dir:string -> string -> string
(** [dir ~data_dir name] is the local clone path for a repo named [name]. *)

val ensure_many :
  ?reporter:Build_progress.reporter ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  data_dir:string ->
  ?refresh:bool ->
  Project.extra_repo list ->
  string list
(** [ensure_many ~fs ~data_dir extras] clones/updates each entry using
    the same age/force semantics as {!ensure}. Each entry is cloned
    into [data_dir/repos/<name>] — two entries with the same name
    collide by design (callers should deduplicate). Returns one
    [packages/] directory per entry in input order. *)

val ensure :
  ?reporter:Build_progress.reporter ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  refresh:bool ->
  label:string ->
  url:string ->
  dir:string ->
  unit ->
  unit
(** [ensure ~url ~dir ~refresh ()] is the low-level clone/update
    primitive. Clones [url] into [dir] if empty, otherwise refreshes
    when [refresh] is true or the clone is older than
    {!Cache.refresh_max_age}. *)
