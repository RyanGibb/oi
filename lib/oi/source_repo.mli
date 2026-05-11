(** Opam repository clones — the low-level [git clone] / refresh primitives
    behind every [packages/] tree the solver consumes.

    Used via {!ensure_many} by every command that resolves [--with-repo URL]
    entries. *)

val ensure_many :
  ?reporter:Build_progress.reporter ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  data_dir:string ->
  ?refresh:bool ->
  Project.extra_repo list ->
  string list
(** [ensure_many ~fs ~data_dir extras] clones/updates each entry into
    [data_dir/repos/<name>], refreshing when [?refresh = true] or the clone is
    older than {!Cache.refresh_max_age}. Two entries with the same name collide
    by design (callers should deduplicate). Returns one [packages/] directory
    per entry in input order. *)
