(** Layer index (SQLite).

    Provides fast lookup of cached layers by package name, binary name, or
    dependency chain. The index is a SQLite database stored at
    [<cache>/layers/index.db] and is rebuilt on demand by scanning the layer
    directories.

    {2 Schema}

    {v
    layers:          hash, os_key, arch, os, distro, os_version,
                     package_name, package_ver, exit_status, created,
                     overlay_handle, overlay_version
    layer_deps:      layer_hash, dep_name, dep_version, dep_hash
    layer_files:     layer_hash, path
    layer_binaries:  layer_hash, binary_name   (files under bin/)
    v}

    [overlay_handle] / [overlay_version] identify the reporepo overlay that
    contributed the opam file used to build a layer. Both are NULL for layers
    built before tagging was introduced or for packages that came from a
    pin-depends tree.

    The [layer_binaries] table enables [oi run <binary>] to quickly find which
    package provides a given binary without scanning layer trees. *)

(** {1 Database lifecycle} *)

type db
(** An open SQLite database handle. *)

val open_ : path:string -> db
(** [open_ ~path] opens (or creates) the index database at [path]. Tables are
    created if they don't already exist. *)

val close : db -> unit
(** [close db] closes the database handle. *)

(** {1 Indexing} *)

val rebuild :
  Config.t ->
  ?overlay_for:(hash:string -> Overlay.t option) ->
  ?include_files:bool ->
  db ->
  unit
(** [rebuild c ?overlay_for ?include_files db] scans all layers under
    [<root>/layers/<os_key>/] and populates the index tables. Existing data
    for [c.os_key] is replaced atomically within a transaction. Each layer's
    [layer.json] is parsed for metadata; its [fs/] tree is scanned for
    binary names ([fs/bin/], [fs/sbin/]) and findlib package metadata
    (every [fs/lib/<dir>/META] is parsed and its declared subpackages
    recorded in [layer_meta]).

    [include_files] (default [false]) controls whether the full file path
    list lands in [layer_files]. The bin-index registry shape leaves this
    off — the table is the bulk of [index.db]'s on-disk size and is only
    needed by the layer-cache shape (where [oi build] verifies tarball
    contents). [oi search] / [find_binary] / [find_meta] don't consult
    [layer_files] and work with [include_files = false].

    [overlay_for] supplies the per-layer overlay attribution (defaulted to
    [fun ~hash:_ -> None]). The [oi] cache wires this to read from the layer's
    [provenance.json] sidecar; tools that don't care about overlay routing can
    leave it at the default. *)

val record_tarball :
  db ->
  hash:string ->
  sha256:string ->
  size:int64 ->
  unit
(** [record_tarball db ~hash ~sha256 ~size] populates the
    [tarball_sha256] / [tarball_size] columns on the [layers] row.
    Called per-layer after the [.tar.zst] has been written to the
    export dir. A row whose tarball columns are NULL belongs to a
    bin-index registry — no layer restore is possible from there. *)

(** {1 Queries} *)

val find_layer :
  db -> name:string -> version:string -> os_key:string -> (string * int) option
(** [find_layer db ~name ~version ~os_key] returns [(hash, exit_status)] for the
    layer matching the given package, or [None]. *)

val find_binary :
  db ->
  binary:string ->
  os_key:string ->
  (string * string * string * Overlay.t option) list
(** [find_binary db ~binary ~os_key] returns all layers that provide
    [bin/<binary>] or [sbin/<binary>], as
    [(package_name, package_version, layer_hash, overlay)], sorted by opam
    version descending (latest version first). [overlay] is set when the layer
    was tagged with a reporepo overlay; [None] otherwise (pin-depends, local
    trees). *)

val search_binary :
  db ->
  pattern:string ->
  os_key:string ->
  (string * string * string * string * Overlay.t option) list
(** [search_binary db ~pattern ~os_key] searches for binaries matching
    [pattern], returning
    [(binary_name, package_name, package_version, layer_hash, overlay)]. The
    pattern is matched exactly by default; use [*] as a wildcard (mapped to SQL
    [LIKE %]). Results are sorted by binary name then opam version descending.
*)

val search_package :
  db ->
  pattern:string ->
  os_key:string ->
  (string * string * string * Overlay.t option) list
(** [search_package db ~pattern ~os_key] searches for built packages whose name
    matches [pattern], returning
    [(package_name, package_version, layer_hash, overlay)]. Pattern matching and
    the [overlay] field have the same semantics as {!search_binary}. Results are
    sorted by package name then opam version descending. *)

val find_meta :
  db ->
  findlib_pkg:string ->
  os_key:string ->
  (string * string * string * Overlay.t option) list
(** [find_meta db ~findlib_pkg ~os_key] returns layers whose findlib
    metadata declares [findlib_pkg] (e.g. ["cohttp.async"]), as
    [(package_name, package_version, layer_hash, overlay)] sorted by
    opam version descending. Use [*] as a wildcard for substring search.
    Reads the [layer_meta] table populated by {!rebuild}. *)

val tarball_info :
  db ->
  hash:string ->
  (string * int64) option
(** [tarball_info db ~hash] returns [(sha256, size)] when the layer's
    [.tar.zst] is published in this registry, or [None] for a bin-index
    registry. Replaces the old [OINDEX.txt] sidecar lookup. *)

val all_tarballs :
  db ->
  os_key:string ->
  (string * string * int64) list
(** [all_tarballs db ~os_key] returns [(hash, sha256, size)] for every
    layer in [os_key] that has a published tarball. Empty list on a
    bin-index registry. The cmdliner layer's [fetch_remote_index]
    consumes this to populate {!Layer.remote_index}. *)

val deps : db -> hash:string -> (string * string * string) list
(** [deps db ~hash] returns the direct dependencies of a layer as
    [(dep_name, dep_version, dep_hash)]. *)

val files : db -> hash:string -> string list
(** [files db ~hash] returns all file paths stored in the layer. *)

val all_layers : db -> os_key:string -> (string * string * string * int) list
(** [all_layers db ~os_key] returns all layers for a platform as
    [(hash, package_name, package_version, exit_status)]. *)

val all_binaries : db -> os_key:string -> (string * string * string) list
(** [all_binaries db ~os_key] returns all indexed binaries as
    [(binary_name, package_name, package_version)]. *)

val stats : db -> os_key:string -> int * int * int
(** [stats db ~os_key] returns [(num_layers, num_binaries, num_files)] for the
    given platform. *)

(** {1 Invalidation} *)

val dependents : db -> hashes:string list -> os_key:string -> string list
(** [dependents db ~hashes ~os_key] returns the hashes of layers in [os_key]
    that directly depend on any layer in [hashes]. Apply iteratively to compute
    the transitive set of layers poisoned by an invalidated package. Returns
    [[]] when [hashes] is empty. *)

val delete_layers : db -> hashes:string list -> unit
(** [delete_layers db ~hashes] removes the layers and their associated rows from
    [layers], [layer_deps], [layer_binaries], and [layer_files]. The on-disk
    layer directories are not touched — the caller must
    [rmtree <root>/layers/<os_key>/<hash>/] for each entry. No-op when [hashes]
    is empty. *)

(** {1 Remote merge} *)

val merge_remote : db -> remote_path:string -> unit
(** [merge_remote db ~remote_path] imports layers from a remote index database
    into [db]. Only layers whose hash does not already exist in [db] are
    inserted (local entries take precedence). Associated deps, binaries, and
    files for new layers are also imported. *)
