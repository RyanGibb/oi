(** Dependency-graph rendering as an indented Unicode tree.

    The graph is typically a DAG (shared toolchain libraries are
    reached from many roots), so the renderer walks each root and
    prunes any subtree it has already expanded under an earlier
    root. First occurrence wins — the deepest expansion of a layer
    lives where it is first reached.

    Two label shapes per node, distinguishing first-occurrence rows
    from re-references:

    - First occurrence:  rendered via [label_first] in the default
      style.
    - Back-reference:    rendered via [label_ref], wrapped in
      {!Style.dim_string}, prefixed with a small "↰" arrow so the
      eye reads "see this layer expanded above". *)

val render :
  label_first:('node -> string) ->
  label_ref:('node -> string) ->
  key_of:('node -> string) ->
  children:('node -> 'node list) ->
  'node list ->
  unit
(** [render ~label_first ~label_ref ~key_of ~children roots] prints
    the dependency tree rooted at [roots] to stdout.

    [key_of n] is the dedup key — typically the layer hash. Two
    nodes with the same key are treated as the same logical layer;
    the second visit renders as a dim back-reference with no
    children.

    [children n] is the list of direct dependencies of [n] within
    the plan (the renderer doesn't filter — return only nodes you
    want walked).

    Empty [roots] prints "(no roots)". *)
