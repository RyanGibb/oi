(** Convert an oi {!Plan.t} into a {!D10ir.Plan.t}.

    For each {!Plan.package_plan} marked [Source] (i.e. needs to be
    built), the emitter:
    - Calls {!Archive_builder.build} to produce a tarball with the
      pre-resolved source tree.
    - Composes a shell script from [build_commands ++ install_commands],
      shell-escaping each argv and joining with newlines under
      [set -e].
    - Emits a {!D10ir.Plan.node} carrying the layer hash, dep layer
      hashes, archive ref, script, env, prefix, and overlay metadata.

    [Binary] package_plans are skipped — they're already in the d10
    store; downstream nodes reference them via [dep_layer_hashes]. *)

val emit :
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  d10:D10.Config.t ->
  ?cache_urls:OpamUrl.t list ->
  ?cli_invocation:string list ->
  toolchain_name:string ->
  toolchain_layer:string ->
  Plan.t ->
  D10ir.Plan.t
(** Uses [plan.cache_root] for source-prep state and writes archives
    under [<d10.root>/d10ir/archives/]. *)
