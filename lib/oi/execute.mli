[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** DAG-driven parallel build executor with per-package staging.

    Executes a {!Plan.t} as a dependency graph: one fiber per package, each
    awaiting promises for its in-plan deps before doing its own work. Source
    packages build into their own staging dir (seeded with hardlinks of every
    transitive dep layer), so [snapshot]/[diff] captures only the new files
    without contention with sibling fibers — no shared prefix and no mutex. A
    semaphore caps concurrent build phases (CPU-bound, fd-heavy); restores and
    installs run unsynchronised across the DAG. The {!Installer} module
    processes [.install] files directly via the opam libraries (no external
    [opam-installer] binary).

    Layers are captured via {!D10.Prefix.diff} against the staging baseline and
    stored in the d10 cache for future reuse. *)

type pkg_event =
  | Started of {
      pkg : string;
      phase : string;
          (** "restore" / "fetch" / "build" / "install" — the lifecycle
              sub-phase the package is entering. Multiple [Started] events fire
              per source package as it transitions fetch → build → install;
              binary packages get a single [Started] with phase ["restore"].
              Reporters use this to drive a phase indicator in the progress bar.
          *)
    }
  | Cached of { pkg : string }
  | Built of { pkg : string }
  | Build_failed of { pkg : string; log : string }
  | Dep_failed of { pkg : string; upstream_log : string }
  | Install_failed of { pkg : string; log : string }

type reporter = { pkg_event : pkg_event -> unit }
(** Typed progress reporter. [Execute.run] calls [pkg_event] exactly once per
    package for each terminal event (Cached / Built / Build_failed / Dep_failed
    / Install_failed), plus one [Started] event before each source-package build
    attempt. A caller who owns the display (e.g. [oi build --all]) supplies a
    reporter that keeps counters and drives a cross-invocation progress bar;
    callers who don't specify a reporter get a sane default that drives a local
    per-invocation bar. *)

val run :
  ?cache_urls:OpamUrl.t list ->
  ?jobs:int ->
  ?failed_layers:(string, string) Hashtbl.t ->
  ?reporter:reporter ->
  ?audit_base:Audit.context ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  sys:D10.Sysops.t ->
  os_key:string ->
  Plan.t ->
  unit
(** [run ~proc_mgr ~fs ~clock ~sys ~os_key plan] executes the build plan. Writes
    per-package failure output to
    [<cache_root>/build/logs/<pkg>-<short_hash>.log] and prints a single pointer
    line to stderr per failure so the live progress bar / summary stay readable.

    [cache_urls] are passed through to [OpamRepository.pull_tree] / [pull_file]
    for every fetch so that opam probes local and/or remote source mirrors (e.g.
    {!Source_mirror.url}, {!Source_mirror.remote_url}) before falling back to
    the upstream URL.

    [jobs] caps the number of packages whose build phase runs concurrently. Each
    in-flight build spawns subprocess pipes plus a recursive tree of compiler
    processes, so raising this too high exhausts the process's [RLIMIT_NOFILE].
    Resolution order: explicit [jobs] argument wins, then [OI_BUILD_PARALLELISM]
    env var, then [min (cpu_count) 4].

    [failed_layers] threads a layer-hash failure tracker through multiple [run]
    invocations. The key is the failing layer's hash; the value is the path to
    the build-failure log (empty for cascaded failures that inherit a dep's
    log). Any package whose [layer_hash] is in this table (or whose deps' layer
    hashes are) is skipped — useful for [oi build --all] where each solve group
    is a separate [run] and a failure in one group should prevent identical
    retries in later groups. When omitted, a fresh tracker is used for this call
    only.

    [audit_base] supplies the trigger / project / toolchain / host fields
    written into every {!Audit.event} this run emits; per-pkg [overlay] is
    folded in from the package plan. Falls back to {!Audit.default_context}
    (argv summary + hostname, no overlay/toolchain/project) when omitted. *)
