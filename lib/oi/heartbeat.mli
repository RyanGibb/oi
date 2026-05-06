(** Periodic in-flight reporter for long-running phases.

    Tracks a live set of named tasks (URL fetches, package builds, ...) and
    periodically logs the lot at warn level so a wedged fiber surfaces in the
    user's terminal instead of leaving them staring at a stale progress bar.

    Each tracker runs an Eio daemon under the caller's switch; the daemon
    exits when the switch closes. The log line is silent when no tasks are
    in flight, so a healthy phase costs one [Eio.Time.sleep] per interval.

    Default interval is 30 seconds. Override per-process via the
    [OI_HEARTBEAT_INTERVAL] env var (seconds; [0] disables). *)

type t
(** Tracker handle, scoped to the [sw] passed to {!create}. *)

val create :
  ?interval_s:float ->
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  string ->
  t
(** [create ~sw ~clock label] allocates a tracker tagged [label] (used as the
    prefix in log lines) and forks the heartbeat daemon under [sw]. The
    daemon exits when [sw] closes. [interval_s] overrides {!default_interval_s}
    / [OI_HEARTBEAT_INTERVAL]; [0] disables the daemon entirely (the tracker
    still works but never logs). *)

val track : t -> string -> (unit -> 'a) -> 'a
(** [track t name f] runs [f ()], registering [name] in [t]'s in-flight set
    for the duration. Removes the entry on return or exception. Safe to call
    concurrently from multiple fibers. *)

val in_flight : t -> string list
(** [in_flight t] is the current set of names registered against [t], oldest
    first. Useful for testing. *)
