[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Terminal progress display.

    A unified progress display using a single progress bar line with dynamic
    status text showing active packages and their phases. The display clears on
    completion leaving only a summary line. *)

type phase = Fetch | Build | Install | Binary | Clone | Configure | Check

type t
(** An active progress display. *)

val run : now:(unit -> float) -> total:int -> title:string -> (t -> 'a) -> 'a
(** [run ~now ~total ~title f] renders a progress bar while [f t] executes. [f]
    receives a handle [t] for reporting activity. [now] provides wall-clock
    time. On return, the display clears and a summary is printed. *)

val start_activity : t -> string -> phase -> unit
val update_phase : t -> string -> phase -> unit
val finish_activity : t -> string -> unit
val fail_activity : t -> string -> cmd:string -> output:string -> unit
val finish : t -> unit

(** {1 Simple spinners} *)

val with_spinner : string -> (unit -> 'a) -> 'a
