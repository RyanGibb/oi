type phase =
  | Solving
  | Fetching
  | Baking
  | Building
  | Assembling

let phase_to_string = function
  | Solving -> "solving"
  | Fetching -> "fetching"
  | Baking -> "baking"
  | Building -> "building"
  | Assembling -> "assembling"

type fetch_kind = Layer | Source

type event =
  | Phase_started of { phase : phase; label : string }
  | Phase_done of phase
  | Status of string
  | Aggregate of { phase : phase; total : int; current : int }
  | Fetch_started of {
      kind : fetch_kind;
      key : string;
      pkg : string;
      size : int64;
    }
  | Fetch_progress of {
      kind : fetch_kind;
      key : string;
      bytes : int64;
      total : int64;
        (* declared total bytes if known (HTTP Content-Length etc.),
           else 0L. Lets callers refine the per-fetch total mid-flight
           when [Fetch_started] couldn't supply it. *)
    }
  | Fetch_finished of { kind : fetch_kind; key : string }
  | Plan_ready of D10ir.Plan.t
  | Build of D10ir.Direct.event
  | Build_summary of D10ir.Direct.result

type reporter = { event : event -> unit }

let null = { event = (fun _ -> ()) }
