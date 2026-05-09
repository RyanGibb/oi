[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Backward-compatibility shim. The four reporepo / source areas were
   split out of one 2300-line [source.ml] into separate files
   ([source_repo.ml], [source_reporepo.ml], [source_pin.ml],
   [source_mirror.ml]) so they can be navigated independently.
   Existing callers spelled [Oi.Source.Repo.X] etc. — this shim
   keeps that surface working.

   Future work: lift these four files into a dedicated [oi.reporepo]
   sublibrary so the build / solve / executor code in [lib/oi] stops
   sharing a compilation unit with reporepo metadata. Blocked on:
   [source_*] depend on [Cache], [Project], [Build_progress], [Stamp],
   all in [lib/oi]. To break the cycle, those four foundational
   modules need their own sublibrary ([oi.base] perhaps), and every
   in-[lib/oi] caller of [Source.X] (~70 sites in [pipeline.ml],
   [toolchain.ml], [build_pipeline.ml], [execute.ml],
   [build_request.ml]) must reach the new [oi.reporepo] module
   instead. Out of scope for the current refactor. *)

module Repo = Source_repo
module Reporepo = Source_reporepo
module Pin = Source_pin
module Mirror = Source_mirror
