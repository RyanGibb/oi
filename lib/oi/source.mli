(** Source acquisition: opam repositories, the reporepo (overlay-of-overlays
    metadata), pin-depends realisation, and the registry source mirror.

    A backward-compat aggregator: the four sub-areas now live in separate files
    ({!Source_repo}, {!Source_reporepo}, {!Source_pin}, {!Source_mirror}) and
    will eventually move into a dedicated [oi.reporepo] sublibrary. Existing
    callers spelled [Oi.Source.Repo.X] etc.; this module keeps that surface
    working by re-exporting each split file. *)

module Repo = Source_repo
module Reporepo = Source_reporepo
module Pin = Source_pin
module Mirror = Source_mirror
