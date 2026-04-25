[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Read project metadata from the *.opam files in a directory. *)

type extra_repo = { name : string; url : string }

type pin = { pkg : OpamPackage.t; url : OpamUrl.t; declared_in : string }
(** [declared_in] is the source *.opam filename, used in error messages. *)

type t = {
  deps : string list;
      (** Direct deps, sorted, deduplicated. Excludes local packages and
          "ocaml". *)
  local_packages : string list;
      (** Names of *.opam files in the dir (without the [.opam] suffix). *)
  extra_repos : extra_repo list;
      (** URL-form entries from [x-repos:] across all *.opam. Each is keyed
          by a synthetic name derived from the URL's MD5, so duplicates dedup
          naturally. Declared-order preserved. *)
  pins : pin list;
      (** Union of [pin-depends:] entries across all *.opam, in declared order.
      *)
  overlays : string list;
      (** Reporepo-handle entries from [x-repos:] (the [@HANDLE] form) across
          all *.opam. Each element is a bare handle (e.g. ["avsm"], with the
          leading [@] stripped); {!Oi.Reporepo} resolves them at sync time.
          Deduplicated, declared-order preserved. *)
}

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> t
(** [load ~fs dir] scans every [*.opam] file in [dir]. Raises
    {!Error.config_error} on malformed [x-repos:] values, on conflicting
    [extra_repos] (same URL synthetic-named, different URL — only possible on
    hash collision), or on conflicting [pins] (same package name, different
    URL or version). *)
