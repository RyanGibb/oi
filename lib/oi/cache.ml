[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

type t = { fs : Eio.Fs.dir_ty Eio.Path.t; root : string }

let create ~root fs =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / root);
  { fs; root }

let root t = Eio.Path.(t.fs / t.root)
let root_s t = t.root
let dune_root t = t.root / "dune"
let fs t = t.fs

(* -- Script run cache ---------------------------------------------------- *)

let run_dir t ~hash =
  let dir = t.root / "runs" / hash in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(t.fs / dir);
  Eio.Path.(t.fs / dir)

(* -- Pin-depends cache --------------------------------------------------- *)

let pins_dir t = t.root / "pins"

(* -- Toolchain install root ---------------------------------------------- *)

(* XDG-derived path used by [Toolchain] for fixed-prefix installs and by
   [cleanable_items] to nuke them. Lives here so [Cache] doesn't need
   to depend on [Toolchain]. *)
let toolchains_root () =
  let base =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some v when v <> "" -> v
    | _ -> (
        match Sys.getenv_opt "HOME" with
        | Some h -> h / ".cache"
        | None -> "/tmp")
  in
  base / "oi" / "toolchains"

(* -- Sentinel-based freshness -------------------------------------------- *)

let refresh_max_age = 86_400.0

let fresh ~refresh ~sentinel ~max_age =
  if refresh then false
  else if not (Sys.file_exists sentinel) then false
  else
    try
      let age = Unix.time () -. (Unix.stat sentinel).Unix.st_mtime in
      age <= max_age
    with Unix.Unix_error _ -> false

(* -- Build logs ---------------------------------------------------------- *)

module Logs = struct
  let dir ~cache_root = cache_root / "build" / "logs"

  let ensure ~fs ~cache_root =
    try
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(fs / dir ~cache_root)
    with _ -> ()

  let short_hash h = String.sub h 0 (min 12 (String.length h))

  let path ~cache_root ~kind ~name ~hash =
    dir ~cache_root / (kind ^ "-" ^ name ^ "-" ^ short_hash hash ^ ".log")

  let write ~fs ~cache_root path content =
    ensure ~fs ~cache_root;
    try Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) content
    with _ -> ()
end

(* -- Cleanup ------------------------------------------------------------- *)

type item = {
  label : string;
  path : Eio.Fs.dir_ty Eio.Path.t;
  description : string;
}

let cleanable_items t ~data_dir =
  let p sub = Eio.Path.(t.fs / t.root / sub) in
  let d sub = Eio.Path.(t.fs / data_dir / sub) in
  [
    {
      label = "sources";
      path = p "sources";
      description = "Downloaded source tarballs";
    };
    {
      label = "layers";
      path = p "layers";
      description = "Binary layer cache (day10 format)";
    };
    { label = "runs"; path = p "runs"; description = "Cached script builds" };
    {
      label = "prefixes";
      path = p "prefixes";
      description = "Assembled prefix cache (hardlinks)";
    };
    { label = "dune"; path = p "dune"; description = "Dune shared build cache" };
    { label = "repos"; path = d "repos"; description = "Cloned repositories" };
    {
      label = "opam-root";
      path = d "opam-root";
      description = "Opam scaffolding (regenerated on demand)";
    };
    {
      label = "pins";
      path = p "pins";
      description = "Pin-depends sources and synthesized packages trees";
    };
    {
      (* Resolved via [toolchains_root] rather than [p "toolchains"]
         because the toolchain path is XDG-derived independently of
         [OI_CACHE_DIR], and we want clean to nuke what's actually on
         disk regardless of any env mismatch. *)
      label = "toolchains";
      path = Eio.Path.(t.fs / toolchains_root ());
      description = "Built compiler toolchains (oxcaml, etc.)";
    };
  ]

let size ~sys path =
  let path_s = Eio.Path.native_exn path in
  try
    let s = String.trim (D10.Sysops.Cmd.run_out sys [ "du"; "-sk"; path_s ]) in
    let kb =
      Int64.of_string_opt (List.hd (String.split_on_char '\t' s))
      |> Stdlib.Option.value ~default:0L
    in
    Int64.mul kb 1024L
  with _ -> 0L

let pp_size fmt sz =
  if sz > 1_000_000_000L then Fmt.pf fmt "%.1fGB" (Int64.to_float sz /. 1e9)
  else if sz > 1_000_000L then Fmt.pf fmt "%.1fMB" (Int64.to_float sz /. 1e6)
  else if sz > 1_000L then Fmt.pf fmt "%.1fKB" (Int64.to_float sz /. 1e3)
  else Fmt.pf fmt "%LdB" sz
