open Cmdliner

let ( / ) = Filename.concat


(* -- search -------------------------------------------------------------- *)

(* Glob match with [*] wildcard. Used for package-name filtering in
   [oi search]; binary-name filtering goes through SQL's [LIKE] inside
   [D10.Index]. *)
let glob_matches ~pattern name =
  if not (String.contains pattern '*') then pattern = name
  else
    let segs = String.split_on_char '*' pattern in
    let anchored_start = not (String.starts_with ~prefix:"*" pattern) in
    let anchored_end = not (String.ends_with ~suffix:"*" pattern) in
    let rec walk pos = function
      | [] -> true
      | [ last ] ->
          if anchored_end then
            String.ends_with ~suffix:last name
            && String.length name - String.length last >= pos
          else
            let _ = last in
            true
      | "" :: rest -> walk pos rest
      | seg :: rest -> (
          let rec find_from start =
            if start + String.length seg > String.length name then None
            else if String.sub name start (String.length seg) = seg then
              Some start
            else find_from (start + 1)
          in
          match find_from pos with
          | None -> false
          | Some i when anchored_start && pos = 0 && i <> 0 -> false
          | Some i -> walk (i + String.length seg) rest)
    in
    walk 0 (List.filter (fun s -> s <> "") segs)
    && ((not anchored_start)
       || List.hd segs = ""
       || String.starts_with ~prefix:(List.hd segs) name)

(* Scan every [repos/overlay-<h>-<v>/packages/] tree under [data_dir]
   for package names matching [pattern]. Returns a list of
   [(handle, version_tag, pkg_name, pkg_version)] rows, one per
   [<name>/<name.version>/opam] found. [version_tag] is the overlay
   version that the clone was pinned at.

   Keeping every version here lets the caller pick latest-per-
   (overlay, name) or expose all with [--all-versions]. *)
let scan_declared_packages ~data_dir ~pattern ~overlay_filter =
  let repos = data_dir / "repos" in
  if not (Sys.file_exists repos) then []
  else
    let entries = Sys.readdir repos |> Array.to_list in
    let rows = ref [] in
    List.iter
      (fun entry ->
        if String.starts_with ~prefix:"overlay-" entry then
          (* Parse overlay-<handle>-<version>. The version always
             matches [YYYYMMDD.N] so it contains no dashes; the
             handle is everything between "overlay-" and the last
             dash. *)
          let rest =
            String.sub entry (String.length "overlay-")
              (String.length entry - String.length "overlay-")
          in
          match String.rindex_opt rest '-' with
          | None -> ()
          | Some i ->
              let handle = String.sub rest 0 i in
              let version =
                String.sub rest (i + 1) (String.length rest - i - 1)
              in
              let keep =
                match overlay_filter with
                | [] -> true
                | xs -> List.mem handle xs
              in
              if keep then
                let pkgs_dir = repos / entry / "packages" in
                if Sys.file_exists pkgs_dir then
                  Array.iter
                    (fun name ->
                      if glob_matches ~pattern name then
                        let name_dir = pkgs_dir / name in
                        if Sys.is_directory name_dir then
                          Array.iter
                            (fun pkg_s ->
                              match OpamPackage.of_string_opt pkg_s with
                              | None -> ()
                              | Some p ->
                                  let v =
                                    OpamPackage.Version.to_string
                                      (OpamPackage.version p)
                                  in
                                  rows := (handle, version, name, v) :: !rows)
                            (Sys.readdir name_dir))
                    (Sys.readdir pkgs_dir))
      entries;
    List.rev !rows

(* Rank of a search-result state. Used to collapse redundant rows
   for the same (kind, overlay, name, version): if a package is
   built locally AND declared in the overlay, we keep [Local] and
   drop [Declared]. *)
type state = Local | Remote | Declared

let state_rank = function Local -> 0 | Remote -> 1 | Declared -> 2

let state_label = function
  | Local -> "local"
  | Remote -> "remote"
  | Declared -> "declared"

let state_styled st =
  let style =
    match st with Local -> `Green | Remote -> `Cyan | Declared -> `Yellow
  in
  Fmt.str "%a" Fmt.(styled style string) (state_label st)

(* Latest version per key, or every version when [all_versions] is set.
   Versions are compared via [OpamPackage.Version.compare]. *)
let trim_to_latest ~all_versions rows key version =
  if all_versions then rows
  else
    let by_key = Hashtbl.create 32 in
    List.iter
      (fun r ->
        let k = key r in
        let v = version r in
        match Hashtbl.find_opt by_key k with
        | None -> Hashtbl.replace by_key k r
        | Some prev
          when OpamPackage.Version.compare
                 (OpamPackage.Version.of_string v)
                 (OpamPackage.Version.of_string (version prev))
               > 0 ->
            Hashtbl.replace by_key k r
        | Some _ -> ())
      rows;
    Hashtbl.fold (fun _ r acc -> r :: acc) by_key []

(* One row of search output. Same shape for [bin] and [pkg] kinds so the
   caller can print them in a single uniform table. *)
type search_row = {
  kind : [ `Bin | `Pkg ];
  overlay : string; (* "@handle" or "-" *)
  binary : string option; (* filled for [Bin]; [None] for [Pkg] *)
  pkg_name : string;
  pkg_version : string;
  state : state;
  hash : string option; (* present for Local / Remote, absent for Declared *)
}

let cmd =
  let run () data_dir cache_dir registry all_versions overlay_filter long
      pattern =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr = _proc_mgr; fs = fs; clock = clock; sys = sys; platform = _platform; os_key = os_key; cache = cache } =
      Harness.bootstrap env cache_dir
    in
    (* Accept [@handle/PATTERN] as a shortcut for
       [--overlay=handle PATTERN]. Combines with any [--overlay] flags
       the user already passed. *)
    let pattern, overlay_filter =
      match Target.split_handle_prefix pattern with
      | None -> (pattern, overlay_filter)
      | Some (h, rest) -> (rest, h :: overlay_filter)
    in
    let clk = (clock :> D10.Config.clk) in
    let index_path = Layer_index.ensure_local ~sys ~fs ~clock:clk ~cache ~os_key in
    (match Layer_index.ensure_remote ~sys ~fs ~cache ~os_key ~registry with
    | Some remote_path -> Layer_index.merge_remote_into_local ~index_path ~remote_path
    | None -> ());
    let db = D10.Index.open_ ~path:index_path in
    let d10 : D10.Config.t =
      { sys; fs; clock = clk; root = Oi.Cache.root cache; os_key }
    in
    let overlay_of = function
      | None -> "-"
      | Some (h, _) ->
          if overlay_filter <> [] && not (List.mem h overlay_filter) then "-"
            (* shouldn't happen after later filter, but defensive *)
          else "@" ^ h
    in
    (* Binary matches from the index. Each hit emits one [Bin] row. *)
    let bin_rows =
      List.map
        (fun (bin, pkg_name, pkg_ver, hash, overlay) ->
          let st = if D10.Layer.succeeded d10 ~hash then Local else Remote in
          {
            kind = `Bin;
            overlay = overlay_of overlay;
            binary = Some bin;
            pkg_name;
            pkg_version = pkg_ver;
            state = st;
            hash = Some hash;
          })
        (D10.Index.search_binary db ~pattern ~os_key)
    in
    (* Built-package matches from the index. *)
    let pkg_built_rows =
      List.map
        (fun (pkg_name, pkg_ver, hash, overlay) ->
          let st = if D10.Layer.succeeded d10 ~hash then Local else Remote in
          {
            kind = `Pkg;
            overlay = overlay_of overlay;
            binary = None;
            pkg_name;
            pkg_version = pkg_ver;
            state = st;
            hash = Some hash;
          })
        (D10.Index.search_package db ~pattern ~os_key)
    in
    (* Declared-package matches scanned from overlay clones. *)
    let pkg_declared_rows =
      scan_declared_packages ~data_dir ~pattern ~overlay_filter
      |> List.map (fun (handle, _ov_version, name, version) ->
          {
            kind = `Pkg;
            overlay = "@" ^ handle;
            binary = None;
            pkg_name = name;
            pkg_version = version;
            state = Declared;
            hash = None;
          })
    in
    (* Apply overlay filter everywhere. The [@default] tag sits on the
       base opam-repository clone, so passing [--overlay=default] keeps
       base-repo rows. *)
    let filter_by_overlay rows =
      match overlay_filter with
      | [] -> rows
      | xs ->
          List.filter
            (fun r ->
              let h =
                if String.length r.overlay > 1 && r.overlay.[0] = '@' then
                  String.sub r.overlay 1 (String.length r.overlay - 1)
                else r.overlay
              in
              List.mem h xs)
            rows
    in
    let all_rows =
      filter_by_overlay bin_rows
      @ filter_by_overlay pkg_built_rows
      @ filter_by_overlay pkg_declared_rows
    in
    (* Collapse redundant rows for the same package: a locally built
       package also appears as a [Declared] row from the overlay scan;
       keep the strongest state ([Local] > [Remote] > [Declared]). *)
    let strongest_per_key =
      let by_key = Hashtbl.create 32 in
      List.iter
        (fun r ->
          let k = (r.kind, r.overlay, r.pkg_name, r.pkg_version, r.binary) in
          match Hashtbl.find_opt by_key k with
          | None -> Hashtbl.replace by_key k r
          | Some prev when state_rank r.state < state_rank prev.state ->
              Hashtbl.replace by_key k r
          | Some _ -> ())
        all_rows;
      Hashtbl.fold (fun _ r acc -> r :: acc) by_key []
    in
    (* Collapse to latest version per (kind, overlay, name, binary)
       unless [--all-versions]. *)
    let displayed =
      trim_to_latest ~all_versions strongest_per_key
        (fun r -> (r.kind, r.overlay, r.pkg_name, r.binary))
        (fun r -> r.pkg_version)
    in
    (* Stable ordering for readable output: bins first, then pkgs,
       alphabetic by name, version descending. *)
    let sorted =
      List.sort
        (fun a b ->
          let c =
            match (a.kind, b.kind) with
            | `Bin, `Pkg -> -1
            | `Pkg, `Bin -> 1
            | _ -> 0
          in
          if c <> 0 then c
          else
            let c = String.compare a.pkg_name b.pkg_name in
            if c <> 0 then c
            else
              let c =
                match (a.binary, b.binary) with
                | Some x, Some y -> String.compare x y
                | None, Some _ -> 1
                | Some _, None -> -1
                | None, None -> 0
              in
              if c <> 0 then c
              else
                OpamPackage.Version.compare
                  (OpamPackage.Version.of_string b.pkg_version)
                  (OpamPackage.Version.of_string a.pkg_version))
        displayed
    in
    if sorted = [] then Fmt.pr "No matches for %s@." pattern
    else begin
      let short_hash = function
        | None -> ""
        | Some h -> String.sub h 0 (min 12 (String.length h))
      in
      List.iter
        (fun r ->
          let kind_s = match r.kind with `Bin -> "bin" | `Pkg -> "pkg" in
          let nv =
            match r.binary with
            | Some b -> Fmt.str "%s (%s.%s)" b r.pkg_name r.pkg_version
            | None -> Fmt.str "%s.%s" r.pkg_name r.pkg_version
          in
          Fmt.pr "%-4s %-14s %-48s %-12s %s@." kind_s r.overlay nv
            (short_hash r.hash) (state_styled r.state);
          if long then
            match r.hash with
            | None -> ()
            | Some h ->
                let deps = D10.Index.deps db ~hash:h in
                if deps = [] then
                  Fmt.pr "  %a@." Fmt.(styled `Faint string) "(no deps)"
                else
                  List.iter
                    (fun (dep_name, dep_ver, dep_hash) ->
                      Fmt.pr "  %a %s.%s@."
                        Fmt.(styled `Faint string)
                        (String.sub dep_hash 0
                           (min 12 (String.length dep_hash)))
                        dep_name dep_ver)
                    deps)
        sorted
    end;
    D10.Index.close db
  in
  let pattern =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PATTERN"
          ~doc:
            "The name or glob to search for. The $(b,*) character is a \
             wildcard. Matching is against binary names and opam package \
             names. Prefix with $(b,@HANDLE/) to restrict the search to a \
             single overlay without passing $(b,--overlay) separately."
          [])
  in
  let all_versions =
    Arg.(
      value & flag
      & info
          ~doc:
            "List every cached version of each match. By default only the \
             latest version per overlay is shown."
          [ "all-versions" ])
  in
  let overlay =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:
            "Restrict results to an overlay. May be given more than once to \
             include several overlays. Equivalent to the $(b,@HANDLE/PATTERN) \
             shorthand."
          [ "overlay" ])
  in
  let long =
    Arg.(
      value & flag
      & info
          ~doc:
            "For built matches, print the direct dependencies of each result. \
             Declared-only rows have no build and therefore no dependency \
             list."
          [ "l"; "long" ])
  in
  let info =
    Cmd.info "search"
      ~doc:"Find binaries and opam packages across caches and overlays"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Look up $(b,PATTERN) across the local layer cache, the remote \
             registry, and every reporepo overlay's package list. One row per \
             match.";
          `S "COLUMNS";
          `I
            ( "$(b,KIND)",
              "$(b,bin) (binary in some layer's $(b,fs/bin/)) or $(b,pkg) \
               (opam metadata)." );
          `I
            ( "$(b,OVERLAY)",
              "$(b,@handle) the match came from, or $(b,-) for pin-depends / \
               untagged layers." );
          `I
            ( "$(b,NAME.VERSION)",
              "Package, with the binary name prefixed for $(b,bin) rows." );
          `I ("$(b,HASH)", "Short layer hash.");
          `I
            ( "$(b,STATE)",
              "$(b,local) (cached), $(b,remote) (fetchable), or $(b,declared) \
               (metadata only, no build)." );
          `S "OPTIONS";
          `I
            ( "$(b,--all-versions)",
              "Every cached/declared version (default: latest only)." );
          `I
            ( "$(b,--overlay=HANDLE)",
              "Filter to one overlay. $(b,@HANDLE/PATTERN) is shorthand." );
          `I ("$(b,-l)", "Print direct deps of each built match.");
          `S Manpage.s_examples;
          `Pre
            "  oi search dune\n\
            \  oi search 'ocaml*'\n\
            \  oi search @avsm/irmin\n\
            \  oi search --overlay=avsm --overlay=default 'fmt*'\n\
            \  oi search --all-versions -l jsont";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.data_dir $ Terms.cache_dir $ Terms.registry
      $ all_versions $ overlay $ long $ pattern)
