open Cmdliner

[@@@warning "-32"]

let ( / ) = Filename.concat

(* Human-readable byte size ("1.2GB", "47MB", …). Defined here rather
   than reusing Cache.pp_size because we want to print directly into a
   string for simple output, not via an Fmt formatter. *)
let human_bytes b =
  if Int64.compare b 1_000_000_000L > 0 then
    Fmt.str "%.1fGB" (Int64.to_float b /. 1e9)
  else if Int64.compare b 1_000_000L > 0 then
    Fmt.str "%.1fMB" (Int64.to_float b /. 1e6)
  else if Int64.compare b 1_000L > 0 then
    Fmt.str "%.1fKB" (Int64.to_float b /. 1e3)
  else Fmt.str "%LdB" b

module Stats = struct
  let cmd =
    let run () cache_dir =
      Harness.run @@ fun env ->
      let {
        Harness.proc_mgr = _proc_mgr;
        fs = _fs;
        clock = _clock;
        sys = _sys;
        platform = _platform;
        os_key = _os_key;
        cache;
      } =
        Harness.bootstrap env cache_dir
      in
      let s = Oi.Source.Mirror.stats ~cache in
      Fmt.pr "Mirror: %s@." (Oi.Source.Mirror.dir ~cache);
      Fmt.pr "  blobs:      %d@." s.count;
      Fmt.pr "  total size: %s@." (human_bytes s.total_size)
    in
    let info =
      Cmd.info "stats"
        ~doc:"Show how many source tarballs are mirrored and their total size"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "$(b,oi registry mirror stats) prints a one-line summary of the \
               source mirror: the number of distinct tarballs it contains and \
               the total disk they occupy. Use it before an $(b,oi registry \
               export) to estimate how much data the export will ship, or to \
               track the size of the mirror over time.";
          ]
    in
    Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)
end

module Gc = struct
  let cmd =
    let run () cache_dir =
      Harness.run @@ fun env ->
      let {
        Harness.proc_mgr = _proc_mgr;
        fs = _fs;
        clock = _clock;
        sys = _sys;
        platform = _platform;
        os_key = _os_key;
        cache;
      } =
        Harness.bootstrap env cache_dir
      in
      let n = Oi.Source.Mirror.gc ~cache in
      Fmt.pr "Removed %d orphaned blob(s)@." n
    in
    let info =
      Cmd.info "gc"
        ~doc:"Delete mirrored tarballs that no package still references"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "$(b,oi registry mirror gc) removes source tarballs from the \
               mirror when no package in the index still points at them. This \
               happens after you have built a newer version of a package but \
               kept the mirror around; the old tarball lingers on disk even \
               though nothing uses it any more.";
            `P
              "Safe to run at any time. If a later rebuild needs a tarball \
               that has been collected, the mirror re-fetches it from upstream \
               on demand.";
          ]
    in
    Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)
end

module Verify = struct
  let cmd =
    let run () cache_dir =
      Harness.run @@ fun env ->
      let {
        Harness.proc_mgr = _proc_mgr;
        fs = _fs;
        clock = _clock;
        sys;
        platform = _platform;
        os_key = _os_key;
        cache;
      } =
        Harness.bootstrap env cache_dir
      in
      match Oi.Source.Mirror.verify ~sys ~cache with
      | [] -> Fmt.pr "All blobs verified OK@."
      | errs ->
          List.iter
            (fun (sha, msg) ->
              Fmt.epr "%a %s: %s@." Fmt.(styled `Red string) "BAD" sha msg)
            errs;
          Fmt.epr "%d blob(s) failed verification@." (List.length errs);
          exit 1
    in
    let info =
      Cmd.info "verify"
        ~doc:"Detect corrupted tarballs in the mirror by re-hashing them"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "$(b,oi registry mirror verify) walks every tarball in the \
               mirror, recomputes its sha256 checksum, and reports any file \
               whose bytes no longer match what the index records. The command \
               exits with a non-zero status if any tarball fails to verify, \
               which makes it useful in a scheduled integrity check.";
            `P
              "Run it before a large $(b,oi registry export) when the mirror \
               has been sitting around for a long time, or after any hardware \
               event that could have corrupted data on disk.";
          ]
    in
    Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)
end

module Ls = struct
  let cmd =
    let run () cache_dir package =
      Harness.run @@ fun env ->
      let {
        Harness.proc_mgr = _proc_mgr;
        fs = _fs;
        clock = _clock;
        sys = _sys;
        platform = _platform;
        os_key = _os_key;
        cache;
      } =
        Harness.bootstrap env cache_dir
      in
      let entries = Oi.Source.Mirror.list ~cache ?package () in
      (* One line per (source, package) reference. Columns:
         <pkg.version>  <kind>  <size>  <sha256 (first 12)>  <url>
       sha256 is shortened for readability; pipe the raw column to
       sqlite3 if you need full hashes. *)
      List.iter
        (fun (e : Oi.Source.Mirror.entry) ->
          let pkg = e.package_name ^ "." ^ e.package_version in
          let kind =
            match e.kind with `Main -> "main" | `Extra n -> "extra:" ^ n
          in
          let short_sha =
            if String.length e.sha256 >= 12 then String.sub e.sha256 0 12
            else e.sha256
          in
          Fmt.pr "%-40s  %-16s  %-12s  %10s  %s@." pkg kind short_sha
            (Fmt.str "%a" Oi.Cache.pp_size e.size)
            e.url)
        entries;
      if entries = [] then
        match package with
        | Some p -> Fmt.pr "No sources in mirror for package %s@." p
        | None -> Fmt.pr "Mirror is empty@."
    in
    let package =
      Arg.(
        value
        & opt (some string) None
        & info ~docv:"PKG"
            ~doc:
              "Restrict the listing to tarballs referenced by the named \
               package."
            [ "p"; "package" ])
    in
    let info =
      Cmd.info "list"
        ~doc:
          "Show every source tarball in the mirror, one row per package that \
           uses it"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "$(b,oi registry mirror list) prints one row for each tarball \
               reference in the mirror. Each row gives the package and version \
               that pulled the tarball in, the kind of source (main tarball or \
               extra patch), a short hash of the tarball, its on-disk size, \
               and the upstream URL it was fetched from.";
            `P
              "The same tarball can appear more than once when several \
               packages share a source. Those duplicate rows all point at the \
               same short hash, so the physical tarball is only counted once \
               in the mirror's size.";
            `P
              "Pass $(b,-p NAME) to restrict the output to a single package. \
               This is the fastest way to find out which sources a specific \
               package has contributed to the mirror.";
          ]
    in
    Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir $ package)
end

let cmd =
  let info =
    Cmd.info "mirror" ~doc:"Manage the local copy of upstream source tarballs"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Whenever $(b,oi) builds a package it keeps a copy of the source \
             tarball it fetched from upstream. Over time these copies form a \
             mirror of the opam ecosystem for the packages you actually use. \
             The mirror is shipped alongside the binary cache when you run \
             $(b,oi registry export), so downstream clients and offline \
             rebuilds do not have to reach the upstream servers.";
          `P
            "The subcommands in this group let you inspect and maintain the \
             mirror: $(b,stats) for a size summary, $(b,list) for a \
             per-tarball listing, $(b,verify) to re-hash every tarball, and \
             $(b,gc) to drop tarballs that no package still references.";
        ]
  in
  Cmd.group info [ Stats.cmd; Ls.cmd; Gc.cmd; Verify.cmd ]
