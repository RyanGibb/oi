open Cmdliner

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

module Build = struct
  let cmd =
    let run () cache_dir output =
      Harness.run @@ fun env ->
      let { Harness.fs; sys; cache; _ } = Harness.bootstrap env cache_dir in
      let dst =
        match output with Some d -> d | None -> Oi.Source.Mirror.dir ~cache
      in
      let reporepo_path = Terms.reporepo_path () in
      Fmt.pr "Populating mirror at %s from %s/v1/@." dst reporepo_path;
      let s = Oi.Source.Mirror.populate ~fs ~sys ~reporepo_path ~dst in
      Fmt.pr
        "Walked %d package(s); fetched %d tarball(s), %d skipped, %d \
         failed.@."
        s.total s.fetched s.skipped
        (List.length s.failed);
      List.iter
        (fun (pkg, reason) ->
          Fmt.pr "  %a %s: %s@."
            Fmt.(styled `Yellow string)
            "failed" pkg reason)
        s.failed;
      if s.failed <> [] then exit 1
    in
    let output =
      Arg.(
        value
        & opt (some string) None
        & info ~docv:"DIR"
            ~doc:
              "Output directory. Defaults to the local cache mirror at \
               \\$XDG_CACHE_HOME/oi/mirror, usable as an opam cache_url. Pass \
               an absolute path to stage a separate directory for rsync to a \
               public registry."
            [ "o"; "output" ])
    in
    let info =
      Cmd.info "build" ~doc:"Populate a source mirror from the reporepo"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Walk $(b,<reporepo>/v1/), fetch each checksummed tarball, \
               verify it, and store it at \
               $(b,DIR/<algo>/<XX>/<full-hash>). Idempotent. Git sources \
               are not cached — opam clones them from the sha-pinned URLs \
               in v1/.";
          ]
    in
    Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir $ output)
end

module Stats = struct
  let cmd =
    let run () cache_dir =
      Harness.run @@ fun env ->
      let { Harness.cache; _ } = Harness.bootstrap env cache_dir in
      let s = Oi.Source.Mirror.stats ~cache in
      Fmt.pr "Mirror: %s@." (Oi.Source.Mirror.dir ~cache);
      Fmt.pr "  blobs:      %d@." s.count;
      Fmt.pr "  total size: %s@." (human_bytes s.total_size)
    in
    let info =
      Cmd.info "stats" ~doc:"Print blob count and total size of the mirror"
    in
    Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)
end

module Verify = struct
  let cmd =
    let run () cache_dir =
      Harness.run @@ fun env ->
      let { Harness.sys; cache; _ } = Harness.bootstrap env cache_dir in
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
      Cmd.info "verify" ~doc:"Re-hash every blob; report any whose bytes drifted"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Walk every blob, recompute its hash, and report any whose \
               filename no longer matches its content. Exits non-zero on any \
               mismatch.";
          ]
    in
    Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir)
end

let cmd =
  let info =
    Cmd.info "mirror" ~doc:"Build and inspect the local source tarball mirror"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "A content-addressed copy of every tarball declared in the \
             reporepo's $(b,v1/) tree. Layout matches opam's $(b,cache_url) \
             format ($(b,<dir>/<algo>/<XX>/<full-hash>)) — directly usable \
             as a fallback source and rsyncable to a public mirror.";
          `P
            "Git sources are not mirrored; opam clones them from the \
             sha-pinned URLs $(b,oi repo bump) writes into v1/.";
        ]
  in
  Cmd.group info [ Build.cmd; Stats.cmd; Verify.cmd ]
