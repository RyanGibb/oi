open Cmdliner


let log_src = Logs.Src.create "oi.cmd.registry_export"
module Log = (val Logs.src_log log_src : Logs.LOG)
[@@@warning "-32"]

let cmd =
  let run () cache_dir registry output =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr = _proc_mgr; fs = fs; clock = clock; sys = sys; platform = _platform; os_key = os_key; cache = cache } =
      Harness.bootstrap env cache_dir
    in
    Registry_index.do_registry_export ~fs ~clock ~sys ~os_key ~cache ~registry ~output
  in
  let output =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"DIR"
          ~doc:
            "The directory the published registry will be written into. \
             Created if it does not exist."
          [])
  in
  let info =
    Cmd.info "export"
      ~doc:"Publish the local cache to a directory for HTTP serving or rsync"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry export) copies every package in the local cache \
             into $(b,DIR) in the on-disk layout that an $(b,oi) client \
             expects from a remote registry. The result is a tree of \
             compressed layer archives along with a sqlite $(b,index.db) and a \
             sha256 $(b,OINDEX.txt) per platform. Serve the tree with any \
             static HTTP server, or $(b,rsync) it to another machine. No \
             $(b,oi) code runs on the server.";
          `P
            "Source tarballs are written once at the top of $(b,DIR) under \
             $(b,sources/), rather than once per platform, because the source \
             for a package is the same regardless of which distribution will \
             compile it.";
          `P
            "The index records the overlay handle and version that produced \
             each layer, so clients that want to scope to a specific overlay \
             can query the index directly. There is no separate per-overlay \
             tree to fetch. Use $(b,oi search) against the published registry \
             to see what overlays it covers.";
          `P
            "When $(b,--registry URL) is given, $(b,oi) downloads the existing \
             registry's index first and merges its rows into the one being \
             published. This is the safe way to $(b,rsync) back to a shared \
             registry: without the merge, your export would overwrite entries \
             contributed by other machines.";
        ]
  in
  Cmd.v info
    Term.(const run $ Terms.log $ Terms.cache_dir $ Terms.registry $ output)

(* Render the per-target summary emitted at the end of [oi registry build].
   One row per target, in the order the user asked for them. Columns are
   truncated/padded so the table stays readable even with long overlay
   package names. Group-level counts are repeated across all targets that
   shared a group — two targets under the same solver solution get the same
   "packages / built / cached" figures, which matches how the build itself
   treated them. *)
