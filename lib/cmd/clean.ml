open Cmdliner


let cmd =
  let run () cache_dir data_dir all toolchains sources binaries dune_cache repos
      dry_run =
    Harness.run @@ fun env ->
    let { Harness.proc_mgr = _proc_mgr; fs = fs; clock = _clock; sys = sys; platform = _platform; os_key = _os_key; cache = cache } =
      Harness.bootstrap env cache_dir
    in
    let clean_any =
      all || toolchains || sources || binaries || dune_cache || repos
    in
    if not clean_any then begin
      Fmt.pr "@[<v>%a@,@," Fmt.(styled `Bold string) "Cleanable items:";
      let items = Oi.Cache.cleanable_items cache ~data_dir in
      List.iter
        (fun (item : Oi.Cache.item) ->
          let path_s = Eio.Path.native_exn item.path in
          if Sys.file_exists path_s then
            Fmt.pr "  --%-20s %a  %s@," item.label Oi.Cache.pp_size
              (Oi.Cache.size ~sys item.path)
              item.description
          else
            Fmt.pr "  --%-20s %a  %s@," item.label
              Fmt.(styled `Faint string)
              "(empty)" item.description)
        items;
      Fmt.pr "@,Use --all to clean everything, or select specific items.@]@."
    end
    else begin
      let items = Oi.Cache.cleanable_items cache ~data_dir in
      let find_item label =
        List.find_opt (fun (i : Oi.Cache.item) -> i.label = label) items
      in
      let rm label =
        match find_item label with
        | None -> ()
        | Some item ->
            let path_s = Eio.Path.native_exn item.path in
            if Sys.file_exists path_s then begin
              let sz = Oi.Cache.size ~sys item.path in
              if dry_run then
                Fmt.pr "Would remove %s (%a) %s@." label Oi.Cache.pp_size sz
                  path_s
              else begin
                Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / path_s);
                Fmt.pr "Removed %s (%a)@." label Oi.Cache.pp_size sz
              end
            end
      in
      if all || toolchains then rm "toolchains";
      if all || sources then rm "sources";
      if all || binaries then rm "layers";
      if all || binaries then rm "runs";
      if all || dune_cache then rm "dune";
      if all || repos then rm "repos";
      Fmt.pr "Done.@."
    end
  in
  let all =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove every category at once (caches, builds, configuration, \
             cloned repositories)."
          [ "all" ])
  in
  let toolchains =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove fixed-prefix toolchain installs under \
             \\$XDG_CACHE_HOME/oi/toolchains/. Reinstalled on the next \
             $(b,--toolchain=NAME) invocation that needs them."
          [ "toolchains" ])
  in
  let sources =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove cached source tarballs and pinned source clones from the \
             mirror."
          [ "sources" ])
  in
  let binaries =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove the binary layer cache and the per-script build \
             directories."
          [ "layers" ])
  in
  let dune_cache =
    Arg.(
      value & flag
      & info ~doc:"Remove dune's shared cross-project build cache." [ "dune" ])
  in
  let repos =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove the local clones of the opam package repositories and of \
             any $(b,--with-repo) extras."
          [ "repos" ])
  in
  let dry_run =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print the items that would be removed and their sizes, but do not \
             delete anything."
          [ "n"; "dry-run" ])
  in
  let info =
    Cmd.info "clean" ~doc:"Free up disk space by deleting cached data"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Remove rebuildable cache data. With no flags, lists each \
             category, its disk usage, and the flag that deletes it. Flags are \
             additive — $(b,oi clean --sources --layers) is fine. Nothing \
             under a project's $(b,_oi/) is touched.";
          `I
            ( "$(b,--toolchains)",
              "Fixed-prefix toolchain installs (oxcaml). Rebuilt on next use."
            );
          `I
            ( "$(b,--sources)",
              "Cached source tarballs and pin source clones. Re-fetched from \
               upstream on next solve." );
          `I
            ( "$(b,--layers)",
              "Pre-built binary layer cache and per-script build dirs. Forces \
               source rebuilds on next $(b,oi run)." );
          `I ("$(b,--dune)", "Dune's shared build cache.");
          `I
            ( "$(b,--repos)",
              "Reporepo overlay clones and $(b,--with-repo) extras. Re-cloned \
               on next solve." );
          `I
            ( "$(b,--all)",
              "Every category above, plus the assembled-prefix cache and \
               script-run dirs. Full reset." );
          `P
            "$(b,-n) / $(b,--dry-run) reports which paths would be removed \
             without deleting. Recommended before $(b,--all).";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.cache_dir $ Terms.data_dir $ all $ toolchains
      $ sources $ binaries $ dune_cache $ repos $ dry_run)

(* -- registry list ------------------------------------------------------- *)

