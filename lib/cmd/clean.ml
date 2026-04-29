open Cmdliner

let ( / ) = Filename.concat

(* -- Per-package layer invalidation -------------------------------------- *)

let parse_target s =
  match String.index_opt s '.' with
  | Some i when i > 0 && i < String.length s - 1 ->
      let name = String.sub s 0 i in
      let version = String.sub s (i + 1) (String.length s - i - 1) in
      (name, Some version)
  | _ -> (s, None)

let short h = String.sub h 0 (min 12 (String.length h))

let pkg_clean ~sys ~fs ~clock ~cache ~os_key ~target ~dry_run =
  let name, version_opt = parse_target target in
  let layers_root = Oi.Cache.root_s cache / "layers" / os_key in
  let index_path = Layer_index.ensure_local ~sys ~fs ~clock ~cache ~os_key in
  if not (Sys.file_exists index_path) then begin
    Fmt.pr "No layer index for %s; nothing to do.@." os_key;
    0
  end
  else begin
    let db = D10.Index.open_ ~path:index_path in
    let direct =
      match version_opt with
      | Some v -> (
          match D10.Index.find_layer db ~name ~version:v ~os_key with
          | Some (h, _) -> [ (name, v, h) ]
          | None -> [])
      | None ->
          D10.Index.search_package db ~pattern:name ~os_key
          |> List.map (fun (n, v, h, _) -> (n, v, h))
    in
    if direct = [] then begin
      Fmt.pr "No cached layers found for %s.@." target;
      D10.Index.close db;
      0
    end
    else begin
      let direct_hashes = List.map (fun (_, _, h) -> h) direct in
      let rec close acc frontier =
        if frontier = [] then acc
        else
          let next = D10.Index.dependents db ~hashes:frontier ~os_key in
          let fresh = List.filter (fun h -> not (List.mem h acc)) next in
          close (acc @ fresh) fresh
      in
      let dependents = close [] direct_hashes in
      let all_hashes = direct_hashes @ dependents in
      let verb = if dry_run then "Would remove" else "Removing" in
      Fmt.pr "@[<v>%s %d layer(s) for %s:@," verb (List.length direct) target;
      List.iter (fun (n, v, h) -> Fmt.pr "  %s.%s  %s@," n v (short h)) direct;
      if dependents <> [] then begin
        Fmt.pr "%s %d dependent layer(s):@," verb (List.length dependents);
        List.iter (fun h -> Fmt.pr "  %s@," (short h)) dependents
      end;
      Fmt.pr "@]@.";
      if not dry_run then begin
        List.iter
          (fun h ->
            let dir = layers_root / h in
            Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
          all_hashes;
        D10.Index.delete_layers db ~hashes:all_hashes;
        Fmt.pr
          "Removed %d layer(s). Run 'oi build' to rebuild what you still \
           need.@."
          (List.length all_hashes)
      end;
      D10.Index.close db;
      List.length all_hashes
    end
  end

(* -- Bulk clean ---------------------------------------------------------- *)

let cmd =
  let run () cache_dir data_dir all toolchains sources binaries dune_cache repos
      dry_run target =
    Harness.run @@ fun env ->
    let { Harness.fs; clock; sys; os_key; cache; _ } =
      Harness.bootstrap env cache_dir
    in
    let bulk_flags =
      all || toolchains || sources || binaries || dune_cache || repos
    in
    match target with
    | Some t ->
        if bulk_flags then begin
          Fmt.epr
            "oi clean: PKG positional cannot be combined with --all / \
             --toolchains / --sources / --layers / --dune / --repos.@.";
          exit 1
        end
        else
          let clk = (clock :> D10.Config.clk) in
          let _ =
            pkg_clean ~sys ~fs ~clock:clk ~cache ~os_key ~target:t ~dry_run
          in
          ()
    | None ->
        let clean_any = bulk_flags in
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
          Fmt.pr
            "@,Use --all to clean everything, or select specific items.@]@."
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
    Arg.(value & flag & info ~doc:"Every category below. Full reset." [ "all" ])
  in
  let toolchains =
    Arg.(
      value & flag
      & info ~doc:"Fixed-prefix toolchain installs (oxcaml)." [ "toolchains" ])
  in
  let sources =
    Arg.(
      value & flag & info ~doc:"Source tarballs and pin clones." [ "sources" ])
  in
  let binaries =
    Arg.(
      value & flag
      & info ~doc:"Binary layer cache and per-script build dirs." [ "layers" ])
  in
  let dune_cache =
    Arg.(value & flag & info ~doc:"Dune's shared build cache." [ "dune" ])
  in
  let repos =
    Arg.(
      value & flag
      & info ~doc:"Reporepo and $(b,--with-repo) clones." [ "repos" ])
  in
  let dry_run =
    Arg.(
      value & flag
      & info ~doc:"Print what would be removed; delete nothing."
          [ "n"; "dry-run" ])
  in
  let target =
    Arg.(
      value
      & pos 0 (some string) None
      & info ~docv:"PKG[.VERSION]"
          ~doc:
            "Drop layers for one package. Layers that transitively depend on a \
             removed entry are cascaded. Mutually exclusive with the bulk \
             flags."
          [])
  in
  let info =
    Cmd.info "clean" ~doc:"Free up disk space by deleting cached data"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Remove rebuildable cache data. With no flags or $(b,PKG), lists \
             each category and its disk usage. Flags are additive. A project's \
             $(b,_oi/) is never touched.";
          `P
            "$(b,PKG) drops every cached version of that package; \
             $(b,PKG.VERSION) drops one. Layers that transitively depend on a \
             removed entry are dropped too, so a poisoned build can't survive \
             in a downstream layer. Re-run $(b,oi build) to rebuild.";
          `Pre "  oi clean --layers\n  oi clean dune\n  oi clean dune.3.22.1 -n";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ Terms.cache_dir $ Terms.data_dir $ all
      $ toolchains $ sources $ binaries $ dune_cache $ repos $ dry_run $ target)
