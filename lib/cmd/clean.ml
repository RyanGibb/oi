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
  if not (Eio.Path.is_file Eio.Path.(fs / index_path)) then begin
    Oi.Say.info "no layer index for %s; nothing to do" os_key;
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
      Oi.Say.info "no cached layers found for %s" target;
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
      let dim s = Fmt.str "%a" Oi.Style.dim_string s in
      Oi.Say.step "%s %d layer(s) for %s" verb (List.length direct) target;
      List.iter
        (fun (n, v, h) -> Oi.Say.info "%s.%s  %s" n v (dim (short h)))
        direct;
      if dependents <> [] then begin
        Oi.Say.step "%s %d dependent layer(s)" verb (List.length dependents);
        List.iter (fun h -> Oi.Say.info "%s" (dim (short h))) dependents
      end;
      if not dry_run then begin
        List.iter
          (fun h ->
            Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / layers_root / h))
          all_hashes;
        D10.Index.delete_layers db ~hashes:all_hashes;
        Oi.Say.ok "removed %d layer(s)" (List.length all_hashes);
        Oi.Say.info "run 'oi build' to rebuild what you still need"
      end;
      D10.Index.close db;
      List.length all_hashes
    end
  end

(* -- Bulk clean ---------------------------------------------------------- *)

let cmd =
  let run (c : Terms.common) all toolchains sources binaries dune_cache repos
      opam_root pins dry_run target =
    Harness.run @@ fun ~sw env ->
    let { Harness.fs; clock; sys; os_key; cache; _ } =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let data_dir = c.data_dir in
    let bulk_flags =
      all || toolchains || sources || binaries || dune_cache || repos
      || opam_root || pins
    in
    match target with
    | Some t ->
        if bulk_flags then begin
          Oi.Say.error
            "PKG positional cannot be combined with bulk flags (--all, \
             --toolchains, --sources, --layers, --dune, --repos, --opam-root, \
             --pins)";
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
          Fmt.pr "%a@.@." Oi.Style.header_string "Cleanable items";
          let items = Oi.Cache.cleanable_items cache ~data_dir in
          let rows =
            List.map
              (fun (item : Oi.Cache.item) ->
                let flag = "--" ^ item.label in
                let size =
                  if
                    Eio.Path.is_directory item.path
                    || Eio.Path.is_file item.path
                  then
                    Tty.Span.text
                      (Fmt.str "%a" Oi.Cache.pp_size
                         (Oi.Cache.size ~sys item.path))
                  else Tty.Span.styled Oi.Style.dim "(empty)"
                in
                [ Tty.Span.text flag; size; Tty.Span.text item.description ])
              items
          in
          let table =
            Tty.Table.of_rows ~header_style:Oi.Style.header
              [
                Tty.Table.column "FLAG";
                Tty.Table.column ~align:`Right "SIZE";
                Tty.Table.column "DESCRIPTION";
              ]
              rows
          in
          Tty.Table.pp Fmt.stdout table;
          Oi.Say.newline ();
          Oi.Say.info "use --all to clean everything, or select specific items"
        end
        else begin
          let items = Oi.Cache.cleanable_items cache ~data_dir in
          let rm_item (item : Oi.Cache.item) =
            if Eio.Path.is_directory item.path || Eio.Path.is_file item.path
            then begin
              let sz = Oi.Cache.size ~sys item.path in
              if dry_run then
                Oi.Say.info "would remove %s (%a) %s" item.label
                  Oi.Cache.pp_size sz
                  (Eio.Path.native_exn item.path)
              else begin
                Eio.Path.rmtree ~missing_ok:true item.path;
                Oi.Say.ok "removed %s (%a)" item.label Oi.Cache.pp_size sz
              end
            end
          in
          (* [--all] sweeps everything in [cleanable_items], so adding
             a new category there picks it up automatically. Per-flag
             groups bundle related caches: [--layers] for instance also
             clears the solve cache, run-cache, build state and
             prefixes, since all of them index off layer hashes that
             just got dropped. *)
          let want_label = function
            | _ when all -> true
            | "toolchains" -> toolchains
            | "sources" | "mirror" -> sources
            | "layers" | "runs" | "run-cache" | "solve-cache" | "build"
            | "prefixes" ->
                binaries
            | "dune" -> dune_cache
            | "repos" -> repos
            | "opam-root" -> opam_root
            | "pins" -> pins
            | _ -> false
          in
          List.iter
            (fun (item : Oi.Cache.item) ->
              if want_label item.label then rm_item item)
            items;
          Oi.Say.step "Done"
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
  let opam_root =
    Arg.(
      value & flag
      & info
          ~doc:
            "Opam scaffolding under $(b,\\$OI_DATA_DIR/opam-root/). \
             Regenerated on demand."
          [ "opam-root" ])
  in
  let pins =
    Arg.(
      value & flag
      & info ~doc:"Pin-depends sources and synthesized packages trees."
          [ "pins" ])
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
             $(b,_oi/) and the reporepo (your overlay catalogue) are never \
             touched — the reporepo is user-authored data, not cache.";
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
      const run $ Terms.common $ all $ toolchains $ sources $ binaries
      $ dune_cache $ repos $ opam_root $ pins $ dry_run $ target)
