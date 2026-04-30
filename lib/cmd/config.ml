open Cmdliner

(* Human-readable byte size ("1.2GB", "47MB", …) for the source-mirror
   summary. Matches the format the old [oi mirror stats] used. *)
let human_bytes b =
  if Int64.compare b 1_000_000_000L > 0 then
    Fmt.str "%.1fGB" (Int64.to_float b /. 1e9)
  else if Int64.compare b 1_000_000L > 0 then
    Fmt.str "%.1fMB" (Int64.to_float b /. 1e6)
  else if Int64.compare b 1_000L > 0 then
    Fmt.str "%.1fKB" (Int64.to_float b /. 1e3)
  else Fmt.str "%LdB" b

let cmd =
  let run () cache_dir data_dir =
    Harness.run @@ fun env ->
    let { Harness.fs; os_key; cache; _ } = Harness.bootstrap env cache_dir in
    Fmt.pr "@[<v>%a@," Oi.Style.header_string "Platform";
    Fmt.pr "  os-key:     %s@," os_key;
    Fmt.pr "  ocaml:      %s (relocatable)@," Workspace.ocaml_version;
    Fmt.pr "@,%a@," Oi.Style.header_string "Directories";
    Fmt.pr "  data:       %s@," data_dir;
    Fmt.pr "  cache:      %s@," cache_dir;
    Fmt.pr "@,%a@," Oi.Style.header_string "Registry";
    Fmt.pr "  url:        %s@," Terms.default_registry;
    Fmt.pr "  index TTL:  %gs@," Layer_index.remote_index_max_age;
    Fmt.pr "@,%a@," Oi.Style.header_string "Source mirror";
    let mirror_stats = Oi.Source.Mirror.stats ~cache in
    Fmt.pr "  dir:        %s@," (Oi.Source.Mirror.dir ~cache);
    Fmt.pr "  blobs:      %d@," mirror_stats.count;
    Fmt.pr "  total size: %s@," (human_bytes mirror_stats.total_size);
    Fmt.pr "@,%a@," Oi.Style.header_string "Toolchains";
    Fmt.pr "  install root:  %s@," (Oi.Toolchain.default_root ());
    List.iter
      (fun (s : Oi.Toolchain.summary) ->
        let url_with_ref =
          match s.ref_ with Some r -> Fmt.str "%s#%s" s.url r | None -> s.url
        in
        let mode_tag =
          if s.relocatable then Fmt.str "[%a]" Oi.Style.ok_string "relocatable"
          else Fmt.str "[%a]" Oi.Style.warn_string "fixed-prefix"
        in
        let default_tag =
          if s.is_default then Fmt.str "  [%a]" Oi.Style.accent_string "default"
          else ""
        in
        Fmt.pr "  %a  %s%s  %s@," Oi.Style.header_string s.handle mode_tag
          default_tag url_with_ref;
        if s.depends <> [] then
          Fmt.pr "    depends:    %s@," (String.concat ", " s.depends);
        Fmt.pr "    roots:      %s@," (String.concat ", " s.roots);
        if s.tools <> [] then
          Fmt.pr "    tools:      %s@," (String.concat ", " s.tools);
        if s.relocatable then ()
        else
          match s.installs with
          | [] ->
              Fmt.pr "    status:     %a@," Oi.Style.dim_string "not installed"
          | xs ->
              List.iter
                (fun (path, ready) ->
                  let status =
                    if ready then Fmt.str "%a" Oi.Style.ok_string "ready"
                    else Fmt.str "%a" Oi.Style.warn_string "partial"
                  in
                  Fmt.pr "    install:    %s  %s@," status path)
                xs)
      (Oi.Toolchain.available ());
    Fmt.pr "@,%a@," Oi.Style.header_string "Base overlays (from reporepo)";
    let base = Oi.Source.Reporepo.base_entries () in
    if base = [] then
      Fmt.pr
        "  %a no 'relocatable' overlay in reporepo %s. Run 'oi repo add' to \
         bootstrap.@,"
        Oi.Style.warn_string "(none)" (Terms.reporepo_path ())
    else
      let path = Terms.reporepo_path () in
      List.iter
        (fun (e : Oi.Source.Reporepo.entry) ->
          let dir =
            Oi.Source.Reporepo.overlay_packages_dir ~path ~handle:e.handle
          in
          let status =
            if e.url = "" then
              Fmt.str "%a" Oi.Style.dim_string "definition only"
            else if Sys.file_exists dir then
              Fmt.str "%a" Oi.Style.ok_string "materialised"
            else Fmt.str "%a" Oi.Style.warn_string "not materialised"
          in
          Fmt.pr "  %a.%s  %s  %s@," Oi.Style.header_string e.handle e.version
            status e.url)
        base;
      Fmt.pr "@]@.";
      let cwd_s, _ = Workspace.resolved_cwd fs in
      let proj =
        match Oi.Project.load ~fs cwd_s with
        | exception Sys_error _ -> None
        | exception Eio.Exn.Io _ -> None
        | p -> Some p
      in
      match proj with
      | None -> ()
      | Some p ->
          if p.extra_repos <> [] then begin
            Fmt.pr "@.Project extra repositories:@.";
            List.iter
              (fun (r : Oi.Project.extra_repo) ->
                Fmt.pr "  %-20s %s@." r.name r.url)
              p.extra_repos
          end;
          if p.pins <> [] then begin
            Fmt.pr "@.Project pin-depends:@.";
            List.iter
              (fun (pin : Oi.Project.pin) ->
                Fmt.pr "  %-20s %s@."
                  (OpamPackage.to_string pin.pkg)
                  (OpamUrl.to_string pin.url))
              p.pins
          end;
          if p.overlays <> [] then begin
            Fmt.pr "@.Project overlays (x-repos @-handles):@.";
            List.iter (fun h -> Fmt.pr "  %s@." h) p.overlays
          end;
          (match p.packages_dir with
           | None -> ()
           | Some dir ->
               Fmt.pr "@.Project local opam-repository:@.";
               Fmt.pr "  %s@." dir);
          (* Dev tools: run the probe registry against cwd and print one
           row per tool. Shown in every project (even one with no
           hits), so it's obvious when merlin / odoc would end up in
           [_oi/tools/] after the next sync. *)
          let tool_results = Oi.Project.Tool.probe ~fs cwd_s in
          Fmt.pr "@.Dev tools:@.";
          List.iter
            (fun (r : Oi.Project.Tool.result) ->
              let mark =
                if r.hit then Fmt.str "%a" Oi.Style.ok_string "hit"
                else Fmt.str "%a" Oi.Style.dim_string "miss"
              in
              Fmt.pr "  %-18s %-4s %s@." r.spec.name mark r.detail)
            tool_results
  in
  let info =
    Cmd.info "config" ~doc:"Show oi's view of this machine and project"
      ~man:
        [
          `S Manpage.s_description;
          `P "Print how $(b,oi) sees the current machine and project.";
          `I
            ( "$(b,Platform)",
              "OS, arch, distribution. The solve picks different packages per \
               platform — check here first when the result surprises." );
          `I
            ( "$(b,Directories)",
              "Cache and data directories in use. $(b,OI_CACHE_DIR) and \
               $(b,OI_DATA_DIR) override XDG defaults." );
          `I
            ( "$(b,Repositories)",
              "Cloned opam repositories backing the solver, each with last \
               refresh time." );
          `I
            ( "$(b,Toolchains)",
              "Toolchains the reporepo defines (handles accepted by \
               $(b,--toolchain=NAME)), tagged $(b,[relocatable]) or \
               $(b,[fixed-prefix]), with their primary source URL and any \
               existing installs under \\$XDG_CACHE_HOME/oi/toolchains." );
          `I
            ( "$(b,Source mirror)",
              "Local opam $(b,cache_url) mirror at \
               \\$XDG_CACHE_HOME/oi/mirror, populated as a side effect of \
               builds. Use $(b,oi build --all --export DIR) to publish a \
               consistent snapshot." );
          `I
            ( "$(b,Project extras)",
              "$(b,x-repos:) and $(b,pin-depends:) entries declared in the \
               current directory's $(b,*.opam) files, plus any project-local \
               opam-repository (a $(b,packages/) tree with a $(b,repo) \
               marker). Only shown when at least one is present." );
          `I
            ( "$(b,Dev tools)",
              "Project-probed tools the next $(b,oi build) would install. \
               $(b,hit) means the trigger fired ($(b,.ocamlformat) for \
               $(b,ocamlformat), $(b,dune-project) for $(b,mdx)); $(b,miss) \
               skips that tool. Tools from the active toolchain's \
               $(b,x-oi-toolchain-tools) field appear under $(b,Toolchains) \
               above and install unconditionally." );
        ]
  in
  Cmd.v info Term.(const run $ Terms.log $ Terms.cache_dir $ Terms.data_dir)

(* -- clean --------------------------------------------------------------- *)

(* dir_size and pp_size are now in Oi.Cache *)
