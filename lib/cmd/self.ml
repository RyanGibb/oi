open Cmdliner

let ( / ) = Filename.concat

(* Default git URL for [--dev]; overridable via [OI_DEV_URL]. The opam
   file on the reporepo's [@avsm/oi] entry tracks this same upstream
   ([x-oi-source-url] in [oi.dev/opam]); duplicating it here keeps
   [oi self update --dev] working even before the reporepo has been
   cloned. *)
let dev_url () =
  match Sys.getenv_opt "OI_DEV_URL" with
  | Some s when s <> "" -> s
  | _ -> "git+https://github.com/avsm/oi#main"

(* Atomic-replace a binary at [dst] with the contents of [src]. Writes
   to a sibling [.tmp] file in the same directory (so [rename] is
   guaranteed atomic), [chmod 0755], then renames over the target.
   Linux and macOS both keep the old inode alive for any process that
   has the binary open / executing, so replacing the running [oi] is
   safe — the current process keeps running on the old image, the next
   [exec] picks up the new one. *)
let install_binary ~fs ~src ~dst =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(fs / Filename.dirname dst);
  let tmp = Fmt.str "%s.%d.tmp" dst (Unix.getpid ()) in
  (try Unix.unlink tmp with Unix.Unix_error _ -> ());
  let in_ch = open_in_bin src in
  Fun.protect
    ~finally:(fun () -> close_in_noerr in_ch)
    (fun () ->
      let out_ch = open_out_bin tmp in
      Fun.protect
        ~finally:(fun () -> close_out_noerr out_ch)
        (fun () ->
          let buf = Bytes.create 65536 in
          let rec loop () =
            let n = input in_ch buf 0 (Bytes.length buf) in
            if n > 0 then begin
              output_bytes out_ch buf;
              if n = Bytes.length buf then loop ()
            end
          in
          loop ()));
  Unix.chmod tmp 0o755;
  Unix.rename tmp dst

let where_cmd =
  let run () =
    let exe = Oi.Selfexe.current () in
    let writable = Oi.Selfexe.is_writable exe in
    Fmt.pr "%a %s@." Oi.Style.header_string "executable:" exe;
    Fmt.pr "%a %s@." Oi.Style.header_string "writable:  "
      (if writable then "yes" else "no");
    match Oi.Selfexe.resolve_target () with
    | In_place p ->
        Fmt.pr "%a in place (%s)@." Oi.Style.header_string "target:    " p
    | Fallback { current; install_dir } ->
        Fmt.pr "%a fallback to %s/oi (current %s is not writable)@."
          Oi.Style.header_string "target:    " install_dir current
  in
  let info =
    Cmd.info "where"
      ~doc:"Show where the running oi binary lives and whether it's writable"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Print the absolute path to the currently-running $(b,oi) \
             executable, whether the user can overwrite it, and the install \
             target $(b,oi self update) would pick (in-place if writable, \
             $(b,~/.local/bin/oi) otherwise).";
        ]
  in
  Cmd.v info Term.(const run $ Terms.log)

let update_cmd =
  let run (c : Terms.common) refresh registry use_registry jobs dev =
    Harness.run @@ fun ~sw env ->
    let {
      Harness.proc_mgr;
      fs;
      clock;
      sys;
      platform;
      os_key;
      cache;
      http_session;
      _;
    } =
      Harness.bootstrap ~sw ~data_dir:c.data_dir env c.cache_dir
    in
    let data_dir = c.data_dir in
    let target = Oi.Selfexe.resolve_target () in
    let oi_dst, oix_dst =
      match target with
      | In_place path ->
          Oi.Say.step "Updating in-place: %s" path;
          (path, Filename.dirname path / "oix")
      | Fallback { current; install_dir } ->
          Oi.Say.warn
            "%s is not writable; installing into %s/oi (and oi/oix) instead"
            current install_dir;
          (install_dir / "oi", install_dir / "oix")
    in
    let conf =
      Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    let { Terms.layer_remote; source_remote } =
      Terms.remotes_of ~url:registry ~mode:use_registry
    in
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    (* [--dev] overrides the reporepo's [@avsm/oi] pin with HEAD of
       upstream main, materialised as a [--with=git+URL] dep. The pinned
       URL contributes its own [*.opam] files as solver roots, which is
       what we want — building exactly that tree. *)
    let with_deps = if dev then [ dev_url () ] else [] in
    let extra_deps, url_project =
      Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    let extra_constraints = Oi.Project.Script.constraints extra_deps in
    let url_overlays =
      Oi.Pipeline.filter_compatible_overlays
        ~reporepo_path:(Terms.reporepo_path ()) ~toolchain:None
        url_project.overlays
    in
    let with_repos =
      if dev then url_overlays
        (* In non-[--dev] mode, target the [@avsm/oi] overlay where the
           [oi] package lives. *)
      else [ "avsm" ]
    in
    let cli_extras =
      Target.cli_extra_repos ~fs ~sys ?toolchain:None with_repos
    in
    let all_extras =
      Target.merge_extras ~cli:cli_extras ~project:url_project.extra_repos
    in
    let tc_handles = with_repos |> List.sort_uniq String.compare in
    let toolchain =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:true
        ~override:None ~handles:tc_handles ()
    in
    let names =
      if dev then
        (* From the cloned URL: take every *.opam at the repo root as a
           solver root. [oi] is the one we care about, but its sibling
           libraries (d10, osrel) need to come along for the build. *)
        List.map OpamPackage.Name.of_string url_project.roots
      else [ OpamPackage.Name.of_string "oi" ]
    in
    let names =
      Oi.Pipeline.drop_override_compiler_roots ~override:None ~toolchain names
    in
    let on_phase msg = Oi.Say.step "%s" msg in
    let on_progress = Oi.Say.progress in
    let layer_hashes =
      Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf ~os_key
        ~session:http_session ~refresh ~extra_repos:all_extras
        ~pins:url_project.pins ~constraints:extra_constraints ?layer_remote
        ?source_remote ?jobs ?toolchain
        ?local_packages_dir:url_project.packages_dir ~on_phase ~on_progress
        names
    in
    let prefix =
      Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
    in
    Oi.Say.progress_clear ();
    let new_oi = prefix / "bin" / "oi" in
    let new_oix = prefix / "bin" / "oix" in
    if not (Sys.file_exists new_oi) then
      Oi.Error.msg "build succeeded but %s is missing" new_oi;
    install_binary ~fs ~src:new_oi ~dst:oi_dst;
    Oi.Say.ok "installed oi → %s" oi_dst;
    if Sys.file_exists new_oix then begin
      install_binary ~fs ~src:new_oix ~dst:oix_dst;
      Oi.Say.ok "installed oix → %s" oix_dst
    end
    else Oi.Say.warn "oix not found in built prefix (%s); skipping" new_oix
  in
  let dev =
    Arg.(
      value & flag
      & info
          ~doc:
            "Install $(b,oi) built from the upstream git repo's $(b,main) \
             branch instead of the version pinned in the reporepo. Use to get \
             unreleased fixes. Override the URL with $(b,OI_DEV_URL)."
          [ "dev" ])
  in
  let info =
    Cmd.info "update" ~doc:"Build a fresh oi binary and install it"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Use $(b,oi) to build $(b,oi) from source, then install the \
             resulting binary over the currently-running executable. The \
             update is atomic — the running process keeps executing on the old \
             image; the next $(b,oi) invocation picks up the new binary.";
          `P
            "If the current binary's path is not writable (system install, \
             read-only mount), $(b,oi self update) installs into \
             $(b,~/.local/bin/oi) instead and warns. Add that directory to \
             $(b,PATH) to use the updated binary.";
          `P "Run $(b,oi self where) to preview the install target.";
          `S "OPTIONS";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.jobs $ dev)

let cmd =
  let info =
    Cmd.info "self" ~doc:"Manage the running oi binary"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Subcommands for inspecting ($(b,oi self where)) and updating \
             ($(b,oi self update)) the currently-running $(b,oi) install.";
        ]
  in
  Cmd.group info [ where_cmd; update_cmd ]
