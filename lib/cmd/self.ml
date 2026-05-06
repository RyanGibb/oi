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

(* -- version ------------------------------------------------------------ *)

(* What an agent needs to know about the binary it just invoked: the
   release/git version, the platform it was built for and runs against,
   and the schema versions it can read/write. Wiring all of those into
   one document makes [oi self version --format=json] sufficient as a
   compatibility probe — no follow-up calls to figure out what to expect
   from the rest of the JSON surface. *)

type version_info = {
  oi_version : string;
  os_key : string;
  schema_json : string;
  schema_cache : int;
  schema_data : int;
  schema_toolchains : int;
  schema_solver_cache : string;
}

let oi_version () =
  match Build_info.V1.version () with
  | Some v -> Build_info.V1.Version.to_string v
  | None -> "n/a"

let schemas_codec =
  let open Jsont in
  Object.map ~kind:"schemas" (fun json cache data toolchains solver_cache ->
      (json, cache, data, toolchains, solver_cache))
  |> Object.mem "json" string ~enc:(fun (j, _, _, _, _) -> j)
  |> Object.mem "cache" int ~enc:(fun (_, c, _, _, _) -> c)
  |> Object.mem "data" int ~enc:(fun (_, _, d, _, _) -> d)
  |> Object.mem "toolchains" int ~enc:(fun (_, _, _, t, _) -> t)
  |> Object.mem "solver_cache" string ~enc:(fun (_, _, _, _, s) -> s)
  |> Object.finish

let version_codec =
  let open Jsont in
  Object.map ~kind:"oi_self_version"
    (fun _schema_version oi_version os_key schemas ->
      let ( schema_json,
            schema_cache,
            schema_data,
            schema_toolchains,
            schema_solver_cache ) =
        schemas
      in
      {
        oi_version;
        os_key;
        schema_json;
        schema_cache;
        schema_data;
        schema_toolchains;
        schema_solver_cache;
      })
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "oi_version" string ~enc:(fun v -> v.oi_version)
  |> Object.mem "os_key" string ~enc:(fun v -> v.os_key)
  |> Object.mem "schemas" schemas_codec ~enc:(fun v ->
      ( v.schema_json,
        v.schema_cache,
        v.schema_data,
        v.schema_toolchains,
        v.schema_solver_cache ))
  |> Object.finish

let version_cmd =
  let run (c : Terms.common) =
    Harness.run @@ fun ~sw env ->
    let { Harness.os_key; _ } =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let info =
      {
        oi_version = oi_version ();
        os_key;
        schema_json = Oi.Stamp.json_schema_version;
        schema_cache = Oi.Stamp.cache_schema;
        schema_data = Oi.Stamp.data_schema;
        schema_toolchains = Oi.Stamp.toolchains_schema;
        schema_solver_cache = Oi.Stamp.solver_cache_schema;
      }
    in
    match c.format with
    | Json -> (
        match
          Jsont_bytesrw.encode_string ~format:Jsont.Indent version_codec info
        with
        | Ok s ->
            print_string s;
            print_newline ()
        | Error e -> Oi.Error.config_error "json encode failed: %s" e)
    | Text ->
        Fmt.pr "oi %s (%s)@." info.oi_version info.os_key;
        Fmt.pr
          "schemas: json=%s cache=%d data=%d toolchains=%d solver_cache=%s@."
          info.schema_json info.schema_cache info.schema_data
          info.schema_toolchains info.schema_solver_cache
  in
  let info =
    Cmd.info "version" ~doc:"Print oi version, platform, and JSON schemas"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Identify the running $(b,oi) and what it can read/write. With \
             $(b,--format=json), emits the version, host $(b,os_key), and \
             every schema constant ([oi]'s wire-format JSON, plus on-disk \
             cache/data/toolchain/solver-cache schemas). Agents should call \
             this first to verify compatibility before parsing other \
             $(b,--format=json) output.";
        ]
  in
  Cmd.v info Term.(const run $ Terms.common)

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

(* List the [oi.<version>] entries published in the [@avsm/oi] overlay
   of the local (just-refreshed) reporepo. Used by [oi self update] to
   decide whether a newer release exists before kicking off a build —
   we don't want to spend minutes resolving + compiling a fresh
   toolchain just to discover the only available version is the one
   already running.

   TODO: once [oi] is published to opam-repository (or its own
   first-class overlay), drop the [@avsm/oi] hardcoding and discover the
   overlay/handle the way every other command does — via the toolchain
   chain, project overlays, or [--with-repo]. The "oi" name and the
   pre-release ergonomic of "look in [@avsm]" are temporary. *)
let list_oi_versions ~reporepo_path =
  let pkg_dir =
    Filename.concat
      (Filename.concat
         (Filename.concat (Filename.concat reporepo_path "v1") "avsm")
         "packages")
      "oi"
  in
  if not (Sys.file_exists pkg_dir) then []
  else
    Sys.readdir pkg_dir |> Array.to_list
    |> List.filter_map (fun entry ->
        let prefix = "oi." in
        if String.starts_with ~prefix entry then
          Some
            (String.sub entry (String.length prefix)
               (String.length entry - String.length prefix))
        else None)

(* Pick the highest [oi.<version>] in [available] that is strictly
   greater than [current] and is not the [dev] pin. Returns [None] when
   no such candidate exists, in which case the caller short-circuits
   with "already up to date". [current="n/a"] (dev/git build with no
   release tag) skips the [>] filter so we still pick the highest
   non-[dev] candidate. *)
let pick_update ~current available =
  let cmp = OpamPackage.Version.compare in
  let v_of = OpamPackage.Version.of_string in
  let known = current <> "n/a" && current <> "" in
  let cur = if known then Some (v_of current) else None in
  available
  |> List.filter (fun s -> s <> "dev")
  |> List.filter (fun s ->
      match cur with None -> true | Some c -> cmp (v_of s) c > 0)
  |> List.sort (fun a b -> cmp (v_of b) (v_of a))
  |> function
  | [] -> None
  | hd :: _ -> Some hd

let update_cmd =
  let run (c : Terms.common) registry use_registry jobs dev =
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
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let data_dir = c.data_dir in
    (* Always refresh on self-update. A bare [oi self update] right
       after a schema bump (which clears the cache via {!Oi.Stamp})
       leaves the reporepo's pin set untouched on disk but the
       cache-side downloads gone — the solver then sees stale pin
       material and fails with "no solution found". Forcing refresh
       costs a few seconds of repo pull and matches what every user
       was already doing manually. *)
    let refresh = true in
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
    (* In release mode, walk the freshly-refreshed reporepo for oi
       releases newer than the running binary. If nothing strictly
       newer (excluding [oi.dev]) is published, exit early — no point
       solving and compiling just to reinstall the same version. *)
    let pinned_update =
      if dev then None
      else
        let current = oi_version () in
        let available =
          list_oi_versions ~reporepo_path:(Terms.reporepo_path ())
        in
        match pick_update ~current available with
        | None ->
            Oi.Say.ok "oi %s is up to date (no newer release in @avsm)" current;
            exit 0
        | Some v ->
            Oi.Say.step "Updating oi %s -> %s" current v;
            Some v
    in
    (* [--dev] overrides the reporepo's [@avsm/oi] pin with HEAD of
       upstream main, materialised as a [--with=git+URL] dep. The pinned
       URL contributes its own [*.opam] files as solver roots, which is
       what we want — building exactly that tree. *)
    let with_deps = if dev then [ dev_url () ] else [] in
    let extra_deps, url_project =
      Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
    in
    let extra_constraints =
      let base = Oi.Project.Script.constraints extra_deps in
      match pinned_update with
      | None -> base
      | Some v ->
          OpamPackage.Name.Map.add
            (OpamPackage.Name.of_string "oi")
            (`Eq, OpamPackage.Version.of_string v)
            base
    in
    let url_overlays =
      Oi.Pipeline.filter_compatible_overlays
        ~reporepo_path:(Terms.reporepo_path ()) ~toolchain:None
        url_project.overlays
    in
    let with_repos =
      if dev then url_overlays
        (* TODO: drop the hardcoded [@avsm] overlay once [oi] is published
           to opam-repository or to its own first-class overlay. Until
           then, [@avsm/oi] is where the release pin lives — see
           {!list_oi_versions} for the matching enumeration. *)
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
      Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
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
      Oi.Pipeline.strip_compiler_roots_for_override ~override:None ~toolchain names
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
      const run $ Terms.common $ Terms.registry $ Terms.use_registry
      $ Terms.jobs $ dev)

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
  Cmd.group info [ version_cmd; where_cmd; update_cmd ]
