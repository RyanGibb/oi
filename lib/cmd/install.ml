open Cmdliner

let ( / ) = Filename.concat

(* Default install prefix: [$HOME/.local]. Matches the XDG-ish convention
   that [pip install --user] / [cargo install] / [pipx] all already use. *)
let default_prefix () =
  match Sys.getenv_opt "HOME" with
  | Some h -> h / ".local"
  | None -> "/usr/local"

(* Expand a leading [~] / [~/] to [$HOME]. Cmdliner doesn't run the
   value through the shell so [--prefix=~/.local] would otherwise create
   a literal [~] directory at cwd. *)
let expand_tilde p =
  if p = "" then p
  else if p = "~" then default_prefix () |> Filename.dirname
  else if String.length p >= 2 && p.[0] = '~' && p.[1] = '/' then
    match Sys.getenv_opt "HOME" with
    | Some h -> h ^ String.sub p 1 (String.length p - 1)
    | None -> p
  else p

(* Normalise for $PATH comparison: drop a trailing [/] so [~/.local/bin]
   and [~/.local/bin/] compare equal. *)
let strip_trailing_slash p =
  let n = String.length p in
  if n > 1 && p.[n - 1] = '/' then String.sub p 0 (n - 1) else p

let path_contains dir =
  let dir = strip_trailing_slash dir in
  let p = Stdlib.Option.value (Sys.getenv_opt "PATH") ~default:"" in
  String.split_on_char ':' p
  |> List.exists (fun e -> strip_trailing_slash e = dir)

(* Detect the user's shell to suggest the right rc file. Returns the
   shell name (e.g. ["bash"]) and a [(rc-path, append-syntax)] pair. *)
let detect_shell_rc () =
  let shell = Stdlib.Option.value (Sys.getenv_opt "SHELL") ~default:"" in
  match Filename.basename shell with
  | "zsh" -> Some ("zsh", "~/.zshrc")
  | "bash" -> Some ("bash", "~/.bashrc")
  | "fish" -> Some ("fish", "~/.config/fish/config.fish")
  | _ -> None

(* PATH hint when [bin_dir] isn't on $PATH. Phrased so the user can
   copy-paste either form (current shell vs persisted). The persisted
   line is shell-specific (fish uses [fish_add_path] rather than
   [export]) so we tailor the snippet when we can detect the shell. *)
let print_path_hint ~bin_dir =
  Oi.Say.newline ();
  Oi.Say.warn "%s is not on your PATH." bin_dir;
  Fmt.pr "Add it to the current shell:@.";
  Fmt.pr "  %s@." (Fmt.str "export PATH=\"%s:$PATH\"" bin_dir);
  match detect_shell_rc () with
  | None -> ()
  | Some ("fish", rc) ->
      Fmt.pr "Persist (fish):@.";
      Fmt.pr "  %s@." (Fmt.str "fish_add_path %s" bin_dir);
      Fmt.pr "  %a@." Oi.Style.dim_string (Fmt.str "(writes to %s)" rc)
  | Some (_, rc) ->
      Fmt.pr "Persist (add to %s):@." rc;
      Fmt.pr "  %s@."
        (Fmt.str "echo 'export PATH=\"%s:$PATH\"' >> %s" bin_dir rc)

(* Map a CLI target token to a [Build_pipeline.target]. [@h/pkg] →
   Overlay_pkg, [@h] → Overlay_all, anything else → Plain. *)
let parse_target s = Oi.Build_pipeline.parse s

(* Enumerate what [collect_install] would write for [root_layer_fs] into
   [prefix], i.e. every [bin/<n>], [sbin/<n>] file in the layer. Used
   for the pre-flight overwrite check; [share/] is deliberately
   skipped because data files commonly land at the same path across
   versions and [--force] is too coarse a hammer for them. *)
let preview_targets ~root_layer_fs ~prefix =
  let scan sub =
    let src = root_layer_fs / sub in
    if not (Sys.file_exists src) then []
    else
      try
        Sys.readdir src |> Array.to_list |> List.sort String.compare
        |> List.filter_map (fun name ->
            let s = src / name in
            match (Unix.stat s).st_kind with
            | Unix.S_REG -> Some (sub, name, prefix / sub / name)
            | _ -> None
            | exception Unix.Unix_error _ -> None)
      with Sys_error _ -> []
  in
  scan "bin" @ scan "sbin"

(* -- Main command body --------------------------------------------------- *)

let cmd =
  let run (c : Terms.common) refresh locked skip_local registry use_registry
      with_repos with_deps jobs toolchain_override prefix force targets =
    Harness.run @@ fun ~sw env ->
    let harness =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let { Harness.fs; clock; os_key; cache; _ } = harness in
    if targets = [] then
      Oi.Error.config_error
        "oi install: pass one or more PKG / @HANDLE/PKG targets.";
    let prefix = expand_tilde prefix in
    let bin_dir = prefix / "bin" in
    (* Pre-flight: make sure the prefix is writable (or creatable). The
       Dist copy code mkdirs on demand but a clear up-front error beats
       a half-finished tree if e.g. the user typoed a path under a dir
       they don't own. *)
    (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / prefix)
     with exn ->
       Oi.Error.config_error
         "oi install: cannot create %s: %s. Pass --prefix=DIR to install \
          elsewhere."
         prefix (Printexc.to_string exn));
    let {
      Pipeline_setup.env = pipeline_env;
      request = req;
      layer_remote;
      source_remote;
      _;
    } =
      Pipeline_setup.prepare ~harness ~refresh ~locked ~skip_local ~registry
        ~use_registry ~with_repos ~with_deps ~toolchain_override
        ~targets:(List.map parse_target targets)
        ()
    in
    let target_label = String.concat ", " targets in
    let solved, build_result =
      Progress_ui.with_ui ~target:target_label
        ~clock:(clock :> _ Eio.Resource.t)
        ~enabled:(Tty.is_tty ())
      @@ fun reporter ->
      let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
      let result =
        Oi.Build_pipeline.build pipeline_env ~reporter
          {
            solved;
            layer_remote;
            source_remote;
            jobs;
            upload_archive_url = None;
          }
      in
      (solved, result)
    in
    let cache_root = Oi.Cache.root_s cache in
    if
      List.for_all
        (fun (gr : Oi.Build_pipeline.group_result) -> Result.is_error gr.error)
        solved.groups
    then
      Oi.Error.config_error
        "oi install: every solve group failed; nothing to install.";
    (* Build outcome triage. Mirrors what [oi run] does, minus the
       empty-d10ir-plan diagnostic (the install path can't usefully
       reason about cache freshness — if [Build_pipeline.build]
       returned [Some r] with zero work, every root layer should
       still be present and we just copy from them). *)
    (match build_result with
    | None -> Oi.Error.config_error "oi install: build pipeline failed."
    | Some r when r.failed = 0 && r.skipped = 0 -> ()
    | Some r ->
        let pp_fail (f : D10ir.Direct.failure) =
          Fmt.str "%s.%s @ %s: %s — see %s" f.package.name f.package.version
            (D10ir.Direct.phase_to_string f.phase)
            f.error f.log_path
        in
        if r.failures <> [] then begin
          let summary = List.map pp_fail r.failures |> String.concat "\n  " in
          Oi.Error.config_error
            "oi install: build failed (%d node(s), %d skipped).@\n  %s" r.failed
            r.skipped summary
        end
        else
          Oi.Error.config_error
            "oi install: build failed (%d skipped). Re-run with \
             --verbosity=debug for the per-node trace."
            r.skipped);
    let root_hashes = Oi.Build_pipeline.root_layer_hashes solved in
    if root_hashes = [] then
      Oi.Error.config_error
        "oi install: solve succeeded but no root packages matched the \
         requested targets. Re-run with --refresh.";
    (* Conflict scan. Walk every root's [bin/] and [sbin/], record any
       destination path that already exists. Without [--force] this is
       a hard error listing every conflict — better than the build
       finishing only for half the binaries to refuse to install. *)
    let root_layer_fs h = cache_root / "layers" / os_key / h / "fs" in
    let conflicts =
      List.concat_map
        (fun h ->
          let layer_fs = root_layer_fs h in
          if not (Sys.file_exists layer_fs) then []
          else
            preview_targets ~root_layer_fs:layer_fs ~prefix
            |> List.filter (fun (_, _, dst) -> Sys.file_exists dst))
        root_hashes
    in
    (if conflicts <> [] && not force then
       let lines =
         List.map
           (fun (sub, name, dst) ->
             Fmt.str "  %s/%s %a %s" sub name Oi.Style.dim_string "→" dst)
           conflicts
         |> String.concat "\n"
       in
       Oi.Error.config_error
         "oi install: %d file(s) already exist under %s:@\n\
          %s@\n\
          Re-run with --force to overwrite."
         (List.length conflicts) prefix lines);
    (* Do the actual copy. [Dist.collect_install] always overwrites
       (O_TRUNC), which is what we want when the conflict pre-check
       has already gated on [--force]. *)
    let installed =
      List.concat_map
        (fun h ->
          let layer_fs = root_layer_fs h in
          if Sys.file_exists layer_fs then
            Dist.collect_install ~root:layer_fs ~dst:prefix
          else [])
        root_hashes
    in
    let count sub =
      List.length (List.filter (fun (s, _, _) -> s = sub) installed)
    in
    let n_bin = count "bin" in
    let n_sbin = count "sbin" in
    let n_share = count "share" in
    Oi.Say.newline ();
    Oi.Say.step "Installed %d binary(ies) to %s" (n_bin + n_sbin) bin_dir;
    List.iter
      (fun (sub, name, dst) ->
        if sub = "bin" || sub = "sbin" then
          Fmt.pr "  %s %a %s@." name Oi.Style.dim_string "→" dst)
      installed;
    if n_share > 0 then
      Oi.Say.info "+ %d data file(s) under %s/share/" n_share prefix;
    if (n_bin > 0 || n_sbin > 0) && not (path_contains bin_dir) then
      print_path_hint ~bin_dir
  in
  let prefix =
    Arg.(
      value
      & opt string (default_prefix ())
      & info ~docv:"DIR"
          ~doc:
            "Install prefix. Binaries land in $(i,DIR)$(b,/bin/), \
             $(i,DIR)$(b,/sbin/), data files in $(i,DIR)$(b,/share/). Default: \
             $(b,\\$HOME/.local). A leading $(b,~) is expanded against \
             $(b,\\$HOME)."
          [ "prefix" ])
  in
  let force =
    Arg.(
      value & flag
      & info
          ~doc:
            "Overwrite existing files under $(b,--prefix). Without this flag, \
             $(b,oi install) refuses to clobber anything already present in \
             $(b,bin/) / $(b,sbin/)."
          [ "force"; "f" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Package to install. Accepts $(b,PKG), $(b,@HANDLE/PKG), or \
             $(b,@HANDLE) (every root package the overlay declares). \
             Repeatable."
          [])
  in
  let info =
    Cmd.info "install"
      ~doc:"Build a package and promote its binaries to a user prefix"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve and build the listed targets the same way $(b,oi build) \
             would, then copy each root layer's $(b,bin/), $(b,sbin/), and \
             $(b,share/) contents into $(b,--prefix) (default \
             $(b,\\$HOME/.local)).";
          `P
            "Existing files under $(b,--prefix) are not overwritten unless \
             $(b,--force) is passed. After a successful install, $(b,oi \
             install) prints a PATH hint if $(b,--prefix/bin) isn't on \
             $(b,\\$PATH).";
          `S Manpage.s_examples;
          `Pre
            "  oi install dune\n\
            \  oi install @avsm/owntracks\n\
            \  oi install --prefix=/opt/oi --force utop merlin";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.locked $ Terms.skip_local
      $ Terms.registry $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps
      $ Terms.jobs $ Terms.toolchain $ prefix $ force $ targets)
