open Cmdliner

[@@@warning "-32"]

let ( / ) = Filename.concat

let reporepo_term =
  Arg.(
    value
    & opt string (Terms.reporepo_path ())
    & info ~docv:"DIR"
        ~doc:
          "Local directory that contains the reporepo clone to operate on. \
           Falls back to $(b,\\$OI_REPOREPO), and then to \
           $(b,\\$OI_DATA_DIR/reporepo) under the XDG data hierarchy."
        [ "reporepo" ])

let reporepo_url_term =
  Arg.(
    value
    & opt string (Terms.reporepo_url ())
    & info ~docv:"URL"
        ~doc:
          "Git URL to clone the reporepo from when the local clone does not \
           yet exist. Falls back to $(b,\\$OI_REPOREPO_URL) and then to the \
           built-in upstream. Once the local clone exists, $(b,oi) never pulls \
           from this URL again. The working copy is yours to edit, commit, and \
           push."
        [ "reporepo-url" ])

let depend_term =
  Arg.(
    value & opt_all string []
    & info ~docv:"HANDLE[=VERSION]"
        ~doc:
          "Make this overlay depend on another one. The form \
           $(b,HANDLE=VERSION) pins a specific recorded version; a bare \
           $(b,HANDLE) accepts any version. May be given more than once. When \
           omitted on a non-base overlay, $(b,oi) auto-fills the current \
           latest versions of $(b,default) and $(b,relocatable)."
        [ "depend"; "d" ])

let parse_depend_spec s =
  match String.index_opt s '=' with
  | None -> (s, None)
  | Some i ->
      let h = String.sub s 0 i in
      let v = String.sub s (i + 1) (String.length s - i - 1) in
      (h, Some v)

let parse_handle_version s =
  match String.index_opt s '=' with
  | None -> (s, None)
  | Some i ->
      (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

(* Visible-column width of the toolchain target column. Counts the
   em-dash (one display column despite 3-byte UTF-8) as 1 so column
   alignment doesn't drift on entries without a toolchain. *)
let handle_span (e : Oi.Source.Reporepo.entry) =
  let style =
    if e.toolchain_name <> None then Oi.Style.accent else Oi.Style.header
  in
  Tty.Span.styled style e.handle

let toolchain_target_span (e : Oi.Source.Reporepo.entry) =
  match e.toolchain with
  | Some t -> Tty.Span.styled Oi.Style.info t
  | None -> Tty.Span.styled Oi.Style.dim "—"

let commit_span commit =
  let short =
    if commit = "" then ""
    else String.sub commit 0 (min 7 (String.length commit))
  in
  Tty.Span.styled Oi.Style.dim short

(* Upstream tip status for a reporepo entry, computed by re-running
   [git ls-remote] against its URL + ref. *)
type upstream_status =
  | Fresh  (** Pinned commit matches the upstream tip. *)
  | Stale of string  (** Upstream tip differs; carries its 40-char sha. *)
  | Unknown  (** [git ls-remote] failed (offline, auth, moved URL…). *)
  | Definition_only
      (** Entry has no [url:] (toolchain definition / metadata-only): nothing to
          check upstream. *)

let short_sha s = String.sub s 0 (min 7 (String.length s))

let check_upstream ~sys (e : Oi.Source.Reporepo.entry) =
  if e.url = "" then Definition_only
  else
    match Oi.Source.Reporepo.ls_remote_sha ~sys ?ref_:e.ref_ e.url with
    | tip when tip = e.commit -> Fresh
    | tip -> Stale tip
    | exception _ -> Unknown

let status_span = function
  | Fresh -> Tty.Span.styled Oi.Style.ok "up-to-date"
  | Unknown -> Tty.Span.styled Oi.Style.warn "unreachable"
  | Definition_only -> Tty.Span.styled Oi.Style.info "toolchain"
  | Stale tip ->
      Tty.Span.(
        styled Oi.Style.error "stale"
        ++ space
        ++ styled Oi.Style.dim (Fmt.str "(%s)" (short_sha tip)))

(* Toolchain section: definitions ([toolchain_name] set) printed under
   their CLI-facing name (the [x-oi-toolchain-name] field) rather than
   the [toolchain-*] reporepo handle. *)
let toolchain_cli_name (e : Oi.Source.Reporepo.entry) =
  match e.toolchain_name with Some n -> n | None -> e.handle

let mode_of (e : Oi.Source.Reporepo.entry) =
  match e.relocatable with
  | Some true -> ("relocatable", Oi.Style.ok)
  | Some false -> ("fixed-prefix", Oi.Style.warn)
  | None -> ("?", Oi.Style.dim)

let overlay_row ?status (e : Oi.Source.Reporepo.entry) =
  let base =
    [
      handle_span e;
      Tty.Span.text e.version;
      commit_span e.commit;
      toolchain_target_span e;
    ]
  in
  let status_cells =
    match status with None -> [] | Some s -> [ status_span s ]
  in
  base @ status_cells @ [ Tty.Span.styled Oi.Style.dim e.url ]

let overlay_columns ~with_status =
  let base =
    [
      Tty.Table.column "HANDLE";
      Tty.Table.column "VERSION";
      Tty.Table.column "COMMIT";
      Tty.Table.column "TOOLCHAIN";
    ]
  in
  let status_col = if with_status then [ Tty.Table.column "STATUS" ] else [] in
  base @ status_col @ [ Tty.Table.column "URL" ]

let render_overlay_table ~with_status rows =
  let table =
    Tty.Table.of_rows ~header_style:Oi.Style.header
      (overlay_columns ~with_status)
      rows
  in
  Tty.Table.pp Fmt.stdout table

let toolchain_row (e : Oi.Source.Reporepo.entry) =
  let name = toolchain_cli_name e in
  let mode, mode_style = mode_of e in
  let default_span =
    if e.default_toolchain then Tty.Span.styled Oi.Style.accent "yes"
    else Tty.Span.text ""
  in
  [
    Tty.Span.styled Oi.Style.header name;
    Tty.Span.text e.version;
    Tty.Span.styled mode_style mode;
    default_span;
    Tty.Span.text (Stdlib.Option.value ~default:"" e.toolchain_compiler);
  ]

let render_toolchain_table rows =
  let table =
    Tty.Table.of_rows ~header_style:Oi.Style.header
      [
        Tty.Table.column "NAME";
        Tty.Table.column "VERSION";
        Tty.Table.column "MODE";
        Tty.Table.column "DEFAULT";
        Tty.Table.column "COMPILER";
      ]
      rows
  in
  Tty.Table.pp Fmt.stdout table

let ref_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"REF"
        ~doc:
          "Track a specific branch or tag instead of the repository's default \
           branch. The ref name is remembered in the reporepo so that later \
           $(b,oi repo bump) invocations keep following the same branch rather \
           than silently falling back to the default. For example, \
           $(b,--ref=relocatable) is how you pin $(b,dra27/opam-repository), \
           whose payload lives on the $(b,relocatable) branch."
        [ "ref"; "r" ])

let toolchain_repo_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"NAME"
        ~doc:
          "Tag this overlay with a builtin toolchain (e.g. $(b,oxcaml), \
           $(b,ocaml-5.4), $(b,ocaml-5.5)). The choice is recorded as \
           $(b,x-oi-toolchain) in the overlay's opam file and changes how \
           $(b,oi repo bump) computes the auto-injected base depends: instead \
           of pinning the default $(b,relocatable)/$(b,default) pair, it pins \
           whatever overlays the named toolchain itself layers under. Pass \
           $(b,--toolchain=oxcaml) to mark an overlay as oxcaml-targeted and \
           lock it against $(b,default) only."
        [ "toolchain" ])

(* Look up a builtin toolchain's [depends] for use as [~base_handles]
   into [Reporepo.add]/[bump]. Errors loudly when the user passes a
   handle that isn't a known builtin so they don't silently get the
   default base set. *)
let base_handles_of_toolchain = function
  | None -> None
  | Some t -> (
      match Oi.Toolchain.depends_of ~handle:t with
      | Some d -> Some d
      | None ->
          Oi.Error.config_error
            "unknown toolchain %S — known builtins listed by 'oi config'" t)

module Ls = struct
  let cmd =
    let run () reporepo reporepo_url no_check =
      Harness.run @@ fun env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url;
      match Oi.Source.Reporepo.load ~path:reporepo with
      | [] -> Fmt.pr "Reporepo %s is empty.@." reporepo
      | entries ->
          Fmt.pr "Reporepo: %s@.@." reporepo;
          let latest_entries =
            entries
            |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
            |> List.sort_uniq String.compare
            |> List.filter_map (fun handle ->
                Oi.Source.Reporepo.latest entries ~handle)
          in
          (* Split toolchain definitions out of the overlay table — they
           don't have a URL of their own and their reporepo handle
           ([toolchain-*]) is an implementation detail; users address
           them by their CLI name (e.g. [ocaml-5.4]). Within overlays,
           base entries (no [x-oi-toolchain] tag, e.g. [default],
           [relocatable]) sort first since they're not user-supplied. *)
          let toolchains, overlays =
            List.partition
              (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain_name <> None)
              latest_entries
          in
          let overlays =
            let untagged, tagged =
              List.partition
                (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain = None)
                overlays
            in
            untagged @ tagged
          in
          let pp_subtitle ppf s = Oi.Style.dim_string ppf s in
          if overlays <> [] then begin
            Fmt.pr "%a@." Oi.Style.header_string "OVERLAYS";
            Fmt.pr "%a@.@." pp_subtitle
              "Curated opam packages pinned to git commits. Use as \
               --with-repo=@HANDLE, --with=@HANDLE/PKG, or 'x-repos:' in \
               *.opam.";
            if no_check then
              render_overlay_table ~with_status:false
                (List.map overlay_row overlays)
            else begin
              (* Parallel [git ls-remote] per entry. Four at a time keeps
               the pipe/fd footprint small without making a 30-entry
               reporepo serial. Failures downgrade to [Unknown] — a
               flaky network must not make [oi repo list] unusable. *)
              let indexed = List.mapi (fun i e -> (i, e)) overlays in
              let statuses = Array.make (List.length indexed) Unknown in
              Eio.Fiber.List.iter ~max_fibers:4
                (fun (i, e) -> statuses.(i) <- check_upstream ~sys e)
                indexed;
              render_overlay_table ~with_status:true
                (List.mapi
                   (fun i e -> overlay_row ~status:statuses.(i) e)
                   overlays)
            end
          end;
          if toolchains <> [] then begin
            if overlays <> [] then Fmt.pr "@.";
            Fmt.pr "%a@." Oi.Style.header_string "TOOLCHAINS";
            Fmt.pr "%a@.@." pp_subtitle
              "Compiler bundles. Select with --toolchain=NAME; the DEFAULT \
               entry is used otherwise.";
            render_toolchain_table (List.map toolchain_row toolchains)
          end
    in
    let no_check =
      Arg.(
        value & flag
        & info
            ~doc:
              "Skip the per-entry $(b,git ls-remote) check and print the \
               reporepo contents without contacting the network."
            [ "no-check" ])
    in
    let info =
      Cmd.info "list" ~doc:"List overlays and toolchains in the reporepo"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Two sections: $(b,OVERLAYS) (base repos and user overlays, with \
               upstream status) and $(b,TOOLCHAINS) (toolchain definitions).";
            `S "OVERLAYS";
            `P
              "An overlay is a curated set of opam packages pinned to a \
               specific git commit. Pull one into a solve with \
               $(b,--with-repo=@HANDLE) or $(b,--with=@HANDLE/PKG), or declare \
               $(b,x-repos: [\"@HANDLE\"]) in a project's $(b,*.opam) to apply \
               it automatically.";
            `P
              "Base repos ($(b,default), $(b,relocatable)) are pulled in by \
               every solve and listed first. User overlays follow in handle \
               order. Add new entries with $(b,oi repo add); fast-forward \
               stale ones with $(b,oi repo bump HANDLE).";
            `P
              "Columns: $(b,HANDLE), $(b,VERSION), pinned $(b,COMMIT), \
               $(b,TOOLCHAIN) target (from $(b,x-oi-toolchain), $(b,—) for \
               base repos), $(b,STATUS), source $(b,URL).";
            `P "Status is computed by $(b,git ls-remote) (four in parallel):";
            `I ("$(b,up-to-date)", "Pinned commit matches the upstream branch.");
            `I ("$(b,stale)", "Upstream has moved past the pin.");
            `I ("$(b,unreachable)", "Remote could not be contacted.");
            `P "$(b,--no-check) skips the check and drops the column.";
            `S "TOOLCHAINS";
            `P
              "A toolchain is a compiler plus its supporting overlays. Pass \
               $(b,--toolchain=NAME) to any oi command to select one; \
               otherwise the entry flagged $(b,DEFAULT) is used. An overlay \
               tagged $(b,x-oi-toolchain: NAME) implies that toolchain when \
               pulled into a solve.";
            `P
              "$(b,relocatable) toolchains build into $(b,_oi/prefix/) like \
               any other package; $(b,fixed-prefix) toolchains install into a \
               shared root under $(b,\\$XDG_CACHE_HOME/oi/toolchains) (visible \
               under $(b,oi config)).";
            `P
              "Columns: $(b,NAME) (the value to pass to $(b,--toolchain)), \
               $(b,VERSION), $(b,MODE), $(b,DEFAULT), and the primary \
               $(b,COMPILER) package.";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ no_check)
end

module Show = struct
  let cmd =
    let run () reporepo reporepo_url handle =
      Harness.run @@ fun env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url;
      let entries = Oi.Source.Reporepo.load ~path:reporepo in
      let matches =
        List.filter
          (fun (e : Oi.Source.Reporepo.entry) -> e.handle = handle)
          entries
        |> List.sort
             (fun
               (a : Oi.Source.Reporepo.entry) (b : Oi.Source.Reporepo.entry) ->
               OpamPackage.Version.compare
                 (OpamPackage.Version.of_string b.version)
                 (OpamPackage.Version.of_string a.version))
      in
      if matches = [] then
        Oi.Error.not_found handle "no overlay %s in reporepo %s" handle reporepo;
      List.iter
        (fun (e : Oi.Source.Reporepo.entry) ->
          Fmt.pr "%s.%s@." e.handle e.version;
          Fmt.pr "  url:    %s@." e.url;
          Fmt.pr "  commit: %s@." e.commit;
          (match e.ref_ with Some r -> Fmt.pr "  ref:    %s@." r | None -> ());
          (match e.depends with
          | [] -> ()
          | ds ->
              Fmt.pr "  depends:@.";
              List.iter
                (fun (h, v) ->
                  match v with
                  | None -> Fmt.pr "    %s@." h
                  | Some ver -> Fmt.pr "    %s = %s@." h ver)
                ds);
          (match e.root_packages with
          | [] -> ()
          | groups ->
              Fmt.pr "  root-packages:@.";
              List.iter
                (fun group ->
                  match group with
                  | [] -> ()
                  | [ p ] -> Fmt.pr "    %s@." p
                  | multi -> Fmt.pr "    [%s]@." (String.concat " " multi))
                groups);
          Fmt.pr "@.")
        matches
    in
    let handle =
      Arg.(
        required
        & pos 0 (some string) None
        & info ~docv:"HANDLE" ~doc:"The overlay handle to inspect." [])
    in
    let info =
      Cmd.info "show"
        ~doc:"Show every version of one overlay, with commits and dependencies"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "$(b,oi repo show) prints the recorded history of a single \
               overlay handle. For each version it lists the git URL and \
               commit it pins, the tracked branch if one was set with \
               $(b,--ref), and the other overlays that version depends on.";
            `P
              "Use this to audit what a particular user's overlay pulls in, \
               and to tell at a glance whether bumping that overlay would drag \
               other overlays along with it.";
          ]
    in
    Cmd.v info
      Term.(const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle)
end

module Add = struct
  let cmd =
    let run () reporepo reporepo_url handle url ref_ toolchain depend_specs
        force =
      Harness.run @@ fun env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url;
      let depends =
        match depend_specs with
        | [] -> None
        | _ -> Some (List.map parse_depend_spec depend_specs)
      in
      let base_handles = base_handles_of_toolchain toolchain in
      let e =
        Oi.Source.Reporepo.add ~fs ~sys ~path:reporepo ~handle ~url ?ref_
          ?toolchain ?base_handles ?depends ~force ()
      in
      Fmt.pr "Added %s.%s@ url=%s@ commit=%s@ at %s@." e.handle e.version e.url
        e.commit e.opam_path;
      if e.depends <> [] then begin
        Fmt.pr "Depends:@.";
        List.iter
          (fun (h, v) ->
            match v with
            | Some ver -> Fmt.pr "  %s = %s@." h ver
            | None -> Fmt.pr "  %s@." h)
          e.depends
      end;
      if e.url <> "" then begin
        Fmt.pr "Materialising v1/%s/ from %s @ %s@." e.handle e.url
          (String.sub e.commit 0 (min 7 (String.length e.commit)));
        let summary =
          Oi.Source.Reporepo.materialise_handle ~fs ~sys ~path:reporepo
            ~handle:e.handle ~url:e.url ~commit:e.commit
        in
        Fmt.pr
          "  %d packages: %d already pinned, %d rewritten, %d marked \
           unavailable@."
          summary.total summary.kept summary.rewrote
          (List.length summary.unavailable)
      end
    in
    let handle =
      Arg.(
        required
        & pos 0 (some string) None
        & info ~docv:"HANDLE"
            ~doc:
              "A short opam-valid name for the overlay, for example $(b,avsm) \
               or $(b,samoht)."
            [])
    in
    let url =
      Arg.(
        required
        & pos 1 (some string) None
        & info ~docv:"URL"
            ~doc:
              "The git URL of the upstream opam-repository to pin under this \
               handle."
            [])
    in
    let force =
      Arg.(
        value & flag
        & info
            ~doc:
              "Write a new $(b,YYYYMMDD.N) entry for $(i,HANDLE) even when the \
               handle already exists. Use this to point an overlay at a \
               different upstream URL without losing history. Older entries \
               stay in place and continue to pin the previous URL, so you can \
               roll back to them if the switch turns out badly."
            [ "force"; "f" ])
    in
    let info =
      Cmd.info "add" ~doc:"Register a new overlay in the reporepo"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Register $(b,HANDLE) in the reporepo, pinned to the current \
               commit on $(b,URL)'s default branch (or $(b,--ref BRANCH)).";
            `P
              "Non-base overlays auto-record dependencies on the current \
               latest $(b,default) and $(b,relocatable) versions, so the new \
               overlay travels with the base set it was built against. \
               $(b,--toolchain=NAME) instead pins the toolchain's own base set \
               (e.g. $(b,oxcaml) → just $(b,default)).";
            `S Manpage.s_examples;
            `Pre
              "  oi repo add default \
               https://github.com/ocaml/opam-repository.git\n\
              \  oi repo add relocatable \
               https://github.com/dra27/opam-repository.git --ref relocatable\n\
              \  oi repo add avsm \
               https://tangled.org/anil.recoil.org/aoah-opam-repo.git";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle $ url
        $ ref_term $ toolchain_repo_term $ depend_term $ force)
end

module Bump = struct
  let bump_one ~fs ~sys ~reporepo ~handle ~url ~ref_ ~toolchain ~depends
      ~default =
    let effective_toolchain =
      match toolchain with
      | Some _ -> toolchain
      | None ->
          let entries = Oi.Source.Reporepo.load ~path:reporepo in
          Stdlib.Option.bind (Oi.Source.Reporepo.latest entries ~handle)
            (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain)
    in
    let base_handles = base_handles_of_toolchain effective_toolchain in
    (* Setting --default on toolchain X requires clearing the flag on
       whichever toolchain currently holds it, otherwise the reporepo
       has two defaults and Source.Reporepo.load refuses to parse. *)
    if default = Some true then begin
      let entries = Oi.Source.Reporepo.load ~path:reporepo in
      match Oi.Source.Reporepo.default_toolchain entries with
      | Some prev when prev.handle <> handle -> begin
          Fmt.pr "Clearing default flag on %s (replaced by %s)@." prev.handle
            handle;
          match
            Oi.Source.Reporepo.bump ~fs ~sys ~path:reporepo ~handle:prev.handle
              ~default:false ()
          with
          | `Bumped b -> Fmt.pr "  -> %s.%s@." b.handle b.version
          | `Unchanged _ -> ()
        end
      | _ -> ()
    end;
    let bumped =
      Oi.Source.Reporepo.bump ~fs ~sys ~path:reporepo ~handle ?url ?ref_
        ?toolchain ?base_handles ?depends ?default ()
    in
    let entry = match bumped with `Bumped e -> e | `Unchanged e -> e in
    (match bumped with
    | `Bumped e ->
        Fmt.pr "Bumped %s to %s@ commit=%s@ at %s@." e.handle e.version e.commit
          e.opam_path;
        if e.default_toolchain then Fmt.pr "  marked as default toolchain@."
    | `Unchanged e ->
        Fmt.pr
          "No change: %s.%s already pins the current upstream commit (%s).@."
          e.handle e.version e.commit);
    if entry.url = "" then ()
    else begin
      Fmt.pr
        "Materialising v1/%s/ from %s @ %s — this resolves every package's url \
         and may take a while.@."
        entry.handle entry.url
        (String.sub entry.commit 0 (min 7 (String.length entry.commit)));
      let summary =
        Oi.Source.Reporepo.materialise_handle ~fs ~sys ~path:reporepo
          ~handle:entry.handle ~url:entry.url ~commit:entry.commit
      in
      Fmt.pr
        "  %d packages: %d already pinned, %d rewritten, %d marked \
         unavailable@."
        summary.total summary.kept summary.rewrote
        (List.length summary.unavailable);
      List.iter
        (fun (pkg, reason) ->
          Fmt.pr "    %a %s: %s@." Oi.Style.warn_string "unavailable" pkg reason)
        summary.unavailable
    end

  let cmd =
    let run () reporepo reporepo_url handle_opt all url ref_ toolchain
        depend_specs default =
      Harness.run @@ fun env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url;
      let depends =
        match depend_specs with
        | [] -> None
        | _ -> Some (List.map parse_depend_spec depend_specs)
      in
      match (all, handle_opt) with
      | false, None ->
          Fmt.epr "oi repo bump: pass HANDLE or --all.@.";
          exit 1
      | true, Some _ ->
          Fmt.epr "oi repo bump: HANDLE and --all are mutually exclusive.@.";
          exit 1
      | true, None ->
          if
            url <> None || ref_ <> None || toolchain <> None
            || depend_specs <> [] || default <> None
          then begin
            Fmt.epr
              "oi repo bump --all: per-entry overrides (--url, --ref, \
               --toolchain, --depend, --default) are not supported with \
               --all.@.";
            exit 1
          end;
          let entries = Oi.Source.Reporepo.load ~path:reporepo in
          let handles =
            entries
            |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
            |> List.sort_uniq String.compare
          in
          List.iter
            (fun h ->
              Fmt.pr "@.%a@." Oi.Style.header_string ("== " ^ h ^ " ==");
              try
                bump_one ~fs ~sys ~reporepo ~handle:h ~url:None ~ref_:None
                  ~toolchain:None ~depends:None ~default:None
              with exn ->
                Fmt.pr "  %a %s: %s@." Oi.Style.error_string "error" h
                  (Printexc.to_string exn))
            handles
      | false, Some handle ->
          bump_one ~fs ~sys ~reporepo ~handle ~url ~ref_ ~toolchain ~depends
            ~default
    in
    let handle =
      Arg.(
        value
        & pos 0 (some string) None
        & info ~docv:"HANDLE" ~doc:"The overlay handle to bump." [])
    in
    let all =
      Arg.(
        value & flag
        & info
            ~doc:
              "Bump every overlay in the reporepo. Per-entry overrides \
               ($(b,--url), $(b,--ref), $(b,--toolchain), $(b,--depend), \
               $(b,--default)) are not allowed in this mode."
            [ "all" ])
    in
    let url =
      Arg.(
        value
        & opt (some string) None
        & info ~docv:"URL"
            ~doc:
              "Override the upstream URL. Defaults to whatever the latest \
               recorded version of the overlay pins."
            [ "url" ])
    in
    let default =
      let set =
        Arg.(
          value & flag
          & info
              ~doc:
                "Mark this toolchain as the default — [oi run] / [oi build] \
                 without [--toolchain] will pick it. Auto-clears any other \
                 default. Only valid on entries with [x-oi-toolchain-name] \
                 set."
              [ "default" ])
      in
      let unset =
        Arg.(
          value & flag
          & info
              ~doc:
                "Clear the default-toolchain flag from this entry. After \
                 clearing, no toolchain is default and [oi run] without \
                 [--toolchain] will hard-error until one is set."
              [ "no-default" ])
      in
      let combine s u =
        match (s, u) with
        | true, true ->
            `Error (false, "--default and --no-default are mutually exclusive")
        | true, false -> `Ok (Some true)
        | false, true -> `Ok (Some false)
        | false, false -> `Ok None
      in
      Term.(ret (const combine $ set $ unset))
    in
    let info =
      Cmd.info "bump" ~doc:"Re-pin an overlay to its current upstream commit"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Re-fetch the upstream commit on $(b,HANDLE)'s tracked branch \
               and record a new $(b,YYYYMMDD.N) meta-entry. Then walk the \
               upstream's packages, sha-pin every URL, and write the result to \
               $(b,<reporepo>/v1/<HANDLE>/). Pass $(b,--all) to bump every \
               overlay.";
            `P
              "Idempotent on the meta-entry: prints $(b,No change) when \
               nothing in commit/URL/ref/depends/flags has moved. The $(b,v1/) \
               tree is rebuilt either way to catch upstream tag drift.";
            `P
              "Non-base overlays auto-relock against the current \
               $(b,default)/$(b,relocatable) (or the toolchain's own base set \
               when $(b,x-oi-toolchain) is set); $(b,--depend) overrides.";
            `P
              "$(b,--default) flips $(b,x-oi-default-toolchain) on \
               toolchain-defining entries; setting it on one auto-clears any \
               other.";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle $ all
        $ url $ ref_term $ toolchain_repo_term $ depend_term $ default)
end

module Set_roots = struct
  let cmd =
    (* Parse a PKG token: a comma-separated list becomes a multi-package
     solve group; a bare name becomes a singleton group. Empty tokens
     between commas are dropped. *)
    let parse_group token =
      String.split_on_char ',' token
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    in
    let run () reporepo reporepo_url handle pkgs =
      Harness.run @@ fun env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url;
      let groups =
        List.filter_map
          (fun t -> match parse_group t with [] -> None | g -> Some g)
          pkgs
      in
      match
        Oi.Source.Reporepo.bump ~fs ~sys ~path:reporepo ~handle
          ~root_packages:groups ()
      with
      | `Bumped e ->
          Fmt.pr "Bumped %s to %s (root-packages: %d entr%s)@." e.handle
            e.version
            (List.length e.root_packages)
            (if List.length e.root_packages = 1 then "y" else "ies")
      | `Unchanged e ->
          Fmt.pr "No change: %s.%s already has that root-packages list.@."
            e.handle e.version
    in
    let handle =
      Arg.(
        required
        & pos 0 (some string) None
        & info ~docv:"HANDLE" ~doc:"The overlay to update." [])
    in
    let pkgs =
      Arg.(
        value & pos_right 0 string []
        & info ~docv:"PKG"
            ~doc:
              "Root package groups for the overlay. Each argument becomes one \
               solve group that $(b,oi build --all) iterates. A bare name is a \
               single-package solve; a comma-separated list is a multi-package \
               group that solves together (used for compiler variants, e.g. \
               $(b,ocaml-option-flambda,ocaml-option-static,ocaml)). No \
               $(b,PKG) clears the list."
            [])
    in
    let info =
      Cmd.info "set-roots"
        ~doc:"Record which packages should be pre-built for an overlay"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Writes $(b,x-root-packages: [...]) on a new bumped version of \
               $(b,HANDLE). $(b,oi build --all) iterates each handle's root \
               groups: a single-package group builds as one $(b,@HANDLE/PKG); \
               a comma-separated group solves together so the resulting layers \
               capture a specific variant.";
            `P
              "Passing zero $(b,PKG) arguments clears the list. The new \
               version is stamped $(b,YYYYMMDD.N) in exactly the same way as \
               $(b,oi repo bump). The previous entry is kept as history so \
               that you can roll back to it.";
            `S Manpage.s_examples;
            `P "Record three independent root packages:";
            `Pre "  oi repo set-roots relocatable dune utop merlin";
            `P "Record a compiler variant alongside plain packages:";
            `Pre
              "  oi repo set-roots relocatable \
               ocaml-option-flambda,ocaml-option-static,ocaml dune utop";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle
        $ pkgs)
end

module Remove = struct
  let cmd =
    let run () reporepo reporepo_url handle_spec =
      Harness.run @@ fun env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url;
      let handle, version = parse_handle_version handle_spec in
      Oi.Source.Reporepo.remove ~fs ~path:reporepo ~handle ?version ();
      Fmt.pr "Removed %s%s from %s@." handle
        (match version with None -> " (all versions)" | Some v -> "." ^ v)
        reporepo
    in
    let handle_spec =
      Arg.(
        required
        & pos 0 (some string) None
        & info ~docv:"HANDLE[=VERSION]"
            ~doc:
              "The overlay to remove. Without $(b,=VERSION) every recorded \
               version of the handle is deleted."
            [])
    in
    let info =
      Cmd.info "remove" ~doc:"Delete an overlay from the reporepo"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "$(b,oi repo remove) deletes an overlay entry from the reporepo. \
               With $(b,HANDLE=VERSION) only the named version is removed. \
               With a bare $(b,HANDLE) every recorded version of that handle \
               is removed.";
            `P
              "Only the reporepo is edited; the upstream git repositories are \
               never touched. Any overlay bundles that have already been \
               cloned under the data directory are also left alone, so \
               re-adding the handle later does not force another full clone. \
               Run $(b,oi clean --repos) if you want the on-disk clones \
               removed too.";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle_spec)
end

module Push = struct
  let cmd =
    let run () reporepo reporepo_url push_url =
      Harness.run @@ fun env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url;
      Fmt.pr "%a %s@." Oi.Style.header_string "reporepo:" reporepo;
      (match push_url with
      | None -> ()
      | Some u ->
          Oi.Source.Reporepo.set_push_url ~sys ~path:reporepo u;
          Fmt.pr "%a push URL of origin set to %s@." Oi.Style.ok_string "ok" u);
      let on_step_start n title =
        Fmt.pr "@.%a %s@." Oi.Style.header_string (Fmt.str "[%d/3]" n) title
      in
      let outcome =
        Oi.Source.Reporepo.push ~on_step_start ~sys ~path:reporepo ()
      in
      Fmt.pr "@.%a@." Oi.Style.header_string "summary:";
      List.iter
        (function
          | Oi.Source.Reporepo.Step_commit { files = [] } ->
              Fmt.pr "  commit: %a (working tree clean)@." Oi.Style.dim_string
                "skipped"
          | Oi.Source.Reporepo.Step_commit { files } ->
              Fmt.pr "  commit: %a (%d file(s))@." Oi.Style.ok_string "ok"
                (List.length files);
              List.iter (fun f -> Fmt.pr "    %s@." f) files
          | Oi.Source.Reporepo.Step_pull { commits = 0 } ->
              Fmt.pr "  pull:   %a (already up to date)@." Oi.Style.dim_string
                "skipped"
          | Oi.Source.Reporepo.Step_pull { commits } ->
              Fmt.pr "  pull:   %a (%d new upstream commit(s))@."
                Oi.Style.ok_string "ok" commits
          | Oi.Source.Reporepo.Step_push { commits = 0 } ->
              Fmt.pr "  push:   %a (nothing to push)@." Oi.Style.dim_string
                "skipped"
          | Oi.Source.Reporepo.Step_push { commits } ->
              Fmt.pr "  push:   %a (%d local commit(s) sent)@."
                Oi.Style.ok_string "ok" commits)
        outcome
    in
    let push_url =
      Arg.(
        value
        & opt (some string) None
        & info [ "push-url" ] ~docv:"URL"
            ~doc:
              "Persistently set $(b,origin)'s push URL on the local reporepo \
               checkout via $(b,git remote set-url --push origin URL), and \
               then push. This is useful when the clone URL is read-only HTTPS \
               but you push over SSH. The fetch URL is left alone, so \
               subsequent $(b,oi repo) commands keep pulling from the original \
               location.")
    in
    let info =
      Cmd.info "push"
        ~doc:"Pull, commit local edits, and push the reporepo to its remote"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "$(b,oi repo push) performs a three-step synchronisation of the \
               reporepo working copy. First, it stages and auto-commits any \
               uncommitted changes, so that edits made by $(b,oi repo bump) \
               and its siblings are captured. Second, it runs $(b,git pull \
               --rebase) to bring in upstream history. Third, it runs $(b,git \
               push) if the local branch is now ahead of its upstream tracking \
               branch. The command is idempotent: a run against a clean, \
               up-to-date reporepo does nothing.";
            `P
              "Authentication uses the system $(b,git) configuration. Whatever \
               credentials work for $(b,git push) inside the reporepo \
               directory work here too. $(b,oi) shells out to $(b,git) and \
               never handles credentials itself.";
            `P
              "Pass $(b,--push-url URL) to switch the push remote on the local \
               checkout. This is the flag to reach for when the clone URL is \
               read-only HTTPS but you have SSH push access. The change is \
               persistent: $(b,oi) edits $(b,.git/config) once, and subsequent \
               $(b,oi repo push) runs reuse the new URL.";
            `S Manpage.s_examples;
            `P "Bump an overlay and publish the new pin in one shot:";
            `Pre "  oi repo bump avsm && oi repo push";
            `P "Switch the reporepo's push URL to SSH, then push:";
            `Pre
              "  oi repo push --push-url \
               git@tangled.org:anil.recoil.org/reporepo.git";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ push_url)
end

(* -- repo lint ----------------------------------------------------------- *)

(* Shape and integrity checks for a reporepo on disk. The contract every
   command's [Pipeline.resolve_toolchain] depends on: exactly one
   default toolchain, every [x-oi-toolchain] reference resolves, every
   toolchain definition carries the fields the resolver and Toolchain
   pipeline read. Loading a reporepo through {!Reporepo.load} already
   enforces some invariants ("at most one default", well-formed opam
   files); [oi repo lint] surfaces the rest as a precommit-grade check. *)
module Lint = struct
  type problem = { handle : string; version : string; msg : string }

  let collect_problems (entries : Oi.Source.Reporepo.entry list) : problem list
      =
    let problems = ref [] in
    let add ~handle ~version fmt =
      Fmt.kstr
        (fun msg -> problems := { handle; version; msg } :: !problems)
        fmt
    in
    let toolchain_names =
      List.filter_map
        (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain_name)
        entries
      |> List.sort_uniq String.compare
    in
    let known_toolchain n = List.mem n toolchain_names in
    List.iter
      (fun (e : Oi.Source.Reporepo.entry) ->
        let here fmt = add ~handle:e.handle ~version:e.version fmt in
        if e.handle = "" then here "empty handle";
        if e.version = "" then here "empty version";
        (match e.toolchain_name with
        | None ->
            if e.relocatable <> None then
              here
                "[%s] is set but [%s] is not — only toolchain definitions take \
                 this field"
                Oi.Keys.relocatable Oi.Keys.toolchain_name;
            if e.toolchain_roots <> [] then
              here
                "[%s] is set but [%s] is not — only toolchain definitions take \
                 this field"
                Oi.Keys.toolchain_roots Oi.Keys.toolchain_name
        | Some name ->
            if name = "" then here "[%s] is empty" Oi.Keys.toolchain_name;
            if e.relocatable = None then
              here "toolchain %S missing [%s] (must be true or false)" name
                Oi.Keys.relocatable;
            if e.toolchain_roots = [] then
              here "toolchain %S has empty [%s]" name Oi.Keys.toolchain_roots;
            if e.toolchain_compiler = None then
              here
                "toolchain %S missing [%s] (e.g. \"ocaml-base-compiler.5.4.1\")"
                name Oi.Keys.toolchain_compiler);
        match e.toolchain with
        | None -> ()
        | Some t ->
            if not (known_toolchain t) then
              here
                "[%s: %s] does not match any [%s] in this reporepo (known: %s)"
                Oi.Keys.toolchain t Oi.Keys.toolchain_name
                (if toolchain_names = [] then "none"
                 else String.concat ", " toolchain_names))
      entries;
    let defaults =
      List.filter
        (fun (e : Oi.Source.Reporepo.entry) -> e.default_toolchain)
        entries
    in
    let here fmt = add ~handle:"(reporepo)" ~version:"" fmt in
    (match defaults with
    | [ _ ] -> () (* exactly one default — fine *)
    | [] ->
        let hint =
          if toolchain_names = [] then "no toolchain definitions found"
          else
            Fmt.str "mark one with: oi repo bump <handle> --default. Known: %s"
              (String.concat ", " toolchain_names)
        in
        here
          "no entry has [%s: true] — %s. Without a default, [oi run] / [oi \
           sync] / etc. hard-error when the user omits [--toolchain]."
          Oi.Keys.default_toolchain hint
    | many ->
        let labels =
          List.map
            (fun (e : Oi.Source.Reporepo.entry) ->
              Fmt.str "%s.%s" e.handle e.version)
            many
        in
        here
          "multiple entries flagged [%s: true]: %s — only one toolchain handle \
           may be the default. Clear the flag on stale versions and on the \
           losing handle."
          Oi.Keys.default_toolchain
          (String.concat ", " labels));
    List.rev !problems

  let cmd =
    let run () reporepo reporepo_url =
      Harness.run @@ fun env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url;
      let entries = Oi.Source.Reporepo.load ~path:reporepo in
      let problems = collect_problems entries in
      if problems = [] then begin
        Fmt.pr "%a %d entries, no problems found.@." Oi.Style.ok_string "OK:"
          (List.length entries);
        exit 0
      end
      else begin
        List.iter
          (fun { handle; version; msg } ->
            let where =
              if version = "" then handle else Fmt.str "%s.%s" handle version
            in
            Fmt.pr "%a %s: %s@." Oi.Style.error_string "error:" where msg)
          problems;
        Fmt.pr "@.%d problem(s) in %s@." (List.length problems) reporepo;
        exit 1
      end
    in
    let info =
      Cmd.info "lint" ~doc:"Validate a reporepo's well-formedness"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "$(b,oi repo lint) walks every entry in the reporepo and reports \
               any violations of the contract the rest of $(b,oi) relies on:";
            `I
              ( "Default toolchain.",
                "Exactly one entry must carry $(b,x-oi-default-toolchain: \
                 true). Without one, every solving command that omits \
                 $(b,--toolchain) hard-errors." );
            `I
              ( "Toolchain references.",
                "Every entry's $(b,x-oi-toolchain: NAME) must resolve to some \
                 other entry's $(b,x-oi-toolchain-name)." );
            `I
              ( "Toolchain definitions.",
                "Entries with $(b,x-oi-toolchain-name) set must also declare \
                 $(b,x-oi-relocatable), $(b,x-oi-toolchain-compiler), and a \
                 non-empty $(b,x-oi-toolchain-roots)." );
            `I
              ( "Field discipline.",
                "Toolchain-only fields ($(b,x-oi-relocatable), \
                 $(b,x-oi-toolchain-roots)) on an entry that does not also set \
                 $(b,x-oi-toolchain-name) are flagged as misplaced." );
            `P
              "Exits non-zero on any failure, suitable for a pre-commit hook \
               or CI gate.";
          ]
    in
    Cmd.v info Term.(const run $ Terms.log $ reporepo_term $ reporepo_url_term)
end

let cmd =
  let info =
    Cmd.info "repo"
      ~doc:"Manage the directory of package-source bundles you pull from"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "A $(i,reporepo) is a directory of pinned opam-repository commits. \
             Each entry ($(i,handle)) names somebody's package set and pins it \
             to a git commit. The reporepo also defines the toolchains \
             $(b,--toolchain=NAME) accepts (entries with \
             $(b,x-oi-toolchain-name)).";
          `P
            "Handles are short aliases. $(b,oi run @avsm/irmin) takes \
             $(b,irmin) from avsm's overlay; $(b,oi run --with-repo=avsm) \
             pulls the whole overlay into the solve. In an opam file, \
             $(b,x-repos: [\"@avsm\"]) does the same automatically; the field \
             also accepts raw URLs as an unpinned escape hatch.";
          `P
            "On a new machine, the first $(b,oi repo) command auto-clones the \
             upstream reporepo. After that, the working copy is yours to edit, \
             commit, and push. Typical workflow: $(b,oi repo bump) to pick up \
             upstream commits, then $(b,oi repo push) to share.";
          `P
            "$(b,oi repo bump) is idempotent — prints $(b,No change) when the \
             upstream commit already matches, so it's safe under cron or a \
             pre-commit hook.";
          `S "FILES";
          `I
            ( "$(b,\\$OI_REPOREPO) (default: $(b,\\$OI_DATA_DIR/reporepo))",
              "Local git working copy. First $(b,oi repo) subcommand runs \
               $(b,git clone \\$OI_REPOREPO_URL \\$OI_REPOREPO). $(b,cd) in to \
               edit by hand." );
          `S "EXAMPLE WORKFLOW";
          `Pre
            "  oi repo list                 # auto-clones on first use\n\
            \  oi repo add h URL            # pin somebody's overlay\n\
            \  oi run @h/some-tool          # use it\n\
            \  oi repo bump h               # pick up upstream commits\n\
            \  oi repo push                 # commit + push the bumps";
        ]
  in
  Cmd.group info
    [
      Ls.cmd;
      Show.cmd;
      Add.cmd;
      Bump.cmd;
      Set_roots.cmd;
      Remove.cmd;
      Push.cmd;
      Lint.cmd;
    ]
