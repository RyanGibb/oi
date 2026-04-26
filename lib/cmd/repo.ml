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
let toolchain_width (e : Oi.Source.Reporepo.entry) =
  match e.toolchain with Some t -> String.length t | None -> 1

(* Print [pp x] to [Fmt.stdout] (where [Fmt_tty.setup_std_outputs] wired
   up the ANSI renderer) and right-pad with [width - visible_chars]
   spaces. [visible_chars] is the visible width of the rendered cell;
   we pass it explicitly so callers don't need to count characters in
   the styled output (ANSI escapes don't count). *)
let pp_padded_to ~width ~visible pp x =
  Fmt.pr "%a%s" pp x (String.make (max 0 (width - visible)) ' ')

let pp_handle ppf (e : Oi.Source.Reporepo.entry) =
  if e.toolchain_name <> None then
    Fmt.(styled `Bold (styled `Cyan string)) ppf e.handle
  else Fmt.(styled `Bold string) ppf e.handle

let pp_toolchain_target ppf (e : Oi.Source.Reporepo.entry) =
  match e.toolchain with
  | Some t -> Fmt.(styled `Cyan string) ppf t
  | None -> Fmt.(styled `Faint string) ppf "—"

let pp_commit ppf commit =
  let short =
    if commit = "" then ""
    else String.sub commit 0 (min 7 (String.length commit))
  in
  Fmt.(styled `Faint string) ppf short

let print_entry_oneline ~tc_w (e : Oi.Source.Reporepo.entry) =
  pp_padded_to ~width:24 ~visible:(String.length e.handle) pp_handle e;
  Fmt.pr "  %-16s  " e.version;
  pp_padded_to ~width:8
    ~visible:(min 7 (String.length e.commit))
    pp_commit e.commit;
  Fmt.pr "  ";
  pp_padded_to ~width:tc_w ~visible:(toolchain_width e) pp_toolchain_target e;
  Fmt.pr "  %a@." Fmt.(styled `Faint string) e.url

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

(* Print the status tag plus pad to a fixed visible width. Returns
   the visible width consumed so callers can pad without re-counting
   ANSI escapes. *)
let pp_status_tag ppf status =
  match status with
  | Fresh -> Fmt.(styled `Green string) ppf "up-to-date"
  | Unknown -> Fmt.(styled `Yellow string) ppf "unreachable"
  | Definition_only -> Fmt.(styled `Cyan string) ppf "toolchain"
  | Stale tip ->
      Fmt.pf ppf "%a %a"
        Fmt.(styled `Bold (styled `Red string))
        "stale"
        Fmt.(styled `Faint string)
        (Fmt.str "(%s)" (short_sha tip))

let status_visible_width = function
  | Fresh -> String.length "up-to-date"
  | Unknown -> String.length "unreachable"
  | Definition_only -> String.length "toolchain"
  | Stale tip -> String.length "stale " + String.length (short_sha tip) + 2

let print_entry_with_upstream ~tc_w (e : Oi.Source.Reporepo.entry) status =
  pp_padded_to ~width:24 ~visible:(String.length e.handle) pp_handle e;
  Fmt.pr "  %-16s  " e.version;
  pp_padded_to ~width:8
    ~visible:(min 7 (String.length e.commit))
    pp_commit e.commit;
  Fmt.pr "  ";
  pp_padded_to ~width:tc_w ~visible:(toolchain_width e) pp_toolchain_target e;
  Fmt.pr "  ";
  pp_padded_to ~width:28
    ~visible:(status_visible_width status)
    pp_status_tag status;
  Fmt.pr "  %a@." Fmt.(styled `Faint string) e.url


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
        let tc_w =
          List.fold_left
            (fun w e -> max w (toolchain_width e))
            (String.length "toolchain")
            latest_entries
        in
        if no_check then List.iter (print_entry_oneline ~tc_w) latest_entries
        else begin
          (* Parallel [git ls-remote] per entry. Four at a time keeps
             the pipe/fd footprint small without making a 30-entry
             reporepo serial. Failures downgrade to [Unknown] — a
             flaky network must not make [oi repo list] unusable. *)
          let indexed = List.mapi (fun i e -> (i, e)) latest_entries in
          let statuses = Array.make (List.length indexed) Unknown in
          Eio.Fiber.List.iter ~max_fibers:4
            (fun (i, e) -> statuses.(i) <- check_upstream ~sys e)
            indexed;
          List.iteri
            (fun i e -> print_entry_with_upstream ~tc_w e statuses.(i))
            latest_entries
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
    Cmd.info "list" ~doc:"List overlays registered in the reporepo"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "One line per overlay handle: pinned commit, toolchain target \
             (from $(b,x-oi-toolchain)), upstream-status tag, source URL.";
          `P "Status is computed by $(b,git ls-remote) (four in parallel):";
          `I ("$(b,up-to-date)", "Pinned commit matches the upstream branch.");
          `I ("$(b,stale)", "Upstream has moved past the pin.");
          `I ("$(b,unreachable)", "Remote could not be contacted.");
          `I
            ( "$(b,toolchain)",
              "Definition-only entry (no own URL); composes other overlays via \
               $(b,depends:)." );
          `P
            "$(b,oi repo bump HANDLE) fast-forwards a stale entry. \
             $(b,--no-check) skips the network round trip.";
        ]
  in
  Cmd.v info
    Term.(const run $ Terms.log $ reporepo_term $ reporepo_url_term $ no_check)

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
           (fun (a : Oi.Source.Reporepo.entry) (b : Oi.Source.Reporepo.entry) ->
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
            "$(b,oi repo show) prints the recorded history of a single overlay \
             handle. For each version it lists the git URL and commit it pins, \
             the tracked branch if one was set with $(b,--ref), and the other \
             overlays that version depends on.";
          `P
            "Use this to audit what a particular user's overlay pulls in, and \
             to tell at a glance whether bumping that overlay would drag other \
             overlays along with it.";
        ]
  in
  Cmd.v info
    Term.(const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle)


end

module Add = struct
let cmd =
  let run () reporepo reporepo_url handle url ref_ toolchain depend_specs force
      =
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
    end
  in
  let handle =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HANDLE"
          ~doc:
            "A short opam-valid name for the overlay, for example $(b,avsm) or \
             $(b,samoht)."
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
             different upstream URL without losing history. Older entries stay \
             in place and continue to pin the previous URL, so you can roll \
             back to them if the switch turns out badly."
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
            "Non-base overlays auto-record dependencies on the current latest \
             $(b,default) and $(b,relocatable) versions, so the new overlay \
             travels with the base set it was built against. \
             $(b,--toolchain=NAME) instead pins the toolchain's own base set \
             (e.g. $(b,oxcaml) → just $(b,default)).";
          `S Manpage.s_examples;
          `Pre
            "  oi repo add default https://github.com/ocaml/opam-repository.git\n\
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
let cmd =
  let run () reporepo reporepo_url handle url ref_ toolchain depend_specs =
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
    (* Effective toolchain for [base_handles] resolution: the
       [--toolchain] flag overrides; otherwise inherit from the
       previous entry so a bare [oi repo bump] keeps using the
       toolchain's base set. *)
    let effective_toolchain =
      match toolchain with
      | Some _ -> toolchain
      | None ->
          let entries = Oi.Source.Reporepo.load ~path:reporepo in
          Stdlib.Option.bind (Oi.Source.Reporepo.latest entries ~handle)
            (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain)
    in
    let base_handles = base_handles_of_toolchain effective_toolchain in
    match
      Oi.Source.Reporepo.bump ~fs ~sys ~path:reporepo ~handle ?url ?ref_
        ?toolchain ?base_handles ?depends ()
    with
    | `Bumped e ->
        Fmt.pr "Bumped %s to %s@ commit=%s@ at %s@." e.handle e.version e.commit
          e.opam_path
    | `Unchanged e ->
        Fmt.pr
          "No change: %s.%s already pins the current upstream commit (%s).@."
          e.handle e.version e.commit
  in
  let handle =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HANDLE" ~doc:"The overlay handle to bump." [])
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
  let info =
    Cmd.info "bump" ~doc:"Bring an overlay up to the latest upstream commit"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Re-fetch the upstream commit on $(b,HANDLE)'s tracked branch and \
             record it as a new $(b,YYYYMMDD.N) entry. Old entries stay in \
             place — the reporepo keeps a git-like timeline you can roll back \
             to.";
          `P
            "Idempotent: prints $(b,No change) when the upstream commit, URL, \
             branch, toolchain tag, and deps still match the previous entry. \
             Safe to run from cron or a pre-commit hook.";
          `P
            "Non-base overlays also re-lock against the current latest \
             $(b,default)/$(b,relocatable) on each bump (or, when the overlay \
             declares $(b,x-oi-toolchain), against that toolchain's own base \
             set). $(b,--depend) overrides the auto-injected pins.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle $ url
      $ ref_term $ toolchain_repo_term $ depend_term)

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
        Fmt.pr "Bumped %s to %s (root-packages: %d entr%s)@." e.handle e.version
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
            "Package specifications to record as the overlay's root packages. \
             Each argument becomes one solve group that $(b,oi registry build \
             --all) will iterate over. A bare package name becomes a \
             single-package solve; a comma-separated list becomes a \
             multi-package group that solves together, which is how you \
             capture a specific compiler variant. For example, \
             $(b,ocaml-option-flambda,ocaml-option-static,ocaml) forces the \
             solver to pick an $(b,ocaml) version compatible with both options \
             at once. Passing no $(b,PKG) arguments clears the list."
          [])
  in
  let info =
    Cmd.info "set-roots"
      ~doc:"Record which packages should be pre-built for an overlay"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi repo set-roots) writes an $(b,x-root-packages: [...]) \
             field on a new bumped version of $(b,HANDLE). The recorded list \
             drives $(b,oi registry build --all), which walks every overlay in \
             the reporepo and builds each handle's root groups. A \
             single-package group solves and builds as one $(b,@HANDLE/PKG); a \
             multi-package group (written comma-separated on the command line) \
             solves together, so that the resulting layers capture a \
             particular variant.";
          `P
            "Passing zero $(b,PKG) arguments clears the list. The new version \
             is stamped $(b,YYYYMMDD.N) in exactly the same way as $(b,oi repo \
             bump). The previous entry is kept as history so that you can roll \
             back to it.";
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
      const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle $ pkgs)

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
             With $(b,HANDLE=VERSION) only the named version is removed. With \
             a bare $(b,HANDLE) every recorded version of that handle is \
             removed.";
          `P
            "Only the reporepo is edited; the upstream git repositories are \
             never touched. Any overlay bundles that have already been cloned \
             under the data directory are also left alone, so re-adding the \
             handle later does not force another full clone. Run $(b,oi clean \
             --repos) if you want the on-disk clones removed too.";
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
    Fmt.pr "%a %s@." Fmt.(styled `Bold string) "reporepo:" reporepo;
    (match push_url with
    | None -> ()
    | Some u ->
        Oi.Source.Reporepo.set_push_url ~sys ~path:reporepo u;
        Fmt.pr "%a push URL of origin set to %s@."
          Fmt.(styled `Green string)
          "ok" u);
    let on_step_start n title =
      Fmt.pr "@.%a %s@." Fmt.(styled `Bold string) (Fmt.str "[%d/3]" n) title
    in
    let outcome =
      Oi.Source.Reporepo.push ~on_step_start ~sys ~path:reporepo ()
    in
    Fmt.pr "@.%a@." Fmt.(styled `Bold string) "summary:";
    List.iter
      (function
        | Oi.Source.Reporepo.Step_commit { files = [] } ->
            Fmt.pr "  commit: %a (working tree clean)@."
              Fmt.(styled `Faint string)
              "skipped"
        | Oi.Source.Reporepo.Step_commit { files } ->
            Fmt.pr "  commit: %a (%d file(s))@."
              Fmt.(styled `Green string)
              "ok" (List.length files);
            List.iter (fun f -> Fmt.pr "    %s@." f) files
        | Oi.Source.Reporepo.Step_pull { commits = 0 } ->
            Fmt.pr "  pull:   %a (already up to date)@."
              Fmt.(styled `Faint string)
              "skipped"
        | Oi.Source.Reporepo.Step_pull { commits } ->
            Fmt.pr "  pull:   %a (%d new upstream commit(s))@."
              Fmt.(styled `Green string)
              "ok" commits
        | Oi.Source.Reporepo.Step_push { commits = 0 } ->
            Fmt.pr "  push:   %a (nothing to push)@."
              Fmt.(styled `Faint string)
              "skipped"
        | Oi.Source.Reporepo.Step_push { commits } ->
            Fmt.pr "  push:   %a (%d local commit(s) sent)@."
              Fmt.(styled `Green string)
              "ok" commits)
      outcome
  in
  let push_url =
    Arg.(
      value
      & opt (some string) None
      & info [ "push-url" ] ~docv:"URL"
          ~doc:
            "Persistently set $(b,origin)'s push URL on the local reporepo \
             checkout via $(b,git remote set-url --push origin URL), and then \
             push. This is useful when the clone URL is read-only HTTPS but \
             you push over SSH. The fetch URL is left alone, so subsequent \
             $(b,oi repo) commands keep pulling from the original location.")
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
             uncommitted changes, so that edits made by $(b,oi repo bump) and \
             its siblings are captured. Second, it runs $(b,git pull --rebase) \
             to bring in upstream history. Third, it runs $(b,git push) if the \
             local branch is now ahead of its upstream tracking branch. The \
             command is idempotent: a run against a clean, up-to-date reporepo \
             does nothing.";
          `P
            "Authentication uses the system $(b,git) configuration. Whatever \
             credentials work for $(b,git push) inside the reporepo directory \
             work here too. $(b,oi) shells out to $(b,git) and never handles \
             credentials itself.";
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
    Term.(const run $ Terms.log $ reporepo_term $ reporepo_url_term $ push_url)

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
    ]
