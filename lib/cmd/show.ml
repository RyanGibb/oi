open Cmdliner

let ( / ) = Filename.concat

(* -- plan ---------------------------------------------------------------- *)

(* Rendering helpers for [oi show]'s default succinct page. *)

(* Format the top-block "Target:" line. For a CLI-supplied target we
   print it verbatim (e.g. "utop", "@avsm/tangled"); for the
   local-project case we show the package name (or count when there
   is more than one *.opam file). *)
let show_target_label ~targets ~local_packages =
  match targets with
  | [] -> (
      match local_packages with
      | [] -> "local project"
      | [ p ] -> Fmt.str "local project (%s)" p
      | many -> Fmt.str "local project (%d packages)" (List.length many))
  | _ -> String.concat " " targets

(* The overlay line is only printed when the solve actually pulled from
   an overlay. CLI-supplied [@handle/pkg] shortcuts and project
   [x-repos:] both feed into [with_repos], so we take the first handle
   we see. *)
let show_overlay_label ~with_repos =
  match with_repos with
  | [] -> None
  | h :: _ when not (Target.is_url_like h) -> Some ("@" ^ h)
  | _ -> None

(* Split the elaborated plan's packages into (cached, source) counts. *)
let show_counts (plan : Oi.Plan.t) =
  List.fold_left
    (fun (c, s) (p : Oi.Plan.package_plan) ->
      match p.method_ with
      | Oi.Identity.Binary -> (c + 1, s)
      | Source -> (c, s + 1))
    (0, 0) plan.packages

let opam_pkg_of_package_plan (p : Oi.Plan.package_plan) =
  match OpamPackage.of_string_opt p.pkg with
  | Some op -> op
  | None ->
      OpamPackage.create
        (OpamPackage.Name.of_string p.pkg)
        (OpamPackage.Version.of_string "0")

(* Compute the depexts declared by every package in the plan (both
   cached and source), along with the host installation status. The
   full closure is what the old [oi depexts] reported and is the right
   answer for scripting use ("what would this need from apt if I were
   building from scratch?"). When [--os] is set the host check isn't
   meaningful and we return [None] for the status. *)
let show_depexts ~conf ~packages_dirs ~(plan : Oi.Plan.t) ~os_override =
  let all_pkgs = List.map opam_pkg_of_package_plan plan.packages in
  let entries = Oi.Depexts.compute_for_conf ~conf ~packages_dirs all_pkgs in
  let all =
    List.fold_left
      (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
      OpamSysPkg.Set.empty entries
  in
  let status =
    if os_override <> None then None else Some (Oi.Depexts.status all)
  in
  (all, status)

(* Read every *.opam file in [cwd] directly, for the no-target case
   where we want to surface the project's own metadata. Returns one
   [(pkg, opam)] per file (sorted alphabetically by package name) with
   a placeholder "dev" version, since a project's own opam files are
   typically versionless. Multi-package projects (foo.opam, bar.opam)
   contribute one entry each so the info page shows every package's
   synopsis/license/etc., not just the first file's. *)
let read_local_opams ~cwd =
  let entries = try Sys.readdir cwd |> Array.to_list with _ -> [] in
  entries
  |> List.filter (fun n ->
      Filename.check_suffix n ".opam" && Filename.chop_suffix n ".opam" <> "")
  |> List.sort String.compare
  |> List.filter_map (fun filename ->
      let path = Filename.concat cwd filename in
      let name = Filename.chop_suffix filename ".opam" in
      try
        let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) in
        let pkg =
          OpamPackage.create
            (OpamPackage.Name.of_string name)
            (OpamPackage.Version.of_string "dev")
        in
        Some (pkg, opam)
      with _ -> None)

(* Pick the package whose metadata we'll surface on the default info
   page. A CLI target resolves to its action-plan node. For the
   local-project case we read the project's own first *.opam file
   directly (otherwise we'd show metadata for the first
   dependency, which is misleading). Anything else falls through to
   the first plan node as a last-ditch option. *)
type show_meta_source =
  | From_pkg of Oi.Plan.package_plan
  | From_project_opams of (OpamPackage.t * OpamFile.OPAM.t) list

let show_primary_meta ~(plan : Oi.Plan.t) ~targets ~project_deps ~cwd =
  let find_name name =
    List.find_opt
      (fun (p : Oi.Plan.package_plan) ->
        let opam_pkg = opam_pkg_of_package_plan p in
        OpamPackage.Name.to_string (OpamPackage.name opam_pkg) = name)
      plan.packages
  in
  match targets with
  | first :: _ -> (
      match find_name first with Some p -> Some (From_pkg p) | None -> None)
  | [] -> (
      match read_local_opams ~cwd with
      | _ :: _ as opams -> Some (From_project_opams opams)
      | [] -> (
          match project_deps with
          | first :: _ ->
              Stdlib.Option.map (fun p -> From_pkg p) (find_name first)
          | [] -> None))

(* Collapse a multi-line synopsis to its first line so the info page
   stays tidy. *)
let first_line s =
  match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i

(* Print a single optional metadata field. Skipped silently when the
   value is absent or empty. The label column is fixed at 11
   characters so all rows on the info page line up. *)
let show_meta_line label value =
  match value with
  | "" -> ()
  | v ->
      Fmt.pr "%a %s@," Oi.Style.header_string (Fmt.str "%-11s" (label ^ ":")) v

(* Extract a compact, user-facing snapshot of an opam file's
   descriptive metadata fields for the info page. *)
let show_package_meta (_pkg : OpamPackage.t) (opam : OpamFile.OPAM.t) =
  let synopsis =
    Stdlib.Option.value (OpamFile.OPAM.synopsis opam) ~default:""
    |> String.trim |> first_line
  in
  let license = String.concat ", " (OpamFile.OPAM.license opam) in
  let homepage = String.concat ", " (OpamFile.OPAM.homepage opam) in
  let dev_repo =
    match OpamFile.OPAM.dev_repo opam with
    | None -> ""
    | Some u -> OpamUrl.to_string u
  in
  let maintainer = String.concat ", " (OpamFile.OPAM.maintainer opam) in
  let tags = String.concat ", " (OpamFile.OPAM.tags opam) in
  let description =
    Stdlib.Option.value (OpamFile.OPAM.descr_body opam) ~default:""
    |> String.trim
  in
  (synopsis, license, homepage, dev_repo, maintainer, tags, description)

(* List the binaries that would end up on [$PATH] when this target's
   layer is assembled into a prefix. When the layer is cached locally
   we scan [layers/<os_key>/<hash>/fs/bin] and [fs/sbin] directly;
   that's cheaper than a sqlite query and also works for layers the
   index doesn't cover (fresh builds that haven't been re-indexed
   yet). Returns [[]] for a layer that hasn't been built, for a
   purely library package, or when the fs/ tree is missing. *)
let show_package_binaries ~cache_root ~os_key ~layer_hash =
  let layer_dir = cache_root / "layers" / os_key / layer_hash / "fs" in
  let scan sub =
    let dir = layer_dir / sub in
    if not (Sys.file_exists dir) then []
    else try Sys.readdir dir |> Array.to_list with _ -> []
  in
  let bins = scan "bin" @ scan "sbin" in
  List.sort_uniq String.compare bins

(* Collect the (handle, version, url) tuples the user would want to
   see on the info page: when a toolchain is active, its overlay
   chain (e.g. [oxcaml + default]); otherwise the default base
   chain (relocatable / default). Plus any overlays named
   explicitly in [with_repos], in that order, deduplicated by
   handle. *)
let show_repositories ?toolchain ~with_repos () =
  let entries =
    try Oi.Source.Reporepo.load ~path:(Terms.reporepo_path ())
    with Oi.Error.E _ -> []
  in
  let base_handles =
    match toolchain with
    | Some (info : Oi.Toolchain.info) -> info.handle :: info.dep_handles
    | None ->
        Oi.Source.Reporepo.base_entries ()
        |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
  in
  let extra_handles =
    List.filter (fun h -> not (Target.is_url_like h)) with_repos
  in
  let all = base_handles @ extra_handles |> List.sort_uniq String.compare in
  let ordered =
    let seen = Hashtbl.create 4 in
    let push acc h =
      if List.mem h all && not (Hashtbl.mem seen h) then begin
        Hashtbl.add seen h ();
        h :: acc
      end
      else acc
    in
    let acc = List.fold_left push [] base_handles in
    let acc = List.fold_left push acc extra_handles in
    List.rev acc
  in
  List.filter_map
    (fun h ->
      match Oi.Source.Reporepo.latest entries ~handle:h with
      | Some (e : Oi.Source.Reporepo.entry) ->
          let url = if e.commit = "" then e.url else e.url ^ "#" ^ e.commit in
          Some (h, e.version, url)
      | None -> (
          (* Toolchain overlay: not in reporepo, but we know its URL. *)
          match Oi.Toolchain.url_of ~handle:h with
          | Some url -> Some (h, "builtin", url)
          | None -> None))
    ordered

(* Print one indented metadata line. Mirrors [show_meta_line]'s 11-char
   label column but at an arbitrary indent so per-package blocks line
   up under their package-name header. *)
let show_indented_meta_line ~indent label = function
  | "" -> ()
  | v ->
      Fmt.pr "%s%a %s@," (String.make indent ' ') Oi.Style.header_string
        (Fmt.str "%-11s" (label ^ ":"))
        v

(* Emit one package's metadata fields. Returns the description body so
   the caller can choose where to render it (today: at the bottom of
   the info page). [indent] is 0 for the legacy single-target layout
   and 2 for per-package blocks under a project-mode pkg header. *)
let show_pkg_meta_block ~indent (pkg : OpamPackage.t) (opam : OpamFile.OPAM.t) =
  let synopsis, license, homepage, dev_repo, maintainer, tags, description =
    show_package_meta pkg opam
  in
  let line = show_indented_meta_line ~indent in
  line "Synopsis" synopsis;
  line "License" license;
  line "Homepage" homepage;
  (* Only surface dev-repo when it adds information beyond the
     homepage. Many opam files repeat the same github URL for both,
     which just makes the info page noisier. *)
  if dev_repo <> homepage then line "Source" dev_repo;
  line "Maintainer" maintainer;
  line "Tags" tags;
  description

(* Render the default succinct info page. [target_opams] is empty when
   we have no metadata to show, length 1 for a CLI target or
   single-package project (legacy inline layout), and length >=2 for a
   multi-package project (one indented block per package, headed by
   the package name). *)
let show_render_info ~target_label ~target_version ~target_opams ~overlay
    ~os_key ~ocaml_version ~n_cached ~n_source ~all_depexts ~dep_status
    ~repositories ~binaries =
  let n_total = n_cached + n_source in
  Fmt.pr "@[<v>";
  let target_line =
    match target_version with
    | "" -> target_label
    | v -> Fmt.str "%s %s" target_label v
  in
  show_meta_line "Target" target_line;
  let descriptions =
    match target_opams with
    | [] -> []
    | [ (pkg, opam) ] ->
        let descr = show_pkg_meta_block ~indent:0 pkg opam in
        if descr = "" then [] else [ ("", descr) ]
    | many ->
        (* Multi-package project mode: render each package as its own
           indented block under a name header so every *.opam in the
           directory contributes its synopsis/license/etc. *)
        let descrs =
          List.filter_map
            (fun (pkg, opam) ->
              let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
              Fmt.pr "@,%a@," Oi.Style.header_string name;
              let descr = show_pkg_meta_block ~indent:2 pkg opam in
              if descr = "" then None else Some (name, descr))
            many
        in
        Fmt.pr "@,";
        descrs
  in
  (match binaries with
  | [] -> ()
  | bs -> show_meta_line "Binaries" (String.concat ", " bs));
  (match overlay with
  | None -> ()
  | Some tag -> show_meta_line "Overlay" (Fmt.str "%a" Oi.Style.info_string tag));
  show_meta_line "Platform" os_key;
  show_meta_line "OCaml" ocaml_version;
  Fmt.pr "@,";
  if n_source = 0 then
    show_meta_line "Packages" (Fmt.str "%d total, all cached locally." n_total)
  else begin
    show_meta_line "Packages" (Fmt.str "%d total" n_total);
    Fmt.pr "              cached: %d@," n_cached;
    Fmt.pr "              build:  %d  (from source)@," n_source
  end;
  Fmt.pr "@,";
  (match (dep_status : Oi.Depexts.status option) with
  | _ when OpamSysPkg.Set.is_empty all_depexts ->
      show_meta_line "Depexts" "(no depexts declared)"
  | None ->
      (* [--os] set: can't tell what's installed on this host, so
         just list them all plain. *)
      let names =
        OpamSysPkg.Set.elements all_depexts |> List.map OpamSysPkg.to_string
      in
      show_meta_line "Depexts" (String.concat ", " names);
      Fmt.pr "            %a@," Oi.Style.dim_string
        "(host check skipped because --os is set)"
  | Some st ->
      (* Every depext declared, with the uninstalled ones marked.
         Missing tokens are styled in yellow so they stand out even
         when "(missing)" is the only textual marker. *)
      let render p =
        let name = OpamSysPkg.to_string p in
        if OpamSysPkg.Set.mem p st.missing then
          Fmt.str "%a" Oi.Style.warn_string (name ^ " (missing)")
        else name
      in
      let rendered =
        OpamSysPkg.Set.elements all_depexts
        |> List.map render |> String.concat ", "
      in
      show_meta_line "Depexts" rendered;
      if not (OpamSysPkg.Set.is_empty st.missing) then
        let missing_names =
          OpamSysPkg.Set.elements st.missing |> List.map OpamSysPkg.to_string
        in
        Fmt.pr "            %a@," Oi.Style.dim_string
          (Fmt.str "Run: sudo apt install %s" (String.concat " " missing_names)));
  (match repositories with
  | [] -> ()
  | rows ->
      Fmt.pr "@,%a@," Oi.Style.header_string "Repositories:";
      (* Two columns: [@handle (version)] left-padded to the longest
         token so URLs line up. *)
      let left = List.map (fun (h, v, _) -> Fmt.str "@%s (%s)" h v) rows in
      let col = List.fold_left (fun m s -> max m (String.length s)) 0 left in
      List.iter2
        (fun (_, _, url) l ->
          Fmt.pr "  %a  %s@," Oi.Style.info_string (Fmt.str "%-*s" col l) url)
        rows left);
  (match descriptions with
  | [] -> ()
  | [ ("", body) ] ->
      Fmt.pr "@,%a@," Oi.Style.header_string "Description:";
      String.split_on_char '\n' body
      |> List.iter (fun line -> Fmt.pr "  %s@," line)
  | many ->
      List.iter
        (fun (name, body) ->
          let label =
            if name = "" then "Description:"
            else Fmt.str "Description (%s):" name
          in
          Fmt.pr "@,%a@," Oi.Style.header_string label;
          String.split_on_char '\n' body
          |> List.iter (fun line -> Fmt.pr "  %s@," line))
        many);
  Fmt.pr "@]@."

(* -- overlay listing ---------------------------------------------------- *)

(* Highest [version] under [pkgs_dir/name/<name.version>/]; [None] when no
   parseable opam dir exists for [name]. *)
let latest_version_of ~pkgs_dir ~name =
  let dir = pkgs_dir / name in
  if not (Sys.is_directory dir) then None
  else
    let parse pkg_s =
      Stdlib.Option.bind (OpamPackage.of_string_opt pkg_s) (fun p ->
          if OpamPackage.Name.to_string (OpamPackage.name p) = name then
            Some (OpamPackage.version p)
          else None)
    in
    let entries = try Sys.readdir dir |> Array.to_list with _ -> [] in
    List.fold_left
      (fun acc s ->
        match (acc, parse s) with
        | _, None -> acc
        | None, Some v -> Some v
        | Some best, Some v when OpamPackage.Version.compare v best > 0 ->
            Some v
        | _ -> acc)
      None entries
    |> Stdlib.Option.map OpamPackage.Version.to_string

(* Binaries shipped by [hash]: every [bin/X] or [sbin/X] entry the index
   has on file. Empty when nothing was indexed. *)
let layer_binaries db ~hash =
  D10.Index.files db ~hash
  |> List.filter_map (fun path ->
      match String.split_on_char '/' path with
      | ("bin" | "sbin") :: name :: _ -> Some name
      | _ -> None)
  |> List.sort_uniq String.compare

type overlay_state =
  | Cached of string list  (** binaries *)
  | Build_failed
  | Declared

let lookup_state ?db ~os_key ~name ~ver () =
  match db with
  | None -> Declared
  | Some db -> (
      match D10.Index.find_layer db ~name ~version:ver ~os_key with
      | Some (hash, 0) -> Cached (layer_binaries db ~hash)
      | Some _ -> Build_failed
      | None -> Declared)

let render_state =
  let dim s = Tty.Span.styled Oi.Style.dim s in
  fun st ->
    let label =
      match st with
      | Cached _ -> Tty.Span.styled Oi.Style.ok "cached"
      | Build_failed -> Tty.Span.styled Oi.Style.error "build failed"
      | Declared -> dim "declared"
    in
    let bins =
      match st with
      | Cached (_ :: _ as bs) -> Tty.Span.text (String.concat ", " bs)
      | _ -> dim "—"
    in
    (label, bins)

(* Walk [<reporepo>/v2/<handle>/packages/<name>/<name.version>/] to
   enumerate every package the overlay declares, then cross-reference
   the d10 index for cached layers + binaries. One row per
   (package, latest-version): cached packages show their binaries;
   uncached ones show as "declared". *)
(* Solve every root group that the overlay declares (or every package
   it ships when [x-root-packages] is empty), elaborate each into an
   exec plan, dedup the resulting [package_plan]s by layer hash, and
   render the union as a Unicode dependency tree.

   This is the [oi show @HANDLE] companion to [oi build @HANDLE]:
   same solver inputs, same per-group elaboration, but no recipe
   emission, no fetch, no build. We merge at the [Oi.Plan.t] level
   rather than via [D10ir.Plan.merge] so the view includes every
   layer the build would touch — including [Binary] cache hits, which
   [Recipe_emitter.emit] strips because [D10ir.Direct.run] doesn't
   need to rebuild them.

   The dedup key is the layer hash, so a package like [uucp.17.0.0]
   resolved into N distinct layer hashes across the overlay's roots
   surfaces as N nodes (one per unique dep set), not one collapsed
   row. That is the whole point of the merged view.

   Per-group failures (solver errors etc.) are logged and the group
   is dropped from the merge — partial output is more useful than no
   output for a 50-root overlay where one root has a conflict. *)
let show_merged_plan ~(harness : Harness.env) ~handles ~refresh =
  let {
    Harness.proc_mgr;
    fs;
    clock;
    sys;
    platform;
    os_key;
    cache;
    data_dir;
    http_session;
    _;
  } =
    harness
  in
  let env : Oi.Build_pipeline.env =
    { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
  in
  let conf_host =
    Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let bar_label = String.concat " " (List.map (fun h -> "@" ^ h) handles) in
  let req : Oi.Build_pipeline.request =
    {
      targets =
        List.map (fun h : Oi.Build_pipeline.target -> Overlay_all h) handles;
      with_repos = [];
      pins = [];
      extra_repos = [];
      constraints = OpamPackage.Name.Map.empty;
      toolchain_override = None;
      toolchain = None;
      conf = conf_host;
      local_packages_dir = None;
      project_root = None;
      force_source = false;
      refresh;
    }
  in
  let solved =
    Progress_ui.with_ui ~target:bar_label
      ~clock:(clock :> _ Eio.Resource.t)
      ~enabled:(Tty.is_tty ())
    @@ fun ui_reporter -> Oi.Build_pipeline.solve env ~reporter:ui_reporter req
  in
  (* Log per-group failures from the unified pipeline. *)
  List.iter
    (fun (gr : Oi.Build_pipeline.group_result) ->
      match gr.error with
      | Ok () -> ()
      | Error e ->
          let msg =
            match e with
            | Solve_failed { msg; _ } -> Fmt.str "solve: %s" msg
            | Cycle _ -> "dependency cycle"
            | Empty_after_strip -> "empty after toolchain strip"
            | Elaborate_failed { msg } -> Fmt.str "elaborate: %s" msg
            | Emit_failed { msg } -> Fmt.str "recipe emit: %s" msg
          in
          Logs.warn (fun m -> m "skip %s: %s" gr.group.label msg))
    solved.groups;
  let all_plans =
    List.concat_map
      (fun (gr : Oi.Build_pipeline.group_result) ->
        match gr.exec_plan with Some p -> p.packages | None -> [])
      solved.groups
  in
  let n_groups_solved =
    List.fold_left
      (fun acc (gr : Oi.Build_pipeline.group_result) ->
        if Result.is_ok gr.error then acc + 1 else acc)
      0 solved.groups
  in
  if all_plans = [] then begin
    Fmt.pr "No solvable groups for %s.@." bar_label;
    exit 1
  end;
  (* Dedup [package_plan]s by layer hash. Same hash from different
     groups → same logical node; first occurrence wins (preserves the
     overlay attribution we'd otherwise lose). *)
  let by_hash : (string, Oi.Plan.package_plan) Hashtbl.t =
    Hashtbl.create 1024
  in
  List.iter
    (fun (p : Oi.Plan.package_plan) ->
      if not (Hashtbl.mem by_hash p.layer_hash) then
        Hashtbl.add by_hash p.layer_hash p)
    all_plans;
  (* Roots: layer hashes that are not depended on by any other
     package_plan in the merged view. *)
  let dep_set = Hashtbl.create 1024 in
  Hashtbl.iter
    (fun _ (p : Oi.Plan.package_plan) ->
      List.iter
        (fun (d : Oi.Identity.dep) -> Hashtbl.replace dep_set d.hash ())
        p.dep_layers)
    by_hash;
  let roots =
    Hashtbl.fold
      (fun h p acc -> if Hashtbl.mem dep_set h then acc else p :: acc)
      by_hash []
    |> List.sort (fun (a : Oi.Plan.package_plan) b ->
        String.compare a.pkg b.pkg)
  in
  let short_hash h = if String.length h > 12 then String.sub h 0 12 else h in
  let label_first (p : Oi.Plan.package_plan) =
    let tag = match p.method_ with Source -> "" | Binary -> " [cached]" in
    Fmt.str "%s  %s%s" p.pkg (short_hash p.layer_hash) tag
  in
  let label_ref (p : Oi.Plan.package_plan) = p.pkg in
  let key_of (p : Oi.Plan.package_plan) = p.layer_hash in
  let children (p : Oi.Plan.package_plan) =
    List.filter_map
      (fun (d : Oi.Identity.dep) -> Hashtbl.find_opt by_hash d.hash)
      p.dep_layers
  in
  Fmt.pr "%a@.@." Oi.Style.header_string
    (Fmt.str "Merged plan for %s on %s" bar_label os_key);
  Oi.Dep_tree.render ~label_first ~label_ref ~key_of ~children roots;
  let n_layers = Hashtbl.length by_hash in
  let n_source =
    Hashtbl.fold
      (fun _ (p : Oi.Plan.package_plan) acc ->
        if p.method_ = Oi.Identity.Source then acc + 1 else acc)
      by_hash 0
  in
  let n_binary = n_layers - n_source in
  Fmt.pr
    "@.%d group(s) \u{2192} %d unique layer(s) (%d source, %d cached), %d \
     root(s); \u{21B0} marks a back-reference to a layer expanded earlier@."
    n_groups_solved n_layers n_source n_binary (List.length roots);
  (* Divergence summary: any [name.version] that resolved to more
     than one layer hash across the overlay's roots. The hash differs
     because the transitive dep set differs (different version of a
     grandchild, different patch set, etc.); for each variant we
     show its consumers and the entries in [dep_layers] that diverge
     between variants. *)
  let variants_by_pkg : (string, Oi.Plan.package_plan) Hashtbl.t =
    Hashtbl.create 64
  in
  Hashtbl.iter
    (fun _ (p : Oi.Plan.package_plan) -> Hashtbl.add variants_by_pkg p.pkg p)
    by_hash;
  let divergent =
    Hashtbl.fold
      (fun pkg () acc ->
        let variants = Hashtbl.find_all variants_by_pkg pkg in
        let unique_hashes =
          List.map (fun (p : Oi.Plan.package_plan) -> p.layer_hash) variants
          |> List.sort_uniq String.compare
        in
        if List.length unique_hashes > 1 then (pkg, variants) :: acc else acc)
      (let s = Hashtbl.create 64 in
       Hashtbl.iter
         (fun _ (p : Oi.Plan.package_plan) -> Hashtbl.replace s p.pkg ())
         by_hash;
       s)
      []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  if divergent = [] then ()
  else begin
    (* For each layer hash, find every consumer (a [package_plan] in
       the merged set whose [dep_layers] references this hash). The
       lookup is per-pair-of-hashes pricey if recomputed, so build it
       once over [by_hash]. *)
    let consumers_of : (string, Oi.Plan.package_plan list) Hashtbl.t =
      Hashtbl.create 1024
    in
    Hashtbl.iter
      (fun _ (p : Oi.Plan.package_plan) ->
        List.iter
          (fun (d : Oi.Identity.dep) ->
            let prev =
              Hashtbl.find_opt consumers_of d.hash
              |> Stdlib.Option.value ~default:[]
            in
            Hashtbl.replace consumers_of d.hash (p :: prev))
          p.dep_layers)
      by_hash;
    let consumers_of_hash h =
      Hashtbl.find_opt consumers_of h
      |> Stdlib.Option.value ~default:[]
      |> List.sort (fun (a : Oi.Plan.package_plan) b ->
          String.compare a.pkg b.pkg)
    in
    Fmt.pr "@.%a@.@." Oi.Style.header_string
      (Fmt.str "Divergent layers (%d package%s with multiple variants)"
         (List.length divergent)
         (if List.length divergent = 1 then "" else "s"));
    List.iter
      (fun (pkg, variants) ->
        let variants =
          (* Sort variants by hash so the diff output is stable
             across runs. *)
          List.sort
            (fun (a : Oi.Plan.package_plan) b ->
              String.compare a.layer_hash b.layer_hash)
            variants
        in
        Fmt.pr "%a (%d variants):@." Oi.Style.accent_string pkg
          (List.length variants);
        List.iter
          (fun (v : Oi.Plan.package_plan) ->
            let cons = consumers_of_hash v.layer_hash in
            let cons_label =
              match cons with
              | [] -> "(root)"
              | _ ->
                  let names =
                    List.map (fun (c : Oi.Plan.package_plan) -> c.pkg) cons
                  in
                  let max_inline = 6 in
                  if List.length names <= max_inline then
                    String.concat ", " names
                  else
                    let head, _ =
                      List.fold_left
                        (fun (acc, i) n ->
                          if i < max_inline then (n :: acc, i + 1)
                          else (acc, i + 1))
                        ([], 0) names
                    in
                    Fmt.str "%s, +%d more"
                      (String.concat ", " (List.rev head))
                      (List.length names - max_inline)
            in
            Fmt.pr "  \u{25B8} %s [%s] used by %s@." (short_hash v.layer_hash)
              (Oi.Identity.method_to_string v.method_)
              cons_label)
          variants;
        (* Pairwise dep diff between consecutive variants — for 2
           variants this is the full diff; for N>2 it shows the chain
           A→B, B→C, … which is enough to spot the changing axis
           without N² output. *)
        let dep_map (p : Oi.Plan.package_plan) =
          let m = Hashtbl.create (List.length p.dep_layers) in
          List.iter
            (fun (d : Oi.Identity.dep) ->
              Hashtbl.replace m (Oi.Identity.to_string d.id) d.hash)
            p.dep_layers;
          m
        in
        let rec diff_pairs = function
          | [] | [ _ ] -> ()
          | (a : Oi.Plan.package_plan)
            :: ((b : Oi.Plan.package_plan) :: _ as rest) ->
              let ma = dep_map a and mb = dep_map b in
              let names =
                let s = Hashtbl.create 32 in
                Hashtbl.iter (fun k _ -> Hashtbl.replace s k ()) ma;
                Hashtbl.iter (fun k _ -> Hashtbl.replace s k ()) mb;
                Hashtbl.fold (fun k () acc -> k :: acc) s []
                |> List.sort String.compare
              in
              let diffs =
                List.filter_map
                  (fun n ->
                    let ah = Hashtbl.find_opt ma n in
                    let bh = Hashtbl.find_opt mb n in
                    match (ah, bh) with
                    | Some x, Some y when String.equal x y -> None
                    | _ -> Some (n, ah, bh))
                  names
              in
              if diffs <> [] then begin
                Fmt.pr "  dep diff %s vs %s:@." (short_hash a.layer_hash)
                  (short_hash b.layer_hash);
                List.iter
                  (fun (n, ah, bh) ->
                    match (ah, bh) with
                    | Some x, Some y ->
                        Fmt.pr "    %s: %s \u{2192} %s@." n (short_hash x)
                          (short_hash y)
                    | Some x, None ->
                        Fmt.pr "    %s: %s \u{2192} (absent)@." n (short_hash x)
                    | None, Some y ->
                        Fmt.pr "    %s: (absent) \u{2192} %s@." n (short_hash y)
                    | None, None -> ())
                  diffs
              end;
              diff_pairs rest
        in
        diff_pairs variants;
        Fmt.pr "@.")
      divergent
  end

let show_overlay ~cache_root ~os_key ~handle =
  let reporepo = Terms.reporepo_path () in
  let pkgs_dir =
    Oi.Source.Reporepo.overlay_packages_dir ~path:reporepo ~handle
  in
  if not (Sys.file_exists pkgs_dir) then begin
    Fmt.pr "@[<v>Overlay %a is not materialised under %s.@,Run %a first.@]@."
      Oi.Style.accent_string ("@" ^ handle) reporepo Oi.Style.header_string
      (Fmt.str "oi repo bump %s" handle);
    exit 1
  end;
  let latest =
    let names = try Sys.readdir pkgs_dir |> Array.to_list with _ -> [] in
    List.filter_map
      (fun name ->
        Stdlib.Option.map
          (fun v -> (name, v))
          (latest_version_of ~pkgs_dir ~name))
      names
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  if latest = [] then begin
    Fmt.pr "Overlay %a has no packages.@." Oi.Style.accent_string ("@" ^ handle);
    exit 0
  end;
  (* Open the local index if present; absent index → every row is
     "declared". *)
  let index_path = cache_root / "layers" / "index.db" in
  let db =
    if Sys.file_exists index_path then
      try Some (D10.Index.open_ ~path:index_path) with _ -> None
    else None
  in
  let rows, cached, declared =
    List.fold_right
      (fun (name, ver) (rows, c, d) ->
        let st = lookup_state ?db ~os_key ~name ~ver () in
        let state_span, bins_span = render_state st in
        let row =
          [
            Tty.Span.text name;
            Tty.Span.styled Oi.Style.dim ver;
            state_span;
            bins_span;
          ]
        in
        match st with
        | Cached _ -> (row :: rows, c + 1, d)
        | Build_failed | Declared -> (row :: rows, c, d + 1))
      latest ([], 0, 0)
  in
  Stdlib.Option.iter D10.Index.close db;
  Fmt.pr "%a@.@." Oi.Style.header_string
    (Fmt.str "Overlay @%s on %s" handle os_key);
  Oi.Style.pp_table Fmt.stdout
    (Tty.Table.of_rows ~header_style:Oi.Style.header
       [
         Tty.Table.column "PACKAGE";
         Tty.Table.column "VERSION";
         Tty.Table.column "STATE";
         Tty.Table.column ~max_width:48 "BINARIES";
       ]
       rows);
  Fmt.pr "@.%a %d package(s) — %d cached, %d declared@." Oi.Style.header_string
    "Total:" (List.length latest) cached declared

(* -- cache listing ------------------------------------------------------- *)

(* Render every cached layer in [<cache>/layers/<os_key>/], optionally
   filtered to a single overlay handle. Rows: handle, package.version,
   short hash, on-disk size. Sorted by handle then package name.
   Overlay attribution comes from each layer's [provenance.json] sidecar
   — [layer.json] no longer carries it. *)
let show_cache ~fs ~sys ~cache_root ~os_key ~handle =
  let layers_dir = cache_root / "layers" / os_key in
  let overlay_of_hash hash =
    Oi.Provenance.overlay_of_layer ~fs ~cache_root ~os_key ~hash
  in
  let rows =
    if not (Sys.file_exists layers_dir) then []
    else
      let entries =
        try Sys.readdir layers_dir |> Array.to_list with _ -> []
      in
      List.filter_map
        (fun hash ->
          match
            D10.Layer.load_meta Eio.Path.(fs / layers_dir / hash / "layer.json")
          with
          | None -> None
          | Some m ->
              let overlay = overlay_of_hash hash in
              let keep =
                match handle with
                | None -> true
                | Some h -> (
                    match overlay with
                    | Some (o : D10.Overlay.t) -> o.handle = h
                    | None -> false)
              in
              if keep then
                let sz =
                  Oi.Cache.size ~sys Eio.Path.(fs / layers_dir / hash / "fs")
                in
                Some (m, overlay, hash, sz)
              else None)
        entries
  in
  if rows = [] then
    let scope, suggest =
      match handle with
      | Some h -> ("@" ^ h, Fmt.str "oi build @%s" h)
      | None -> ("the cache", "oi build --all")
    in
    Fmt.pr "(no cached layers in %s; run %a to populate)@." scope
      Oi.Style.header_string suggest
  else begin
    let handle_of (o : D10.Overlay.t option) =
      match o with Some o -> o.handle | None -> ""
    in
    let rows =
      List.sort
        (fun ((a : D10.Layer.meta), oa, _, _) (b, ob, _, _) ->
          match String.compare (handle_of oa) (handle_of ob) with
          | 0 -> String.compare a.package b.package
          | c -> c)
        rows
    in
    let handle_str = function
      | Some (o : D10.Overlay.t) -> "@" ^ o.handle
      | None -> ""
    in
    let header =
      match handle with
      | Some h -> Fmt.str "Cached layers for @%s on %s" h os_key
      | None -> Fmt.str "Cached layers on %s" os_key
    in
    Fmt.pr "%a@.@." Oi.Style.header_string header;
    let total = ref 0L in
    let table_rows =
      List.map
        (fun ((m : D10.Layer.meta), overlay, hash, sz) ->
          total := Int64.add !total sz;
          let short = String.sub hash 0 (min 12 (String.length hash)) in
          [
            Tty.Span.styled Oi.Style.info (handle_str overlay);
            Tty.Span.text m.package;
            Tty.Span.styled Oi.Style.dim short;
            Tty.Span.text (Fmt.str "%a" Oi.Cache.pp_size sz);
          ])
        rows
    in
    let table =
      Tty.Table.of_rows ~header_style:Oi.Style.header
        [
          Tty.Table.column "OVERLAY";
          Tty.Table.column "PACKAGE";
          Tty.Table.column "HASH";
          Tty.Table.column ~align:`Right "SIZE";
        ]
        table_rows
    in
    Oi.Style.pp_table Fmt.stdout table;
    Fmt.pr "@.%a %d layer(s), %a total@." Oi.Style.header_string "Summary:"
      (List.length rows) Oi.Cache.pp_size !total
  end

let cmd =
  let run (c : Terms.common) refresh skip_local toolchain_override targets
      with_repos with_deps tree plan_view summary only_depexts os_override
      show_all show_cache_listing =
    Harness.run @@ fun ~sw env ->
    let harness =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let { Harness.fs; clock; sys; platform; os_key; cache; _ } = harness in
    let data_dir = c.data_dir in
    (* Bare [@HANDLE] dispatch:
       - [--all]: walk every cached layer (handle-filtered).
       - [--cache]: legacy overlay listing of declared / cached pkgs.
       - default: solve every overlay root (or every package, if
         [x-root-packages] is empty), merge the recipes, and print the
         deduped layer-hash dependency tree. Same view that
         [oi build @HANDLE] would execute. *)
    (* All-bare-handle inputs route to the merged-plan view: [oi
       show @avsm @samoht] solves every overlay's roots and renders
       a single deduped tree. Mixed inputs ([@HANDLE] + [pkg.version])
       fall through to the regular per-target flow. *)
    let bare_handles =
      match targets with
      | [] -> None
      | _ ->
          let hs = List.filter_map Target.bare_handle targets in
          if List.compare_lengths hs targets = 0 then Some hs else None
    in
    let cache_root = Oi.Cache.root_s cache in
    (match (bare_handles, show_all, show_cache_listing) with
    | _, true, _ ->
        let h = match bare_handles with Some [ h ] -> Some h | _ -> None in
        show_cache ~fs ~sys ~cache_root ~os_key ~handle:h;
        exit 0
    | Some [ h ], false, true ->
        show_overlay ~cache_root ~os_key ~handle:h;
        exit 0
    | Some _, false, true ->
        Oi.Error.config_error
          "oi show --cache: pass exactly one bare @HANDLE to filter the listing"
    | Some handles, false, false ->
        show_merged_plan ~harness ~handles ~refresh;
        exit 0
    | None, false, _ -> ());
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf_host =
      Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    let conf =
      match os_override with
      | None -> conf_host
      | Some os -> Os_override.resolve conf_host os
    in
    let cwd_s, _ = Workspace.resolved_cwd fs in
    (* No pre-rewrite of the targets: solve them as-is. The layer
       index is consulted later only when the solve doesn't yield a
       matching package — same policy as [oi run]. Pre-rewriting
       would inject a [@default/X] handle pin that overrides any
       user [--with=X.VERSION] constraint. *)
    (* One [Target.extract_handle_pins] pass handles both user-typed
       [@handle/pkg] and the rewrites we just introduced: the
       handle is routed into [with_repos], the stripped package spec
       replaces the original token, and a [handle_pin] is recorded
       so the overlay version gets pinned later. *)
    let targets, with_repos, target_pins =
      Target.extract_handle_pins ~with_repos targets
    in
    let with_deps, with_repos, with_pins =
      Target.extract_handle_pins ~with_repos with_deps
    in
    let handle_pins = target_pins @ with_pins in
    let extra_deps, url_project =
      Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
    in
    (* Only consult the local project's declarations when the user did
       not name an explicit target; otherwise [oi show pkg] inside a
       project would silently pull the project's own deps into the
       solve and produce misleading output. *)
    let ( project_extras,
          project_pins,
          project_overlays,
          project_deps,
          project_local_packages,
          project_packages_dir ) =
      if targets <> [] || skip_local then ([], [], [], [], [], None)
      else
        match Oi.Project.load ~fs cwd_s with
        | exception Sys_error _ -> ([], [], [], [], [], None)
        | exception Eio.Exn.Io _ -> ([], [], [], [], [], None)
        | p ->
            ( p.extra_repos,
              p.pins,
              p.overlays,
              p.deps,
              p.local_packages,
              p.packages_dir )
    in
    let project_extras = project_extras @ url_project.extra_repos in
    let project_pins = project_pins @ url_project.pins in
    let project_overlays = project_overlays @ url_project.overlays in
    (* Resolve the toolchain now that all [@handle]s are in scope. *)
    let tc_handles =
      Target.pin_handles handle_pins
      @ Target.handles_of_tokens with_repos
      @ project_overlays
      |> List.sort_uniq String.compare
    in
    let toolchain =
      Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:false
        ~override:toolchain_override ~handles:tc_handles ()
    in
    let project_overlays =
      Oi.Pipeline.filter_compatible_overlays
        ~reporepo_path:(Terms.reporepo_path ()) ~toolchain project_overlays
    in
    let with_repos = project_overlays @ with_repos in
    let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
    let all_extras =
      Target.merge_extras ~cli:cli_extras ~project:project_extras
    in
    let local_packages_dir =
      match project_packages_dir with
      | Some _ -> project_packages_dir
      | None -> url_project.packages_dir
    in
    let extra_constraints = Oi.Project.Script.constraints extra_deps in
    let handle_constraints =
      Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras
        handle_pins
    in
    let extra_constraints =
      OpamPackage.Name.Map.union
        (fun a _ -> a)
        handle_constraints extra_constraints
    in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Project.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_deps
    in
    let url_names = List.map OpamPackage.Name.of_string url_project.roots in
    let project_dep_names = List.map OpamPackage.Name.of_string project_deps in
    (* [pkg.version] is a common shorthand at the CLI ("show me uchar
       at exactly 0.0.2"). Split each [TARGET] via [parse_pkg_target]
       so a dotted name routes its version into [extra_constraints],
       leaving [names] as the bare package names the solver expects.
       Bare names ([uchar]) come back as [(name, None)] and pass
       through unchanged. *)
    let target_names, target_constraints =
      List.fold_left
        (fun (names, cs) s ->
          let name, vc = Target.parse_pkg_target s in
          let cs =
            match vc with
            | None -> cs
            | Some vc -> OpamPackage.Name.Map.add name vc cs
          in
          (name :: names, cs))
        ([], OpamPackage.Name.Map.empty)
        targets
    in
    let target_names = List.rev target_names in
    let extra_constraints =
      OpamPackage.Name.Map.union
        (fun existing _ -> existing)
        extra_constraints target_constraints
    in
    let names =
      target_names @ project_dep_names @ extra_names @ url_names
      |> Oi.Pipeline.strip_compiler_roots_for_override
           ~override:toolchain_override ~toolchain
    in
    if names = [] then
      Oi.Error.config_error
        "oi show: nothing to show (no TARGET, no --with, and no *.opam files \
         in %s)"
        cwd_s;
    (* Drive the solve through [Build_pipeline.solve] — same path as
       [oi build TARGET] so target classification, toolchain pickup,
       and pin/extra-repo handling match. The downstream rendering
       below consumes [solved.groups[0].(pkgs, build_plan, pkgs_dir)]. *)
    let pipeline_env : Oi.Build_pipeline.env =
      {
        proc_mgr = harness.proc_mgr;
        fs;
        clock;
        sys;
        os_key;
        cache;
        data_dir;
        http_session = harness.http_session;
      }
    in
    let token_strs = List.map OpamPackage.Name.to_string names in
    let req : Oi.Build_pipeline.request =
      {
        targets = [ Group { tokens = token_strs; handles = [] } ];
        with_repos;
        pins = project_pins;
        extra_repos = all_extras;
        constraints = extra_constraints;
        toolchain_override;
        toolchain;
        conf;
        local_packages_dir;
        project_root = None;
        force_source = false;
        refresh;
      }
    in
    let solve_label =
      match targets with [] -> "." | xs -> String.concat " " xs
    in
    let solved =
      Progress_ui.with_ui ~target:solve_label
        ~clock:(clock :> _ Eio.Resource.t)
        ~enabled:(Tty.is_tty ())
      @@ fun ui_reporter ->
      Oi.Build_pipeline.solve pipeline_env ~reporter:ui_reporter req
    in
    let group =
      match solved.groups with
      | [ gr ] -> gr
      | _ -> Oi.Error.msg "oi show: unexpected solve group count"
    in
    let exec_plan =
      match group.error with
      | Error (Solve_failed { msg; _ }) ->
          (* Same did-you-mean hint as the inline solver-error path. *)
          let contains ~needle s =
            let nl = String.length needle and sl = String.length s in
            if nl = 0 || nl > sl then false
            else
              let rec loop i =
                if i + nl > sl then false
                else if String.sub s i nl = needle then true
                else loop (i + 1)
              in
              loop 0
          in
          let suggest_for target =
            let lower = String.lowercase_ascii target in
            if String.length lower < 4 then []
            else
              List.concat_map
                (fun dir -> try Sys.readdir dir |> Array.to_list with _ -> [])
                group.pkgs_dir
              |> List.sort_uniq String.compare
              |> List.filter (fun name ->
                  let ln = String.lowercase_ascii name in
                  String.length ln >= 4
                  && ln <> lower
                  && (contains ~needle:lower ln || contains ~needle:ln lower))
          in
          let extras =
            targets
            |> List.concat_map suggest_for
            |> List.sort_uniq String.compare
          in
          let hint =
            match extras with
            | [] -> ""
            | xs ->
                let shown, rest =
                  if List.length xs > 8 then
                    (List.filteri (fun i _ -> i < 8) xs, List.length xs - 8)
                  else (xs, 0)
                in
                Fmt.str "\n\nDid you mean one of these packages?\n  %s%s"
                  (String.concat " " shown)
                  (if rest > 0 then Fmt.str " (+%d more)" rest else "")
          in
          Oi.Error.no_solution (msg ^ hint)
      | Error (Cycle cycles) ->
          Oi.Error.config_error "dependency cycle in solved packages:@\n%a"
            Oi.Plan.pp_cycles cycles
      | Error (Empty_after_strip | Elaborate_failed _ | Emit_failed _) ->
          Oi.Error.msg "oi show: solve produced no plan"
      | Ok () -> (
          match group.exec_plan with
          | Some xp -> xp
          | None -> Oi.Error.msg "oi show: empty solve result")
    in
    let packages_dirs = group.pkgs_dir in
    let json_plan_node (p : Oi.Plan.package_plan) =
      let opam_pkg = opam_pkg_of_package_plan p in
      let name = OpamPackage.Name.to_string (OpamPackage.name opam_pkg) in
      let version =
        OpamPackage.Version.to_string (OpamPackage.version opam_pkg)
      in
      let method_ = Oi.Identity.method_to_string p.method_ in
      let deps =
        List.map (fun (d : Oi.Identity.dep) -> d.id.name) p.dep_layers
      in
      (name, version, method_, p.layer_hash, deps)
    in
    let json_node_codec =
      let open Jsont in
      Object.map ~kind:"plan_node" (fun name version method_ layer_hash deps ->
          (name, version, method_, layer_hash, deps))
      |> Object.mem "name" string ~enc:(fun (n, _, _, _, _) -> n)
      |> Object.mem "version" string ~enc:(fun (_, v, _, _, _) -> v)
      |> Object.mem "method" string ~enc:(fun (_, _, m, _, _) -> m)
      |> Object.mem "layer_hash" string ~enc:(fun (_, _, _, h, _) -> h)
      |> Object.mem "deps" (list string) ~dec_absent:[]
           ~enc:(fun (_, _, _, _, d) -> d)
           ~enc_omit:(( = ) [])
      |> Object.finish
    in
    let render_json_plan () =
      let all_depexts, _ =
        show_depexts ~conf ~packages_dirs ~plan:exec_plan ~os_override
      in
      let depexts =
        OpamSysPkg.Set.fold
          (fun p acc -> OpamSysPkg.to_string p :: acc)
          all_depexts []
        |> List.sort String.compare
      in
      let nodes = List.map json_plan_node exec_plan.packages in
      let target_label =
        match targets with
        | [] -> (
            match project_local_packages with
            | [ p ] -> p
            | _ :: _ :: _ -> "(project)"
            | [] -> "(project)")
        | _ -> String.concat " " targets
      in
      let toolchain_handle =
        match toolchain with
        | None -> None
        | Some (info : Oi.Toolchain.info) -> Some info.handle
      in
      let envelope_codec =
        let open Jsont in
        Object.map ~kind:"oi_show"
          (fun
            _schema_version
            target
            os_key
            ocaml_version
            toolchain
            packages
            depexts
          -> (target, os_key, ocaml_version, toolchain, packages, depexts))
        |> Object.mem "schema_version" string ~enc:(fun _ ->
            Oi.Stamp.json_schema_version)
        |> Object.mem "target" string ~enc:(fun (t, _, _, _, _, _) -> t)
        |> Object.mem "os_key" string ~enc:(fun (_, o, _, _, _, _) -> o)
        |> Object.mem "ocaml_version" string ~enc:(fun (_, _, v, _, _, _) -> v)
        |> Object.opt_mem "toolchain" string ~enc:(fun (_, _, _, t, _, _) -> t)
        |> Object.mem "packages" (list json_node_codec)
             ~enc:(fun (_, _, _, _, p, _) -> p)
        |> Object.mem "depexts" (list string) ~dec_absent:[]
             ~enc:(fun (_, _, _, _, _, d) -> d)
             ~enc_omit:(( = ) [])
        |> Object.finish
      in
      match
        Jsont_bytesrw.encode_string ~format:Jsont.Indent envelope_codec
          ( target_label,
            os_key,
            conf.ocaml_version,
            toolchain_handle,
            nodes,
            depexts )
      with
      | Ok s ->
          print_string s;
          print_newline ()
      | Error e -> Oi.Error.config_error "json encode failed: %s" e
    in
    (match c.format with
    | Json ->
        render_json_plan ();
        exit 0
    | Text -> ());
    (* Default view selection.

       The user-visible defaults are:
         no flag         → tree (this is the most useful skim view)
         --plan          → verbose per-package plan
         --summary       → metadata + depexts (the historical default)
         --only-depexts  → bare depext list (script-friendly)
         --tree          → tree (explicit; same as no flag)

       [only_depexts] and [show_all] are higher-precedence than the
       view picker — they suppress everything else. *)
    (* [--tree] is the default, so the flag is accepted but never
       branched on — present so users who explicitly type it get the
       same behavior as the default. *)
    let _ = tree in
    let view =
      if only_depexts || show_all then `Existing
      else if plan_view then `Plan
      else if summary then `Summary
      else `Tree
    in
    if view = `Plan then
      begin match group.exec_plan with
      | Some plan -> Fmt.pr "%a@." Oi.Plan.pp plan
      | None -> Oi.Error.msg "oi show --plan: no exec plan available"
      end
    else if view = `Tree then begin
      (* Render the elaborated plan as a Unicode dep tree. Each
         [package_plan] keys by its layer_hash (so two identical
         packages from different solve groups collapse to one
         back-reference); children come from [dep_layers].

         Tree roots come from [names] — the solver-root set the
         user actually asked for (CLI targets, [project_deps] from
         local *.opam files, [--with] script deps, URL-project
         roots). Falling back on graph-roots — packages no other
         package depends on — buries the user's declared deps when
         they happen to be transitive deps of a sibling: e.g. on
         a project depending on [dune; dockerfile; …], graph-roots
         hide [dune] under [dockerfile.dune.…] because [dockerfile]
         pulls [dune] in transitively. The user's mental model is
         "show me my depends as roots", not "show me leaves". *)
      let nodes = exec_plan.packages in
      let by_hash =
        let h = Hashtbl.create 64 in
        List.iter
          (fun (p : Oi.Plan.package_plan) ->
            if not (Hashtbl.mem h p.layer_hash) then
              Hashtbl.add h p.layer_hash p)
          nodes;
        h
      in
      let consumed = Hashtbl.create 64 in
      List.iter
        (fun (p : Oi.Plan.package_plan) ->
          List.iter
            (fun (d : Oi.Identity.dep) -> Hashtbl.replace consumed d.hash ())
            p.dep_layers)
        nodes;
      let by_name =
        let h = Hashtbl.create 64 in
        List.iter
          (fun (p : Oi.Plan.package_plan) ->
            let n =
              OpamPackage.Name.to_string
                (OpamPackage.name (opam_pkg_of_package_plan p))
            in
            if not (Hashtbl.mem h n) then Hashtbl.add h n p)
          nodes;
        h
      in
      let solver_root_names =
        List.map OpamPackage.Name.to_string names
        |> List.sort_uniq String.compare
      in
      (* Position in the topologically-sorted plan: deps come first,
         dependents later. Sorting roots by this index makes leaf-
         roots expand at top level before any dependent root reaches
         them, so e.g. on a project depending on both [dune] and
         [dockerfile], [dune] gets a full subtree of its own instead
         of being rendered first as a back-reference under
         [dockerfile]. *)
      let topo_pos =
        let h = Hashtbl.create 64 in
        List.iteri
          (fun i (p : Oi.Plan.package_plan) ->
            if not (Hashtbl.mem h p.layer_hash) then
              Hashtbl.add h p.layer_hash i)
          nodes;
        h
      in
      let by_topo a b =
        let pa =
          Stdlib.Option.value
            (Hashtbl.find_opt topo_pos a.Oi.Plan.layer_hash)
            ~default:max_int
        in
        let pb =
          Stdlib.Option.value
            (Hashtbl.find_opt topo_pos b.Oi.Plan.layer_hash)
            ~default:max_int
        in
        compare pa pb
      in
      let roots =
        let from_solver =
          List.filter_map
            (fun n -> Hashtbl.find_opt by_name n)
            solver_root_names
          |> List.sort by_topo
        in
        if from_solver <> [] then from_solver
        else
          (* Fallback for plans built without a meaningful root set
             (rare — would require [names = []], which the upstream
             "nothing to show" guard already rejects). Keep the
             graph-root behaviour so the tree still renders something
             useful instead of hard-erroring. *)
          List.filter
            (fun (p : Oi.Plan.package_plan) ->
              not (Hashtbl.mem consumed p.layer_hash))
            nodes
      in
      let label_first (p : Oi.Plan.package_plan) =
        let h = p.layer_hash in
        let short = if String.length h > 12 then String.sub h 0 12 else h in
        Fmt.str "%s  %s" p.pkg short
      in
      let label_ref (p : Oi.Plan.package_plan) = p.pkg in
      let key_of (p : Oi.Plan.package_plan) = p.layer_hash in
      let children (p : Oi.Plan.package_plan) =
        List.filter_map
          (fun (d : Oi.Identity.dep) -> Hashtbl.find_opt by_hash d.hash)
          p.dep_layers
      in
      Fmt.pr "%a@.@." Oi.Style.header_string "Dependency tree";
      Oi.Dep_tree.render ~label_first ~label_ref ~key_of ~children roots;
      Fmt.pr
        "@.%d packages, %d root(s); \u{21B0} marks a back-reference to a layer \
         expanded earlier in the tree@."
        (List.length nodes) (List.length roots)
    end
    else
      (* `Existing or `Summary — both fall through to the original
            metadata + depexts listing. *)
      let all_depexts, dep_status =
        show_depexts ~conf ~packages_dirs ~plan:exec_plan ~os_override
      in
      if only_depexts then
        (* Always print every depext, one per line, with no status
           marking. Intended for piping into a package manager; the
           caller handles which ones are already installed. *)
        OpamSysPkg.Set.iter
          (fun p -> Fmt.pr "%s@." (OpamSysPkg.to_string p))
          all_depexts
      else
        let target_label =
          show_target_label ~targets ~local_packages:project_local_packages
        in
        let overlay = show_overlay_label ~with_repos in
        let n_cached, n_source = show_counts exec_plan in
        let primary =
          show_primary_meta ~plan:exec_plan ~targets ~project_deps ~cwd:cwd_s
        in
        let target_version, target_opams, target_layer_hash =
          match primary with
          | None -> ("", [], None)
          | Some (From_pkg p) ->
              let opam_pkg = opam_pkg_of_package_plan p in
              ( OpamPackage.Version.to_string (OpamPackage.version opam_pkg),
                [ (opam_pkg, p.opam) ],
                Some p.layer_hash )
          | Some (From_project_opams pkgs) ->
              (* Project *.opam files rarely pin a real version;
                 "dev" isn't useful on a user-facing line, so we
                 suppress the version column here. *)
              ("", pkgs, None)
        in
        let repositories = show_repositories ?toolchain ~with_repos () in
        let binaries =
          match target_layer_hash with
          | None -> []
          | Some h -> show_package_binaries ~cache_root ~os_key ~layer_hash:h
        in
        show_render_info ~target_label ~target_version ~target_opams ~overlay
          ~os_key ~ocaml_version:conf.ocaml_version ~n_cached ~n_source
          ~all_depexts ~dep_status ~repositories ~binaries
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Opam package, binary name, $(b,pkg.VERSION), bare $(b,@HANDLE), \
             or $(b,@HANDLE/PKG). Repeatable. Omitted: read $(b,*.opam) in the \
             cwd."
          [])
  in
  let tree =
    Arg.(
      value & flag
      & info
          ~doc:
            "Render the dependency graph as a Unicode tree (default view). \
             Back-references to layers expanded earlier are prefixed with \
             $(b,\u{21B0})."
          [ "tree"; "graph" ])
  in
  let plan_view =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print the full per-package plan: layer hashes, source URLs, \
             resolved build/install commands."
          [ "plan"; "details" ])
  in
  let summary =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print metadata, package counts, depexts (missing marked), and \
             pinned overlays."
          [ "summary"; "meta" ])
  in
  let only_depexts =
    Arg.(
      value & flag
      & info ~doc:"Print depexts only, one per line." [ "only-depexts" ])
  in
  let os_override =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"OS"
          ~doc:
            "Evaluate depexts for $(b,OS) instead of the host. Any \
             $(b,dockerfile-opam) tag ($(b,alpine-3.23), $(b,ubuntu-22.04)) or \
             bare distro name. Skips the host-installed check."
          [ "os" ])
  in
  let show_all =
    Arg.(
      value & flag
      & info
          ~doc:
            "List every cached layer. With bare $(b,@HANDLE), filter to that \
             overlay."
          [ "all" ])
  in
  let show_cache_listing =
    Arg.(
      value & flag
      & info
          ~doc:
            "With bare $(b,@HANDLE), list the overlay's declared and cached \
             packages instead of the merged plan. Ignored otherwise."
          [ "cache" ])
  in
  let info =
    Cmd.info "show" ~doc:"Summarise a target's build plan and depexts"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve for $(b,TARGET) and print its plan, metadata, and depexts. \
             No sources fetched, no builds run.";
          `P "With no $(b,TARGET), reads $(b,*.opam) in the cwd.";
          `P
            "Multiple $(b,TARGET)s are solved as a single group. Bare \
             $(b,@HANDLE) targets switch to the merged-overlay view: every \
             overlay root is solved and rendered as one deduped tree.";
          `S "MODES";
          `I ("(default)", "Dependency tree.");
          `I ("$(b,--tree)", "Same as the default.");
          `I
            ( "$(b,--summary)",
              "Metadata, package counts, binaries, depexts, pinned overlays." );
          `I
            ( "$(b,--plan)",
              "Full per-package plan: layer hashes, source URLs, build/install \
               commands." );
          `I ("$(b,--only-depexts)", "Depexts only, one per line.");
          `I
            ( "bare $(b,@HANDLE)…",
              "Merged build plan across every overlay root, deduped by layer \
               hash." );
          `I
            ( "bare $(b,@HANDLE) $(b,--cache)",
              "Per-overlay package listing (cache state, known binaries)." );
          `I
            ( "$(b,--all)",
              "Cached layers across the whole cache. With bare $(b,@HANDLE), \
               filter to that overlay." );
          `S Manpage.s_examples;
          `Pre
            "  oi show utop\n\
            \  oi show --tree utop\n\
            \  sudo apt install \\$(oi show --only-depexts @avsm/tangled)\n\
            \  oi show --only-depexts --os=fedora-43\n\
            \  oi show --all\n\
            \  oi show @avsm @samoht";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.skip_local
      $ Terms.toolchain $ targets $ Terms.with_repos $ Terms.with_deps $ tree
      $ plan_view $ summary $ only_depexts $ os_override $ show_all
      $ show_cache_listing)
