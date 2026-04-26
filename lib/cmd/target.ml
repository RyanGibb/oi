let ( / ) = Filename.concat

let log_overlay fmt = Fmt.kstr (fun s -> Logs.debug (fun m -> m "%s" s)) fmt

(* -- Extras (CLI URL → Oi.Project.extra_repo) ---------------------------- *)

let cli_extra_repo_of_url url_s : Oi.Project.extra_repo =
  let hash = Digest.string url_s |> Digest.to_hex in
  let name = "extra-" ^ String.sub hash 0 10 in
  { name; url = url_s }

let is_url_like s =
  List.exists
    (fun p -> String.starts_with ~prefix:p s)
    [ "http://"; "https://"; "git+"; "git://"; "git@"; "file://"; "./"; "/" ]
  || String.contains s '/'

let overlay_extras_of_handles ~fs ~sys handles =
  if handles = [] then []
  else begin
    let path = Terms.reporepo_path () in
    let url = Terms.reporepo_url () in
    Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path ~url;
    log_overlay "resolving handles %s against reporepo %s"
      (String.concat ", " handles)
      path;
    let entries = Oi.Source.Reporepo.load ~path in
    let roots =
      List.rev handles
      |> List.map (fun h : Oi.Source.Reporepo.root ->
          { handle = h; version = None })
    in
    let resolved =
      Oi.Source.Reporepo.resolve entries ~roots
      (* Resolve returns deps-first (topological). The opam solver's
         packages_dirs fold is first-wins on name collisions, so we
         reverse to get dependents-first: [samoht, relocatable, default]
         means samoht wins over relocatable wins over default. *)
      |> List.rev
    in
    log_overlay "overlay closure (highest priority first): %s"
      (String.concat ", "
         (List.map
            (fun (e : Oi.Source.Reporepo.entry) ->
              Fmt.str "%s.%s@%s" e.handle e.version
                (String.sub e.commit 0 (min 7 (String.length e.commit))))
            resolved));
    List.map
      (fun (e : Oi.Source.Reporepo.entry) ->
        let url = if e.commit = "" then e.url else e.url ^ "#" ^ e.commit in
        let name = "overlay-" ^ e.handle ^ "-" ^ e.version in
        { Oi.Project.name; url })
      resolved
  end

let cli_extra_repos ~fs ~sys tokens =
  let urls, handles = List.partition is_url_like tokens in
  overlay_extras_of_handles ~fs ~sys handles
  @ List.map cli_extra_repo_of_url urls

(* -- Build target classification ---------------------------------------- *)

type build_target =
  | Plain_target of string
  | Overlay_pkg of string * string
  | Overlay_all of string

let is_handle_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let parse_build_target s =
  if String.length s < 2 || s.[0] <> '@' then Plain_target s
  else
    let rest = String.sub s 1 (String.length s - 1) in
    match String.index_opt rest '/' with
    | None ->
        if String.for_all is_handle_char rest && rest <> "" then
          Overlay_all rest
        else Plain_target s
    | Some i ->
        let handle = String.sub rest 0 i in
        let pkg = String.sub rest (i + 1) (String.length rest - i - 1) in
        if String.for_all is_handle_char handle && handle <> "" && pkg <> ""
        then Overlay_pkg (handle, pkg)
        else Plain_target s

(* -- Handle pins -------------------------------------------------------- *)

type handle_pin = {
  handle : string;
  pkg : OpamPackage.Name.t;
  user_constr : OpamFormula.version_constraint option;
}

let split_handle_prefix s =
  if String.length s < 2 || s.[0] <> '@' then None
  else
    let rest = String.sub s 1 (String.length s - 1) in
    match String.index_opt rest '/' with
    | None ->
        if String.for_all is_handle_char rest && rest <> "" then
          Oi.Error.config_error
            "overlay handle %S given without a package (use '@%s/PKG')" rest
            rest
        else None
    | Some i ->
        let handle = String.sub rest 0 i in
        let pkg_spec = String.sub rest (i + 1) (String.length rest - i - 1) in
        if (not (String.for_all is_handle_char handle)) || handle = "" then None
        else if pkg_spec = "" then
          Oi.Error.config_error
            "overlay handle %S given without a package (use '@%s/PKG')" handle
            handle
        else begin
          log_overlay "detected handle shortcut: %s -> target=%s" handle
            pkg_spec;
          Some (handle, pkg_spec)
        end

let extract_handle_pins ~with_repos tokens =
  let acc_repos = ref with_repos in
  let acc_pins = ref [] in
  let stripped =
    List.map
      (fun t ->
        match split_handle_prefix t with
        | None -> t
        | Some (h, pkg_spec) ->
            let pkg, user_constr = OpamFormula.atom_of_string pkg_spec in
            acc_repos := !acc_repos @ [ h ];
            acc_pins := !acc_pins @ [ { handle = h; pkg; user_constr } ];
            pkg_spec)
      tokens
  in
  (stripped, !acc_repos, !acc_pins)

let latest_version_in_dirs ~pkg dirs =
  let prefix = pkg ^ "." in
  let versions =
    List.concat_map
      (fun d ->
        let subdir = d / pkg in
        if not (Sys.file_exists subdir) then []
        else
          Sys.readdir subdir |> Array.to_list
          |> List.filter_map (fun entry ->
              if String.starts_with ~prefix entry then
                Some
                  (String.sub entry (String.length prefix)
                     (String.length entry - String.length prefix))
              else None))
      dirs
    |> List.sort_uniq String.compare
  in
  match versions with
  | [] -> None
  | _ ->
      Some
        (List.fold_left
           (fun a v ->
             if
               OpamPackage.Version.compare
                 (OpamPackage.Version.of_string v)
                 (OpamPackage.Version.of_string a)
               > 0
             then v
             else a)
           (List.hd versions) (List.tl versions))

let handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins =
  if handle_pins = [] then OpamPackage.Name.Map.empty
  else
    let overlay_pkg_dirs =
      Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh cli_extras
    in
    List.fold_left
      (fun acc { handle; pkg; user_constr } ->
        match user_constr with
        | Some c -> OpamPackage.Name.Map.add pkg c acc
        | None -> (
            let pkg_s = OpamPackage.Name.to_string pkg in
            match latest_version_in_dirs ~pkg:pkg_s overlay_pkg_dirs with
            | None ->
                Oi.Error.config_error
                  "overlay %s does not provide a package named %s" handle pkg_s
            | Some v ->
                log_overlay "pinning %s = %s from overlay %s" pkg_s v handle;
                OpamPackage.Name.Map.add pkg
                  (`Eq, OpamPackage.Version.of_string v)
                  acc))
      OpamPackage.Name.Map.empty handle_pins

let merge_extras ~cli ~project =
  let seen = Hashtbl.create 8 in
  let acc = ref [] in
  let push (e : Oi.Project.extra_repo) =
    if not (Hashtbl.mem seen e.name) then begin
      Hashtbl.add seen e.name ();
      acc := e :: !acc
    end
  in
  List.iter push project;
  List.iter push cli;
  List.rev !acc

let parse_pkg_target s =
  match OpamPackage.of_string_opt s with
  | Some pkg -> (OpamPackage.name pkg, Some (`Eq, OpamPackage.version pkg))
  | None -> OpamFormula.atom_of_string s
