[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "oi.plan"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* -- Action graph -------------------------------------------------------- *)

type node = {
  pkg : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  method_ : Identity.method_;
  deps : OpamPackage.Name.t list;
  layer_hash : string;
}

type graph = {
  nodes_by_name : node OpamPackage.Name.Map.t;
  topo_order : OpamPackage.Name.t list;
}

let build ctx ?d10 ~packages_dirs pkgs =
  let in_solution =
    List.fold_left
      (fun s p -> OpamPackage.Name.Set.add (OpamPackage.name p) s)
      OpamPackage.Name.Set.empty pkgs
  in
  let transitive_deps : (OpamPackage.Name.t, OpamPackage.Name.Set.t) Hashtbl.t =
    Hashtbl.create 64
  in
  let _installed, nodes_by_name, topo_order =
    List.fold_left
      (fun (installed, nodes, order) pkg ->
        let name = OpamPackage.name pkg in
        let dep_set =
          Solver.dep_names ~packages_dirs ~conf:(Solver.Ctx.conf ctx) pkg
            in_solution
        in
        let deps =
          dep_set |> OpamPackage.Name.Set.elements
          |> List.filter (fun n -> OpamPackage.Name.Set.mem n in_solution)
        in
        let opam =
          Solver.load_opam packages_dirs pkg
          |> Stdlib.Option.value ~default:(OpamFile.OPAM.create pkg)
        in
        let trans =
          List.fold_left
            (fun acc dep_name ->
              let acc = OpamPackage.Name.Set.add dep_name acc in
              match Hashtbl.find_opt transitive_deps dep_name with
              | Some s -> OpamPackage.Name.Set.union acc s
              | None -> acc)
            OpamPackage.Name.Set.empty deps
        in
        Hashtbl.replace transitive_deps name trans;
        (* Order must match day10's here:
           build the sub-DAG of pkg's transitive closure keyed by pkg with each
           entry's transitive-dep set as its value, iteratively peel leaves
           (alphabetical within each level via OpamPackage.Map ordering),
           reverse so the root comes first, then drop the root (pkg itself).
           Anything else produces a different layer_hash than day10. *)
        let all_dep_pkgs =
          let pkg_of_name n =
            OpamPackage.Name.Map.find_opt n nodes
            |> Stdlib.Option.map (fun node -> node.pkg)
          in
          let names_to_pkg_set s =
            OpamPackage.Name.Set.elements s
            |> List.filter_map pkg_of_name
            |> OpamPackage.Set.of_list
          in
          let dag =
            OpamPackage.Name.Set.fold
              (fun dep_name acc ->
                match pkg_of_name dep_name with
                | None -> acc
                | Some dep_pkg ->
                    let dep_trans =
                      Hashtbl.find_opt transitive_deps dep_name
                      |> Stdlib.Option.value ~default:OpamPackage.Name.Set.empty
                    in
                    OpamPackage.Map.add dep_pkg (names_to_pkg_set dep_trans) acc)
              trans OpamPackage.Map.empty
          in
          let dag = OpamPackage.Map.add pkg (names_to_pkg_set trans) dag in
          let rec topo_sort m =
            if OpamPackage.Map.is_empty m then []
            else
              let installable, remainder =
                OpamPackage.Map.partition
                  (fun _ deps -> OpamPackage.Set.is_empty deps)
                  m
              in
              if OpamPackage.Map.is_empty installable then []
              else
                let installable_list =
                  OpamPackage.Map.bindings installable |> List.map fst
                in
                let remainder =
                  OpamPackage.Map.map
                    (fun deps ->
                      List.fold_left
                        (fun acc p -> OpamPackage.Set.remove p acc)
                        deps installable_list)
                    remainder
                in
                installable_list @ topo_sort remainder
          in
          match topo_sort dag |> List.rev with [] -> [] | _ :: rest -> rest
        in
        let layer_hash = D10.Layer.hash ~packages_dirs (pkg :: all_dep_pkgs) in
        (* Non-relocatable toolchains bake their [install_prefix] into
           the binaries they produce (e.g. bytecode shebangs), so the
           layer hash must include the toolchain identity or stale
           binaries with dangling shebangs get reused from the cache. *)
        let layer_hash =
          match Solver.Ctx.toolchain ctx with
          | Some tc when not tc.relocatable ->
              Digest.string (layer_hash ^ ":" ^ tc.hash) |> Digest.to_hex
          | _ -> layer_hash
        in
        let method_ : Identity.method_ =
          match d10 with
          | Some d10 when D10.Layer.succeeded d10 ~hash:layer_hash -> Binary
          | _ -> Source
        in
        Log.debug (fun m ->
            m "Plan %s: deps=[%s]"
              (OpamPackage.to_string pkg)
              (String.concat ", " (List.map OpamPackage.Name.to_string deps)));
        let node = { pkg; opam; method_; deps; layer_hash } in
        let installed = OpamPackage.Name.Set.add name installed in
        (installed, OpamPackage.Name.Map.add name node nodes, order @ [ name ]))
      (OpamPackage.Name.Set.empty, OpamPackage.Name.Map.empty, [])
      pkgs
  in
  { nodes_by_name; topo_order }

(* -- Graph accessors ----------------------------------------------------- *)

let find g name = OpamPackage.Name.Map.find name g.nodes_by_name
let nodes g = List.map (find g) g.topo_order

(* Group [g]'s nodes into stages where every node in a stage can build
   in parallel: each stage contains nodes whose dependencies are all
   present in some earlier stage. Used by [pp_tree] and [resolve]. *)
let parallel_groups g =
  let assigned = Hashtbl.create (List.length g.topo_order) in
  let groups = ref [] in
  let remaining = ref g.topo_order in
  while !remaining <> [] do
    let ready, rest =
      List.partition
        (fun name ->
          let node = find g name in
          List.for_all (fun dep -> Hashtbl.mem assigned dep) node.deps)
        !remaining
    in
    let ready, rest =
      if ready = [] then
        match rest with [] -> ([], []) | hd :: tl -> ([ hd ], tl)
      else (ready, rest)
    in
    List.iter (fun name -> Hashtbl.replace assigned name true) ready;
    if ready <> [] then groups := ready :: !groups;
    remaining := rest
  done;
  List.rev !groups

let pp_method_short ~remote_has fmt : Identity.method_ -> unit = function
  | Binary -> Fmt.pf fmt "%a" Style.ok_string "binary"
  | Source ->
      if remote_has then Fmt.pf fmt "%a" Style.info_string "remote"
      else Fmt.pf fmt "%a" Style.source_string "source"

let pp_tree ?(remote_has = fun _ -> false) fmt g =
  let groups = parallel_groups g in
  let n_groups = List.length groups in
  Fmt.pf fmt "@[<v>";
  List.iteri
    (fun gi names ->
      let is_last_group = gi = n_groups - 1 in
      let cont = if is_last_group then "  " else "│ " in
      let n = List.length names in
      Fmt.pf fmt "%a %a@," Style.dim_string
        (Fmt.str "stage %d" (gi + 1))
        Style.dim_string
        (Fmt.str "(%d package%s)" n (if n = 1 then "" else "s"));
      List.iteri
        (fun i name ->
          let node = find g name in
          let pkg_s = OpamPackage.to_string node.pkg in
          let b = if i = n - 1 then "└── " else "├── " in
          Fmt.pf fmt "%s%s%a %a@," cont b Style.header_string pkg_s
            (pp_method_short ~remote_has:(remote_has node.layer_hash))
            node.method_)
        names)
    groups;
  Fmt.pf fmt "@]"

let layer_hashes g =
  List.map (fun name -> (find g name).layer_hash) g.topo_order

(* -- Executable plan ----------------------------------------------------- *)

type source_info = { url : string; checksums : string list }
type patch = { file : string; filter : string option }
type subst = string

type package_plan = {
  pkg : string;
  layer_hash : string;
  method_ : Identity.method_;
  dep_layers : Identity.dep list;
  source : source_info option;
  extra_sources : (string * source_info) list;
  extra_files : (string * string) list;
  patches : patch list;
  substs : subst list;
  subst_vars : (string * string) list;
  build_commands : string list list;
  install_commands : string list list;
  install_file : string;
  env : string array;
  build_dir : string;
  prefix : string;
  overlay : D10.Overlay.t option;
  opam_path : string option;
  pkgs_dir : string option;
  depexts : string list;
}

type group = { stage : int; packages : package_plan list }

type t = {
  groups : group list;
  cache_root : string;
  os_key : string;
  ocaml_version : string;
  build_prefix : string;
  total_packages : int;
}

let find_pkg_source_dir ~packages_dirs pkg =
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let full = OpamPackage.to_string pkg in
  List.find_opt
    (fun d -> Sys.file_exists (d / name / full / "opam"))
    packages_dirs

(* Resolve overlay attribution by deriving an [Origin.t] from the
   [packages/] directory the opam file was found in, then projecting onto
   its [overlay] field. Pins / local trees yield [None]. *)
let overlay_of_pkg ~packages_dirs pkg =
  match find_pkg_source_dir ~packages_dirs pkg with
  | None -> None
  | Some d ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let full = OpamPackage.to_string pkg in
      (Origin.of_packages_dir ~pkgs_dir:d ~name ~full).overlay

let resolve_groups ctx ~packages_dirs ~cache_root ~os_key:_ ~build_prefix g =
  let prefix = build_prefix in
  let groups = parallel_groups g in
  (* Non-relocatable toolchains live at a fixed prefix and are NOT
     built/installed into the consumer prefix — we drop them from
     each group here so Execute never tries to restore or re-build
     them. Keeping them in [g] (i.e. in the hash graph) is still
     important for layer_hash correctness: consumer layers must
     change when the toolchain does.

     Relocatable toolchains take the opposite path: their compiler
     IS built into the consumer prefix (matching [Solver.Ctx.create]'s
     [mark_installed] skip), so leave the packages in their build
     groups. The layer cache replays them like any other dep. *)
  let tc_pkgs =
    match Solver.Ctx.toolchain ctx with
    | None -> OpamPackage.Set.empty
    | Some tc when tc.relocatable -> OpamPackage.Set.empty
    | Some tc -> tc.packages
  in
  let is_toolchain_name name =
    match OpamPackage.Name.Map.find_opt name g.nodes_by_name with
    | None -> false
    | Some node -> OpamPackage.Set.mem node.pkg tc_pkgs
  in
  let mark_stage_installed names =
    List.iter
      (fun name ->
        match OpamPackage.Name.Map.find_opt name g.nodes_by_name with
        | None -> ()
        | Some node ->
            let config = Solver.Ctx.synthetic_config ctx node.pkg node.opam in
            Solver.Ctx.mark_installed ctx node.pkg node.opam config)
      names
  in
  List.mapi
    (fun i names ->
      let names = List.filter (fun n -> not (is_toolchain_name n)) names in
      let packages =
        List.map
          (fun name ->
            let node = find g name in
            let pkg = node.pkg in
            let opam = node.opam in
            let pkg_s = OpamPackage.to_string pkg in
            let layer_hash = node.layer_hash in
            let method_ = node.method_ in
            let dep_layers =
              List.filter_map
                (fun dep_name ->
                  match
                    OpamPackage.Name.Map.find_opt dep_name g.nodes_by_name
                  with
                  | None -> None
                  | Some n ->
                      Some (Identity.dep_of_opam n.pkg ~hash:n.layer_hash))
                node.deps
            in
            let source =
              OpamFile.OPAM.url opam
              |> Stdlib.Option.map (fun urlf ->
                  let url = OpamUrl.to_string (OpamFile.URL.url urlf) in
                  let checksums =
                    List.map OpamHash.to_string (OpamFile.URL.checksum urlf)
                  in
                  { url; checksums })
            in
            let extra_sources =
              List.map
                (fun (basename, urlf) ->
                  let name = OpamFilename.Base.to_string basename in
                  let url = OpamUrl.to_string (OpamFile.URL.url urlf) in
                  let checksums =
                    List.map OpamHash.to_string (OpamFile.URL.checksum urlf)
                  in
                  (name, { url; checksums }))
                (OpamFile.OPAM.extra_sources opam)
            in
            let extra_files =
              match OpamFile.OPAM.extra_files opam with
              | None -> []
              | Some xs ->
                  let pkg_source_dir = find_pkg_source_dir ~packages_dirs pkg in
                  List.filter_map
                    (fun (base, _hash) ->
                      let basename = OpamFilename.Base.to_string base in
                      match pkg_source_dir with
                      | None -> None
                      | Some d ->
                          let src =
                            d
                            / OpamPackage.Name.to_string (OpamPackage.name pkg)
                            / pkg_s / "files" / basename
                          in
                          if Sys.file_exists src then Some (basename, src)
                          else None)
                    xs
            in
            let patches =
              List.map
                (fun (base, filter) ->
                  let file = OpamFilename.Base.to_string base in
                  let filter = Stdlib.Option.map OpamFilter.to_string filter in
                  { file; filter })
                (OpamFile.OPAM.patches opam)
            in
            let substs =
              List.map OpamFilename.Base.to_string (OpamFile.OPAM.substs opam)
            in
            let short_hash =
              String.sub layer_hash 0 (min 12 (String.length layer_hash))
            in
            let build_dir =
              cache_root / "build" / "_build" / (pkg_s ^ "-" ^ short_hash)
            in
            let build_commands =
              Solver.Ctx.resolve_commands ctx ~test:false ~doc:false
                ~dev_setup:false ~build_dir opam
            in
            let install_commands =
              let env = Solver.Ctx.resolve ctx opam in
              OpamFilter.commands env (OpamFile.OPAM.install opam)
              |> List.filter_map (function [] -> None | cmd -> Some cmd)
            in
            let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
            let install_file = build_dir / (name_s ^ ".install") in
            let env = Solver.Ctx.compilation_env ctx opam in
            let subst_vars = Solver.Ctx.resolve_substs ctx opam in
            let pkgs_dir = find_pkg_source_dir ~packages_dirs pkg in
            let overlay =
              match pkgs_dir with
              | None -> None
              | Some d ->
                  (Origin.of_packages_dir ~pkgs_dir:d ~name:name_s ~full:pkg_s)
                    .overlay
            in
            let opam_path =
              Stdlib.Option.map (fun d -> d / name_s / pkg_s / "opam") pkgs_dir
            in
            (* Active depexts under the solver's filter env: declared
               [depexts:] entries whose filter evaluates true. Returned as
               opaque package-name strings — host install status is computed
               separately by {!Execute}. *)
            let depexts =
              let env = Solver.Ctx.platform_env ctx in
              List.fold_left
                (fun acc (pkgs, filter) ->
                  if OpamFilter.eval_to_bool ~default:false env filter then
                    OpamSysPkg.Set.union acc pkgs
                  else acc)
                OpamSysPkg.Set.empty
                (OpamFile.OPAM.depexts opam)
              |> OpamSysPkg.Set.elements
              |> List.map OpamSysPkg.to_string
            in
            {
              pkg = pkg_s;
              layer_hash;
              method_;
              dep_layers;
              source;
              extra_sources;
              extra_files;
              patches;
              substs;
              subst_vars;
              build_commands;
              install_commands;
              install_file;
              env;
              build_dir;
              prefix;
              overlay;
              opam_path;
              pkgs_dir;
              depexts;
            })
          names
      in
      mark_stage_installed names;
      { stage = i + 1; packages })
    groups

let resolve ctx ~packages_dirs ~cache_root ~os_key ~ocaml_version ?build_prefix
    g =
  let build_prefix =
    match build_prefix with
    | Some p -> p
    | None -> cache_root / "build" / "prefix"
  in
  let groups =
    resolve_groups ctx ~packages_dirs ~cache_root ~os_key ~build_prefix g
  in
  let total_packages =
    List.fold_left (fun acc g -> acc + List.length g.packages) 0 groups
  in
  { groups; cache_root; os_key; ocaml_version; build_prefix; total_packages }

(* -- Pretty-printing ----------------------------------------------------- *)

let pp_commands fmt cmds =
  List.iter (fun cmd -> Fmt.pf fmt "    %s@," (String.concat " " cmd)) cmds

let pp_package ~os_key fmt p =
  let short_hash h = String.sub h 0 (min 12 (String.length h)) in
  let method_s =
    match p.method_ with
    | Identity.Source -> "source"
    | Binary -> "binary (cached)"
  in
  Fmt.pf fmt "@[<v>  %a [%s]@," Style.header_string p.pkg method_s;
  Fmt.pf fmt "    layer: %s/%s@," os_key (short_hash p.layer_hash);
  if p.dep_layers <> [] then begin
    Fmt.pf fmt "    needs:@,";
    List.iter
      (fun (d : Identity.dep) ->
        Fmt.pf fmt "      %s → %s/%s@," (Identity.to_string d.id) os_key
          (short_hash d.hash))
      p.dep_layers
  end;
  (match p.source with
  | None -> ()
  | Some s ->
      Fmt.pf fmt "    source: %s@," s.url;
      List.iter (fun c -> Fmt.pf fmt "      %s@," c) s.checksums);
  List.iter
    (fun (name, s) -> Fmt.pf fmt "    extra-source %s: %s@," name s.url)
    p.extra_sources;
  List.iter
    (fun patch ->
      Fmt.pf fmt "    patch: %s%s@," patch.file
        (match patch.filter with None -> "" | Some f -> Fmt.str " {%s}" f))
    p.patches;
  List.iter (fun s -> Fmt.pf fmt "    subst: %s.in@," s) p.substs;
  if p.build_commands <> [] then begin
    Fmt.pf fmt "    build:@,";
    pp_commands fmt p.build_commands
  end;
  if p.install_commands <> [] then begin
    Fmt.pf fmt "    install:@,";
    pp_commands fmt p.install_commands
  end;
  Fmt.pf fmt "    build-dir: %s@," p.build_dir;
  Fmt.pf fmt "    prefix: %s@," p.prefix;
  Fmt.pf fmt "    env: (%d vars)@," (Array.length p.env);
  if p.subst_vars <> [] then begin
    Fmt.pf fmt "    subst-vars:@,";
    List.iter
      (fun (k, v) ->
        let v_short =
          if String.length v > 60 then String.sub v 0 57 ^ "..." else v
        in
        Fmt.pf fmt "      %s=%s@," k v_short)
      p.subst_vars
  end;
  Fmt.pf fmt "@]"

let pp fmt t =
  Fmt.pf fmt
    "@[<v>Plan: %d packages for %s (ocaml %s)@,Build root: %s/build/@,@,"
    t.total_packages t.os_key t.ocaml_version t.cache_root;
  List.iter
    (fun g ->
      let n = List.length g.packages in
      Fmt.pf fmt "%a %a@," Style.dim_string
        (Fmt.str "stage %d" g.stage)
        Style.dim_string
        (Fmt.str "(%d package%s, parallel)" n (if n = 1 then "" else "s"));
      List.iter (fun p -> pp_package ~os_key:t.os_key fmt p) g.packages;
      Fmt.pf fmt "@,")
    t.groups;
  Fmt.pf fmt "@]"
