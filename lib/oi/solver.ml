[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.solver"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Dir_context (was lib/oi/dir_context.ml) ----------------------------- *)

(* Multi-directory Dir_context for opam-0install with first-wins lookup
   across multiple [packages_dirs]. Private to this module. *)
module Dir_context = struct
  type rejection =
    | UserConstraint of OpamFormula.atom
    | Unavailable [@warning "-37"]

  let with_dir path fn =
    let ch = Unix.opendir path in
    Fun.protect ~finally:(fun () -> Unix.closedir ch) (fun () -> fn ch)

  let list_dir path =
    let rec aux acc ch =
      match Unix.readdir ch with
      | name when name.[0] <> '.' -> aux (name :: acc) ch
      | _ -> aux acc ch
      | exception End_of_file -> acc
    in
    with_dir path (aux [])

  type t = {
    env : string -> OpamVariable.variable_contents option;
    packages_dirs : string list;
    pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
    constraints : OpamFormula.version_constraint OpamTypes.name_map;
    test : OpamPackage.Name.Set.t;
    doc : OpamPackage.Name.Set.t;
    prefer_oldest : bool;
  }

  let load t pkg =
    let { OpamPackage.name; version = _ } = pkg in
    match OpamPackage.Name.Map.find_opt name t.pins with
    | Some (_, opam) -> opam
    | None ->
        List.find_map
          (fun packages_dir ->
            let opam =
              packages_dir
              / OpamPackage.Name.to_string name
              / OpamPackage.to_string pkg / "opam"
            in
            if Sys.file_exists opam then Some opam else None)
          t.packages_dirs
        |> Option.get |> OpamFilename.raw |> OpamFile.make |> OpamFile.OPAM.read

  let user_restrictions t name =
    OpamPackage.Name.Map.find_opt name t.constraints

  let dev = OpamPackage.Version.of_string "dev"

  let env t pkg v =
    if List.mem v OpamPackageVar.predefined_depends_variables then None
    else
      match OpamVariable.Full.to_string v with
      | "version" ->
          Some
            (OpamTypes.S
               (OpamPackage.Version.to_string (OpamPackage.version pkg)))
      | x -> t.env x

  let filter_deps t pkg f =
    let dev = OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
    let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
    let doc = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.doc in
    f
    |> OpamFilter.partial_filter_formula (env t pkg)
    |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc ~dev
         ~dev_setup:false ~default:false

  let version_compare t (v1, v1_avoid, _) (v2, v2_avoid, _) =
    match (v1_avoid, v2_avoid) with
    | true, true | false, false ->
        if t.prefer_oldest then OpamPackage.Version.compare v1 v2
        else OpamPackage.Version.compare v2 v1
    | true, false -> 1
    | false, true -> -1

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.pins with
    | Some (version, opam) -> [ (version, Ok opam) ]
    | None ->
        let versions =
          List.concat_map
            (fun packages_dir ->
              try packages_dir / OpamPackage.Name.to_string name |> list_dir
              with Unix.Unix_error (Unix.ENOENT, _, _) -> [])
            t.packages_dirs
          |> List.sort_uniq compare
        in
        let user_constraints = user_restrictions t name in
        versions
        |> List.filter_map (fun dir ->
            match OpamPackage.of_string_opt dir with
            | Some pkg ->
                List.find_opt
                  (fun packages_dir ->
                    Sys.file_exists
                      (packages_dir
                      / OpamPackage.Name.to_string name
                      / dir / "opam"))
                  t.packages_dirs
                |> Option.map (fun _ -> OpamPackage.version pkg)
            | _ -> None)
        |> List.filter_map (fun v ->
            let pkg = OpamPackage.create name v in
            let opam = load t pkg in
            let avoid = OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam in
            let available = OpamFile.OPAM.available opam in
            match
              OpamFilter.eval_to_bool ~default:false (env t pkg) available
            with
            | true -> Some (v, avoid, opam)
            | false -> None)
        |> (fun l ->
        if List.for_all (fun (_, avoid, _) -> avoid) l then [] else l)
        |> List.sort (version_compare t)
        |> List.map (fun (v, _, opam) ->
            match user_constraints with
            | Some test
              when not
                     (OpamFormula.check_version_formula (OpamFormula.Atom test)
                        v) ->
                (v, Error (UserConstraint (name, Some test)))
            | _ -> (v, Ok opam))

  let pp_rejection f = function
    | UserConstraint x ->
        Fmt.pf f "Rejected by user-specified constraint %s"
          (OpamFormula.string_of_atom x)
    | Unavailable -> Fmt.string f "Availability condition not satisfied"

  let create ?(prefer_oldest = false) ?(test = OpamPackage.Name.Set.empty)
      ?(doc = OpamPackage.Name.Set.empty) ?(pins = OpamPackage.Name.Map.empty)
      ~constraints ~env packages_dirs =
    { env; packages_dirs; pins; constraints; test; doc; prefer_oldest }
end

(* -- Synthetic switch state (was lib/oi/opam_ctx.ml) --------------------- *)

module Ctx = struct
  let log_src = Logs.Src.create "oi.solver.ctx"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  type conf = {
    arch : string;
    os : string;
    os_distribution : string;
    os_version : string;
    os_family : string;
    ocaml_version : string;
    jobs : int;
  }

  let pp_conf fmt c =
    Fmt.pf fmt
      "@[<v>arch:            %s@,\
       os:              %s@,\
       os-distribution: %s@,\
       os-version:      %s@,\
       os-family:       %s@,\
       ocaml-version:   %s@,\
       jobs:            %d@]"
      c.arch c.os c.os_distribution c.os_version c.os_family c.ocaml_version
      c.jobs

  type toolchain = {
    install_prefix : string;
    hash : string;
    relocatable : bool;
    packages : OpamPackage.Set.t;
    root_names : OpamPackage.Name.Set.t;
  }

  type t = {
    mutable st : OpamStateTypes.rw OpamStateTypes.switch_state;
    conf : conf;
    prefix : string;
    toolchain : toolchain option;
  }

  let load_cache : (string, OpamFile.OPAM.t OpamPackage.Map.t) Hashtbl.t =
    Hashtbl.create 8

  let load_opams_from_dir dir =
    match Hashtbl.find_opt load_cache dir with
    | Some m -> m
    | None ->
        let opams = ref OpamPackage.Map.empty in
        if Sys.file_exists dir then
          Array.iter
            (fun name_s ->
              let name_dir = dir / name_s in
              if Sys.is_directory name_dir then
                Array.iter
                  (fun pkg_s ->
                    let pkg_dir = name_dir / pkg_s in
                    let opam_path = pkg_dir / "opam" in
                    if Sys.file_exists opam_path then
                      try
                        let opam =
                          OpamFile.OPAM.read
                            (OpamFile.make (OpamFilename.raw opam_path))
                        in
                        let pkg = OpamPackage.of_string pkg_s in
                        opams := OpamPackage.Map.add pkg opam !opams
                      with _ ->
                        Log.debug (fun m -> m "Could not parse %s" opam_path))
                  (Sys.readdir name_dir))
            (Sys.readdir dir);
        let m = !opams in
        Hashtbl.add load_cache dir m;
        m

  let _global_state :
      OpamStateTypes.unlocked OpamStateTypes.global_state option ref =
    ref None

  let init_opam ~root =
    Unix.putenv "OPAMROOT" root;
    OpamSystem.init ();
    OpamCoreConfig.init ~yes:(Some true) ~verbose_level:0 ~debug_level:0
      ~disp_status_line:`Never ();
    OpamFormatConfig.init ();
    OpamSolverConfig.init ();
    OpamClientConfig.init ();
    let root_dir = OpamFilename.Dir.of_string root in
    OpamStateConfig.update ~no_depexts:true ~root_dir ();
    OpamFilename.mkdir root_dir;
    let config_path = OpamPath.config root_dir in
    if not (OpamFile.exists config_path) then begin
      let config =
        OpamFile.Config.empty
        |> OpamFile.Config.with_opam_version (OpamVersion.of_string "2.0")
        |> OpamFile.Config.with_opam_root_version OpamFile.Config.root_version
      in
      OpamFile.Config.write config_path config
    end;
    let gt = OpamGlobalState.load `Lock_none in
    _global_state := Some (OpamGlobalState.unlock gt)

  let conf t = t.conf
  let prefix t = t.prefix
  let toolchain t = t.toolchain

  let switch_env ?toolchain ~prefix () =
    let no_toolchain_env () =
      [
        ("OPAM_SWITCH_PREFIX", prefix);
        ("OCAMLLIB", prefix / "lib" / "ocaml");
        ( "CAML_LD_LIBRARY_PATH",
          String.concat ":"
            [
              prefix / "lib" / "stublibs"; prefix / "lib" / "ocaml" / "stublibs";
            ] );
        ("OCAMLFIND_CONF", prefix / "lib" / "findlib.conf");
        ("OCAMLFIND_DESTDIR", prefix / "lib");
        ( "OCAMLPATH",
          String.concat ":" [ prefix / "lib"; prefix / "lib" / "ocaml" ] );
        ("OCAMLTOP_INCLUDE_PATH", prefix / "lib" / "toplevel");
        ("OCAML_TOPLEVEL_PATH", prefix / "lib" / "toplevel");
      ]
    in
    match toolchain with
    | None -> no_toolchain_env ()
    | Some { relocatable = true; _ } -> no_toolchain_env ()
    | Some tc ->
        [
          ("OPAM_SWITCH_PREFIX", prefix);
          ( "CAML_LD_LIBRARY_PATH",
            String.concat ":"
              [
                prefix / "lib" / "stublibs";
                prefix / "lib" / "ocaml" / "stublibs";
                tc.install_prefix / "lib" / "stublibs";
              ] );
          ("OCAMLFIND_DESTDIR", prefix / "lib");
          ( "OCAMLPATH",
            String.concat ":"
              [
                prefix / "lib";
                prefix / "lib" / "ocaml";
                tc.install_prefix / "lib";
              ] );
          ("OCAMLTOP_INCLUDE_PATH", prefix / "lib" / "toplevel");
          ("OCAML_TOPLEVEL_PATH", prefix / "lib" / "toplevel");
        ]

  let create ~prefix ~packages_dirs ~conf ?toolchain () =
    let loaded_gt =
      match !_global_state with
      | Some gt -> gt
      | None -> Fmt.failwith "Solver.Ctx.init_opam must be called before create"
    in
    let prefix = try Unix.realpath prefix with Unix.Unix_error _ -> prefix in
    let switch_parent = Filename.dirname prefix in
    let switch_name = Filename.basename prefix in
    let root = OpamFilename.Dir.of_string switch_parent in
    let switch = OpamSwitch.of_string switch_name in
    OpamFilename.mkdir root;
    OpamFilename.mkdir (OpamFilename.Dir.of_string prefix);
    let meta_dir = OpamFilename.Dir.of_string (prefix / ".opam-switch") in
    OpamFilename.mkdir meta_dir;
    List.iter
      (fun sub ->
        OpamFilename.mkdir
          (OpamFilename.Dir.of_string (prefix / ".opam-switch" / sub)))
      [ "build"; "install"; "config"; "sources"; "packages" ];
    let config =
      OpamFile.Config.empty
      |> OpamFile.Config.with_opam_version (OpamVersion.of_string "2.0")
      |> OpamFile.Config.with_opam_root_version OpamFile.Config.root_version
      |> OpamFile.Config.with_installed_switches [ switch ]
    in
    let switch_config =
      let sc = OpamFile.Switch_config.empty in
      let base_env =
        List.map
          (fun (k, v) ->
            OpamTypesBase.env_update_resolved k Eq v ~comment:"switch env")
          (switch_env ?toolchain ~prefix ())
      in
      let env =
        match toolchain with
        | None | Some { relocatable = true; _ } -> base_env
        | Some tc ->
            OpamTypesBase.env_update_resolved "PATH" PlusEq
              (tc.install_prefix / "bin")
              ~comment:"toolchain bin"
            :: base_env
      in
      { sc with env }
    in
    OpamStateConfig.update ~root_dir:root ();
    let all_opams =
      List.fold_left
        (fun acc dir ->
          OpamPackage.Map.union (fun a _ -> a) acc (load_opams_from_dir dir))
        OpamPackage.Map.empty packages_dirs
    in
    let all_packages = OpamPackage.keys all_opams in
    let global_variables =
      let open OpamTypes in
      let var s value desc =
        (OpamVariable.of_string s, (lazy (Some value), desc))
      in
      List.fold_left
        (fun m (k, v) -> OpamVariable.Map.add k v m)
        loaded_gt.global_variables
        [
          var "sys-ocaml-version" (S conf.ocaml_version) "OCaml version";
          var "sys-ocaml-arch" (S conf.arch) "OCaml arch";
          var "sys-ocaml-cc" (S "cc") "OCaml C compiler";
          var "sys-ocaml-libc" (S "libc") "OCaml C library";
          var "jobs" (S (string_of_int conf.jobs)) "Parallel jobs";
          var "make" (S "make") "Make command";
          var "prefix" (S prefix) "Switch prefix";
          var "os" (S conf.os) "Operating system";
          var "os-distribution" (S conf.os_distribution) "OS distribution";
          var "os-family" (S conf.os_family) "OS family";
          var "os-version" (S conf.os_version) "OS version";
          var "arch" (S conf.arch) "Architecture";
        ]
    in
    let gt : OpamStateTypes.unlocked OpamStateTypes.global_state =
      { loaded_gt with root; config; global_variables }
    in
    let rt : OpamStateTypes.unlocked OpamStateTypes.repos_state =
      {
        repos_lock = OpamSystem.lock_none;
        repos_global = gt;
        repositories = OpamRepositoryName.Map.empty;
        repos_definitions = OpamRepositoryName.Map.empty;
        repo_opams = OpamRepositoryName.Map.empty;
        repos_tmp = Hashtbl.create 0;
      }
    in
    let st : OpamStateTypes.rw OpamStateTypes.switch_state =
      {
        switch_lock = OpamSystem.lock_none;
        switch_global = gt;
        switch_repos = rt;
        switch;
        switch_invariant = OpamFormula.Empty;
        compiler_packages = OpamPackage.Set.empty;
        switch_config;
        repos_package_index = all_opams;
        opams = all_opams;
        conf_files = OpamPackage.Name.Map.empty;
        packages = all_packages;
        sys_packages = lazy OpamPackage.Map.empty;
        available_packages =
          lazy
            (OpamSwitchState.compute_available_packages gt switch switch_config
               ~pinned:OpamPackage.Set.empty ~opams:all_opams);
        pinned = OpamPackage.Set.empty;
        installed = OpamPackage.Set.empty;
        installed_opams = OpamPackage.Map.empty;
        installed_roots = OpamPackage.Set.empty;
        reinstall = lazy OpamPackage.Set.empty;
        invalidated = lazy OpamPackage.Set.empty;
        overwrote_opams = OpamPackage.Map.empty;
      }
    in
    let st =
      match toolchain with
      | None | Some { relocatable = true; _ } -> st
      | Some tc ->
          OpamPackage.Set.fold
            (fun pkg st ->
              match OpamPackage.Map.find_opt pkg all_opams with
              | None -> st
              | Some opam ->
                  let name = OpamPackage.name pkg in
                  let config =
                    match OpamPackage.Name.to_string name with
                    | "ocaml" ->
                        let var s = OpamVariable.of_string s in
                        let b v = OpamTypes.B v in
                        let s v = OpamTypes.S v in
                        Some
                          (OpamFile.Dot_config.create
                             [
                               (var "native", b true);
                               (var "native-tools", b true);
                               (var "native-dynlink", b true);
                               (var "preinstalled", b false);
                               (var "compiler", s "");
                             ])
                    | _ -> None
                  in
                  let open OpamStateTypes in
                  {
                    st with
                    installed = OpamPackage.Set.add pkg st.installed;
                    installed_opams =
                      OpamPackage.Map.add pkg opam st.installed_opams;
                    conf_files =
                      (match config with
                      | Some c -> OpamPackage.Name.Map.add name c st.conf_files
                      | None -> st.conf_files);
                  })
            tc.packages st
    in
    { st; conf; prefix; toolchain }

  let switch_state t =
    (Obj.magic t.st : OpamStateTypes.unlocked OpamStateTypes.switch_state)

  let resolve t opam = OpamPackageVar.resolve ~opam t.st

  let platform_env t v =
    let s = OpamVariable.Full.to_string v in
    match
      OpamVariable.Map.find_opt (OpamVariable.of_string s)
        t.st.switch_global.global_variables
    with
    | Some ((lazy value), _) -> value
    | None -> None

  let compilation_env t opam =
    let build_env =
      List.map
        (fun env ->
          OpamEnv.resolve_separator_and_format
            (OpamEnv.env_expansion ~opam t.st env))
        (OpamFile.OPAM.build_env opam)
    in
    let updates =
      [
        OpamTypesBase.env_update_resolved "CDPATH" Eq "" ~comment:"sanitize";
        OpamTypesBase.env_update_resolved "MAKEFLAGS" Eq "" ~comment:"sanitize";
        OpamTypesBase.env_update_resolved "MAKELEVEL" Eq "" ~comment:"sanitize";
        OpamTypesBase.env_update_resolved "OCAML_TOPLEVEL_PATH" Eq
          (t.prefix / "lib" / "toplevel")
          ~comment:"sanitize";
        OpamTypesBase.env_update_resolved "OPAM_PACKAGE_NAME" Eq
          (OpamPackage.Name.to_string (OpamFile.OPAM.name opam))
          ~comment:"build env";
        OpamTypesBase.env_update_resolved "OPAM_PACKAGE_VERSION" Eq
          (OpamPackage.Version.to_string (OpamFile.OPAM.version opam))
          ~comment:"build env";
        OpamTypesBase.env_update_resolved "OPAMCLI" Eq "2.0"
          ~comment:"opam CLI version";
      ]
      @ build_env
    in
    OpamEnv.get_full ~set_opamroot:true ~set_opamswitch:true ~force_path:true
      t.st ~updates
    |> OpamTypesBase.env_array

  let resolve_substs t opam =
    let env = OpamPackageVar.resolve ~opam t.st in
    let vars = Hashtbl.create 32 in
    let record_var v =
      let full = OpamVariable.Full.of_string v in
      let value =
        match env full with
        | Some (OpamTypes.S s) -> s
        | Some (OpamTypes.B b) -> string_of_bool b
        | Some (OpamTypes.L l) -> String.concat " " l
        | None -> ""
      in
      Hashtbl.replace vars v value;
      value
    in
    let _subst_basenames = OpamFile.OPAM.substs opam in
    List.iter
      (fun v -> ignore (record_var v))
      [
        "name";
        "version";
        "prefix";
        "lib";
        "bin";
        "share";
        "doc";
        "etc";
        "man";
        "sbin";
        "make";
        "jobs";
        "os";
        "arch";
        "ocaml:version";
        "ocaml:native";
        "ocaml:native-dynlink";
        "opam-version";
      ];
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) vars []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)

  let resolve_commands t ~test ~doc ~dev_setup ?build_dir opam =
    let local =
      let base =
        [
          (OpamVariable.of_string "with-test", Some (OpamTypes.B test));
          (OpamVariable.of_string "with-doc", Some (OpamTypes.B doc));
          (OpamVariable.of_string "with-dev-setup", Some (OpamTypes.B dev_setup));
        ]
      in
      let with_build =
        match build_dir with
        | None -> base
        | Some d ->
            (OpamVariable.of_string "build", Some (OpamTypes.S d)) :: base
      in
      OpamVariable.Map.of_list with_build
    in
    let env = OpamPackageVar.resolve ~opam ~local t.st in
    OpamFilter.commands env (OpamFile.OPAM.build opam)
    |> List.filter_map (function [] -> None | cmd -> Some cmd)

  let mark_installed t pkg opam config =
    let name = OpamPackage.name pkg in
    t.st <-
      {
        t.st with
        installed = OpamPackage.Set.add pkg t.st.installed;
        installed_opams = OpamPackage.Map.add pkg opam t.st.installed_opams;
        conf_files =
          (match config with
          | Some c -> OpamPackage.Name.Map.add name c t.st.conf_files
          | None -> t.st.conf_files);
      }

  let synthetic_config _t pkg _opam =
    let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
    match name with
    | "ocaml" ->
        let var s = OpamVariable.of_string s in
        let s v = OpamTypes.S v in
        let b v = OpamTypes.B v in
        Some
          (OpamFile.Dot_config.create
             [
               (var "native", b true);
               (var "native-tools", b true);
               (var "native-dynlink", b true);
               (var "preinstalled", b false);
               (var "compiler", s "");
             ])
    | _ -> None
end

(* -- Prefix env wrappers (was lib/oi/prefix.ml) -------------------------- *)

module Env = struct
  let canonical p = try Unix.realpath p with Unix.Unix_error _ -> p

  let env_vars ?toolchain ~prefix ~dune_cache_root () =
    let prefix = canonical prefix in
    Ctx.switch_env ?toolchain ~prefix ()
    @ [
        ("OCAMLFIND_DESTDIR", prefix / "lib");
        ("DUNE_CACHE", "enabled");
        ("DUNE_CACHE_ROOT", dune_cache_root);
        ("DUNE_CACHE_STORAGE_MODE", "hardlink");
      ]

  let envrc_content ?toolchain ~prefix ?tools ~dune_cache_root () =
    let prefix = canonical prefix in
    let tools = Option.map canonical tools in
    let switch_vars =
      List.map
        (fun (k, v) -> Fmt.str "export %s=\"%s\"" k v)
        (Ctx.switch_env ?toolchain ~prefix ())
    in
    let header =
      [
        "# Generated by oi sync — do not edit";
        Fmt.str "OI_PREFIX=$(expand_path %s)" prefix;
        Fmt.str "OI_CACHE=$(expand_path %s)" dune_cache_root;
      ]
      @ (match tools with
        | None -> []
        | Some t -> [ Fmt.str "OI_TOOLS=$(expand_path %s)" t ])
      @ [ "" ]
    in
    let path_adds =
      (match tools with
        | None -> []
        | Some _ -> [ "PATH_add \"$OI_TOOLS/bin\"" ])
      @ [ "PATH_add \"$OI_PREFIX/bin\"" ]
      @
      match toolchain with
      | None | Some { Ctx.relocatable = true; _ } -> []
      | Some (tc : Ctx.toolchain) ->
          [ Fmt.str "PATH_add \"%s/bin\"" tc.install_prefix ]
    in
    String.concat "\n"
      (header @ path_adds @ switch_vars
      @ [
          Fmt.str "export OCAMLFIND_DESTDIR=\"$OI_PREFIX/lib\"";
          "export DUNE_CACHE=enabled";
          Fmt.str "export DUNE_CACHE_ROOT=\"$OI_CACHE\"";
          "export DUNE_CACHE_STORAGE_MODE=hardlink";
          "";
        ])

  let make_env ?toolchain ~prefix ?tools ~dune_cache_root () =
    let vars = env_vars ?toolchain ~prefix ~dune_cache_root () in
    let dominated =
      "PATH" :: List.map (fun (k, _) -> String.uppercase_ascii k) vars
    in
    let has_prefix s k =
      let k = k ^ "=" in
      String.length s >= String.length k
      && String.uppercase_ascii (String.sub s 0 (String.length k)) = k
    in
    let current_path =
      try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin"
    in
    let base =
      Unix.environment () |> Array.to_list
      |> List.filter (fun s -> not (List.exists (has_prefix s) dominated))
    in
    let tools_bin = Option.map (fun t -> canonical t / "bin") tools in
    let toolchain_bin =
      match toolchain with
      | None | Some { Ctx.relocatable = true; _ } -> None
      | Some (tc : Ctx.toolchain) -> Some (tc.install_prefix / "bin")
    in
    let path_components =
      List.filter_map Fun.id [ tools_bin; Some (prefix / "bin"); toolchain_bin ]
    in
    let path_entry =
      "PATH=" ^ String.concat ":" path_components ^ ":" ^ current_path
    in
    let env_pairs = path_entry :: List.map (fun (k, v) -> k ^ "=" ^ v) vars in
    Array.of_list (env_pairs @ base)
end

(* -- Persistent solve cache (was lib/oi/solve_cache.ml) ------------------ *)

module Cache = struct
  let log_src = Logs.Src.create "oi.solver.cache"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  (* Bumped to v6 with the working-tree-aware signature below: cache
     entries written under v5 keys are no longer trustworthy. *)
  let schema_version = "v6"
  let signature_memo : (string, string option) Hashtbl.t = Hashtbl.create 16

  let read_process_output cmd =
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 256 in
    let chunk = Bytes.create 4096 in
    let rec loop () =
      let n = input ic chunk 0 (Bytes.length chunk) in
      if n > 0 then begin
        Buffer.add_subbytes buf chunk 0 n;
        loop ()
      end
    in
    loop ();
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> Some (Buffer.contents buf)
    | _ -> None

  (* Working-tree-aware signature for [packages_dir]: combines
     [git rev-parse HEAD] (the committed tip) with a hash of
     [git status --porcelain] (tracked + untracked working-tree state)
     scoped to [packages_dir]. This catches reporepo working-tree edits
     that bumps make to [v1/<handle>/packages/] without committing —
     reusing the previous "tip-only" key cached stale solves once those
     edits stopped moving the parent commit. *)
  let compute_git_signature packages_dir =
    let repo = Filename.dirname packages_dir in
    let head =
      Option.map String.trim
        (read_process_output
           (Printf.sprintf "git -C %s rev-parse HEAD 2>/dev/null"
              (Filename.quote repo)))
    in
    let status =
      read_process_output
        (Printf.sprintf
           "git -C %s status --porcelain -- %s 2>/dev/null"
           (Filename.quote repo)
           (Filename.quote packages_dir))
    in
    match (head, status) with
    | Some h, Some s when h <> "" ->
        Some (h ^ ":" ^ Digest.to_hex (Digest.string s))
    | _ -> None

  let git_signature_for_packages_dir packages_dir =
    match Hashtbl.find_opt signature_memo packages_dir with
    | Some r -> r
    | None ->
        let r = compute_git_signature packages_dir in
        Hashtbl.add signature_memo packages_dir r;
        r

  let key ~(conf : Ctx.conf) ~packages_dirs ~constraints ~names ?toolchain () =
    let signatures =
      List.map (fun d -> (d, git_signature_for_packages_dir d)) packages_dirs
    in
    if List.exists (fun (_, s) -> s = None) signatures then begin
      List.iter
        (fun (d, s) ->
          if s = None then
            Log.info (fun m ->
                m "solve cache disabled: %s is not a git working tree" d))
        signatures;
      None
    end
    else
      let buf = Buffer.create 1024 in
      let add k v =
        Buffer.add_string buf k;
        Buffer.add_char buf '=';
        Buffer.add_string buf v;
        Buffer.add_char buf '\n'
      in
      add "schema" schema_version;
      add "arch" conf.arch;
      add "os" conf.os;
      add "os_distribution" conf.os_distribution;
      add "os_version" conf.os_version;
      add "os_family" conf.os_family;
      add "ocaml_version" conf.ocaml_version;
      List.iteri
        (fun i (d, sig_) ->
          add (Fmt.str "dir%d_path" i) d;
          add (Fmt.str "dir%d_sig" i)
            (match sig_ with Some s -> s | None -> assert false))
        signatures;
      let cs =
        OpamPackage.Name.Map.bindings constraints
        |> List.map (fun (name, (relop, version)) ->
            OpamPackage.Name.to_string name
            ^ " "
            ^ OpamFormula.string_of_relop relop
            ^ " "
            ^ OpamPackage.Version.to_string version)
        |> List.sort String.compare
      in
      List.iter (add "constraint") cs;
      let ns =
        List.map OpamPackage.Name.to_string names |> List.sort String.compare
      in
      List.iter (add "name") ns;
      (match toolchain with
      | None -> ()
      | Some (tc : Ctx.toolchain) -> add "toolchain_hash" tc.hash);
      Some (Digest.to_hex (Digest.string (Buffer.contents buf)))

  let cache_dir ~cache_root = cache_root / "solve-cache"

  let key_path ~cache_root ~sub key =
    let shard = String.sub key 0 2 in
    cache_dir ~cache_root / sub / shard / (key ^ ".marshal")

  let short_key k = String.sub k 0 (min 12 (String.length k))

  let read_marshal ~label path =
    if not (Sys.file_exists path) then None
    else
      match
        try
          let ic = open_in_bin path in
          Fun.protect
            ~finally:(fun () -> close_in_noerr ic)
            (fun () -> Some (Marshal.from_channel ic))
        with _ -> None
      with
      | Some _ as r ->
          Log.debug (fun m -> m "%s hit %s" label path);
          r
      | None ->
          Log.debug (fun m -> m "%s corrupt %s; unlinking" label path);
          (try Sys.remove path with _ -> ());
          None

  let write_marshal ~fs ~label path v =
    let dir = Filename.dirname path in
    try
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
      let tmp = path ^ ".tmp" in
      Out_channel.with_open_bin tmp (fun oc -> Marshal.to_channel oc v []);
      Sys.rename tmp path
    with _ -> Log.debug (fun m -> m "%s store failed for %s" label path)

  let lookup ~cache_root ~key =
    match
      read_marshal ~label:"solve-cache" (key_path ~cache_root ~sub:"pkgs" key)
    with
    | Some v -> Some (v : OpamPackage.t list)
    | None -> None

  let store ~fs ~cache_root ~key pkgs =
    let path = key_path ~cache_root ~sub:"pkgs" key in
    write_marshal ~fs ~label:"solve-cache" path (pkgs : OpamPackage.t list);
    Log.debug (fun m ->
        m "solve-cache stored %s (%d pkgs)" (short_key key) (List.length pkgs))

  let lookup_layers ~cache_root ~key =
    match
      read_marshal ~label:"layer-cache" (key_path ~cache_root ~sub:"layers" key)
    with
    | Some v -> Some (v : string list)
    | None -> None

  let store_layers ~fs ~cache_root ~key hashes =
    let path = key_path ~cache_root ~sub:"layers" key in
    write_marshal ~fs ~label:"layer-cache" path (hashes : string list);
    Log.debug (fun m ->
        m "layer-cache stored %s (%d layers)" (short_key key)
          (List.length hashes))
end

(* -- Solving (was lib/oi/solve.ml) --------------------------------------- *)

module Inst = Opam_0install.Solver.Make (Dir_context)

let load_opam_from packages_dir pkg =
  let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let pkg_s = OpamPackage.to_string pkg in
  let path = packages_dir / name_s / pkg_s / "opam" in
  if Sys.file_exists path then
    Some (OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)))
  else None

let load_opam packages_dirs pkg =
  List.find_map (fun d -> load_opam_from d pkg) packages_dirs

let std_env ?(ocaml_native = true) ?opam_version (conf : Ctx.conf) v =
  match v with
  | "arch" -> Some (OpamTypes.S conf.arch)
  | "os" -> Some (OpamTypes.S conf.os)
  | "os-distribution" -> Some (OpamTypes.S conf.os_distribution)
  | "os-version" -> Some (OpamTypes.S conf.os_version)
  | "os-family" -> Some (OpamTypes.S conf.os_family)
  | "opam-version" ->
      Some
        (OpamVariable.S
           (Stdlib.Option.value
              ~default:OpamVersion.(to_string current)
              opam_version))
  | "sys-ocaml-arch" | "sys-ocaml-cc" | "sys-ocaml-libc" | "sys-ocaml-system"
  | "sys-ocaml-version" ->
      Some (OpamTypes.S "")
  | "ocaml:native" -> Some (OpamTypes.B ocaml_native)
  | "ocaml:version" -> Some (OpamTypes.S conf.ocaml_version)
  | "enable-ocaml-beta-repository" -> None
  | v ->
      OpamConsole.warning "Unknown variable %S" v;
      None

let ctx_env ctx = std_env (Ctx.conf ctx)

let filter_env (conf : Ctx.conf) v =
  std_env conf (OpamVariable.Full.to_string v)

let dep_names ~packages_dirs ~(conf : Ctx.conf) pkg in_solution =
  let env v =
    if List.mem v OpamPackageVar.predefined_depends_variables then None
    else
      match OpamVariable.Full.to_string v with
      | "version" ->
          Some
            (OpamTypes.S
               (OpamPackage.Version.to_string (OpamPackage.version pkg)))
      | x -> std_env conf x
  in
  match load_opam packages_dirs pkg with
  | None -> OpamPackage.Name.Set.empty
  | Some opam ->
      let names_from_formula f =
        let atoms = OpamFormula.atoms f in
        List.fold_left
          (fun s (name, _) ->
            if OpamPackage.Name.Set.mem name in_solution then
              OpamPackage.Name.Set.add name s
            else s)
          OpamPackage.Name.Set.empty atoms
      in
      let deps =
        OpamFile.OPAM.depends opam
        |> OpamFilter.partial_filter_formula env
        |> OpamFilter.filter_deps ~build:true ~post:false ~test:false ~doc:false
             ~dev_setup:false ~dev:false ~default:false
        |> names_from_formula
      in
      let depopts =
        OpamFormula.fold_left
          (fun s (name, _) ->
            if OpamPackage.Name.Set.mem name in_solution then
              OpamPackage.Name.Set.add name s
            else s)
          OpamPackage.Name.Set.empty
          (OpamFile.OPAM.depopts opam)
      in
      OpamPackage.Name.Set.union deps depopts

let topo_sort ~packages_dirs ~conf pkgs =
  let in_solution =
    List.fold_left
      (fun s p -> OpamPackage.Name.Set.add (OpamPackage.name p) s)
      OpamPackage.Name.Set.empty pkgs
  in
  let pkg_by_name =
    List.fold_left
      (fun m p -> OpamPackage.Name.Map.add (OpamPackage.name p) p m)
      OpamPackage.Name.Map.empty pkgs
  in
  let dep_map =
    List.fold_left
      (fun m p ->
        OpamPackage.Name.Map.add (OpamPackage.name p)
          (dep_names ~packages_dirs ~conf p in_solution)
          m)
      OpamPackage.Name.Map.empty pkgs
  in
  let visited = Hashtbl.create (List.length pkgs) in
  let result = ref [] in
  let rec visit name =
    if not (Hashtbl.mem visited name) then begin
      Hashtbl.replace visited name true;
      OpamPackage.Name.Map.find_opt name dep_map
      |> Stdlib.Option.iter (OpamPackage.Name.Set.iter visit);
      OpamPackage.Name.Map.find_opt name pkg_by_name
      |> Stdlib.Option.iter (fun pkg -> result := pkg :: !result)
    end
  in
  List.iter (fun p -> visit (OpamPackage.name p)) pkgs;
  List.rev !result

let solve_dir ~env ~packages_dirs ~constraints names =
  let dir_ctx = Dir_context.create ~env ~constraints packages_dirs in
  match Inst.solve dir_ctx names with
  | Ok sels -> Ok (Inst.packages_of_result sels)
  | Error diag -> Error (Inst.diagnostics diag)

let augment_compiler_constraints ctx constraints =
  match Ctx.toolchain ctx with
  | None ->
      (* Every code path that reaches the augment step now goes
         through Pipeline.resolve_toolchain (which returns the
         x-oi-default-toolchain entry when no --toolchain is given,
         or hard-errors). Hitting this branch means the caller
         constructed a Ctx without a toolchain and tried to solve —
         a bug rather than a configuration question. *)
      Error.config_error
        "Solver.augment_compiler_constraints: no toolchain set on Solver.Ctx — \
         every solve must thread a toolchain through (default or \
         --toolchain=NAME). Construct the Ctx via Pipeline.resolve_toolchain."
  | Some tc ->
      let add name version_s m =
        if OpamPackage.Name.Map.mem name m then m
        else
          let v = OpamPackage.Version.of_string version_s in
          OpamPackage.Name.Map.add name (`Eq, v) m
      in
      OpamPackage.Name.Set.fold
        (fun name acc ->
          match
            OpamPackage.Set.elements tc.packages
            |> List.find_opt (fun p ->
                OpamPackage.Name.equal (OpamPackage.name p) name)
          with
          | None -> acc
          | Some pkg ->
              add name
                (OpamPackage.Version.to_string (OpamPackage.version pkg))
                acc)
        tc.root_names constraints

let solve_with_dir_context ctx ~packages_dirs ~constraints names =
  let constraints = augment_compiler_constraints ctx constraints in
  Log.info (fun m ->
      m "Solving for %a (toolchain-pinned compiler)"
        Fmt.(list ~sep:comma string)
        (List.map OpamPackage.Name.to_string names));
  match solve_dir ~env:(ctx_env ctx) ~packages_dirs ~constraints names with
  | Ok pkgs ->
      Log.info (fun m -> m "Solution: %d packages to build" (List.length pkgs));
      Ok (pkgs, packages_dirs)
  | Error msg ->
      Log.debug (fun m -> m "No solution: %s" msg);
      Error msg

let log_package_sources ~packages_dirs pkgs =
  let by_dir = Hashtbl.create 8 in
  List.iter
    (fun pkg ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let full = OpamPackage.to_string pkg in
      let src =
        List.find_opt
          (fun d -> Sys.file_exists (d / name / full / "opam"))
          packages_dirs
      in
      let src_s = Stdlib.Option.value src ~default:"<not found>" in
      Log.debug (fun m -> m "  %s <- %s" full src_s);
      let n = Stdlib.Option.value (Hashtbl.find_opt by_dir src_s) ~default:0 in
      Hashtbl.replace by_dir src_s (n + 1))
    pkgs;
  Log.debug (fun m -> m "packages_dirs contribution to solution:");
  let seen = Hashtbl.create 8 in
  List.iter
    (fun d ->
      if not (Hashtbl.mem seen d) then begin
        Hashtbl.add seen d ();
        match Hashtbl.find_opt by_dir d with
        | Some n -> Log.debug (fun m -> m "  %d <- %s" n d)
        | None -> ()
      end)
    packages_dirs

let solve ~fs ~cache_root ctx ~packages_dirs ~constraints names =
  let conf = Ctx.conf ctx in
  let names =
    match Ctx.toolchain ctx with
    | None -> names
    | Some tc ->
        let existing = OpamPackage.Name.Set.of_list names in
        let extra =
          OpamPackage.Name.Set.diff tc.root_names existing
          |> OpamPackage.Name.Set.elements
        in
        names @ extra
  in
  let log_selected_versions pkgs =
    let by_name =
      List.fold_left
        (fun m p -> OpamPackage.Name.Map.add (OpamPackage.name p) p m)
        OpamPackage.Name.Map.empty pkgs
    in
    let render n =
      match OpamPackage.Name.Map.find_opt n by_name with
      | Some p ->
          Some
            (Fmt.str "%s.%s"
               (OpamPackage.Name.to_string n)
               (OpamPackage.Version.to_string (OpamPackage.version p)))
      | None -> None
    in
    let extra_names =
      match Ctx.toolchain ctx with
      | None -> []
      | Some tc ->
          OpamPackage.Name.Set.elements tc.root_names
          |> List.filter (fun n ->
              not (List.exists (OpamPackage.Name.equal n) names))
    in
    let interesting =
      List.filter_map render names @ List.filter_map render extra_names
    in
    if interesting <> [] then
      Log.info (fun m -> m "Selected: %s" (String.concat ", " interesting))
  in
  let run_solve () =
    match solve_with_dir_context ctx ~packages_dirs ~constraints names with
    | Ok (pkgs, _) ->
        let pkgs = topo_sort ~packages_dirs ~conf:(Ctx.conf ctx) pkgs in
        log_package_sources ~packages_dirs pkgs;
        log_selected_versions pkgs;
        Ok pkgs
    | Error _ as e -> e
  in
  match
    Cache.key ~conf ~packages_dirs ~constraints ~names
      ?toolchain:(Ctx.toolchain ctx) ()
  with
  | None -> run_solve ()
  | Some cache_key -> (
      match Cache.lookup ~cache_root ~key:cache_key with
      | Some pkgs ->
          Log.info (fun m ->
              m "solve cache hit %s (%d packages)"
                (String.sub cache_key 0 12)
                (List.length pkgs));
          log_selected_versions pkgs;
          Ok pkgs
      | None -> (
          match run_solve () with
          | Ok pkgs as r ->
              Cache.store ~fs ~cache_root ~key:cache_key pkgs;
              r
          | Error _ as e -> e))
