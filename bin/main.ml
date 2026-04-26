[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

open Cmdliner

let ( / ) = Filename.concat
let app_name = "oi"
let log_src = Logs.Src.create "oi.cli"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Common terms -------------------------------------------------------- *)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Progress.logs_reporter ())

let log_term =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let data_dir_term =
  let app_upper = String.uppercase_ascii app_name in
  let app_env = app_upper ^ "_DATA_DIR" in
  let xdg_var = "XDG_DATA_HOME" in
  let home = Sys.getenv "HOME" in
  let default_path = home / ".local" / "share" / app_name in
  let doc =
    Fmt.str
      "Override data directory. Can also be set with %s or %s. Default: %s"
      app_env xdg_var default_path
  in
  let arg =
    Arg.(value & opt string default_path & info ~docv:"DIR" ~doc [ "data-dir" ])
  in
  Term.(
    const (fun cmdline_val ->
        if cmdline_val <> default_path then cmdline_val
        else
          match Sys.getenv_opt app_env with
          | Some v when v <> "" -> v
          | _ -> (
              match Sys.getenv_opt xdg_var with
              | Some v when v <> "" -> v / app_name
              | _ -> default_path))
    $ arg)

let cache_dir_term = Xdge.Cmd.cache_term app_name

let refresh_term =
  Arg.(
    value & flag
    & info
        ~doc:
          "Re-fetch opam repositories, pinned sources, and git URLs even if \
           they are still fresh. Caches older than 24 hours refresh on their \
           own, so this flag is only needed when you want to pick up an \
           upstream change immediately."
        [ "refresh" ])

(* Common CLI flag: [--with-repo URL], repeatable. Available on every
   command that solves; extras are merged with the project's [x-repos:]
   entries at the call site. *)
let with_repos_term =
  Arg.(
    value & opt_all string []
    & info ~docv:"URL"
        ~doc:
          "Add another opam repository to the solve. The argument is either a \
           git URL or a short reporepo handle (see $(b,oi repo)). May be given \
           more than once to stack repositories."
        [ "with-repo" ])

(* Common CLI flag: [-j N] / [--jobs N]. Available on every command that
   builds; caps concurrent package builds within a stage to bound fd
   and process pressure. Unset here (= None) defers to
   [OI_BUILD_PARALLELISM] and then the executor's default. *)
let jobs_term =
  Arg.(
    value
    & opt (some int) None
    & info ~docv:"N"
        ~doc:
          "Build at most $(b,N) packages in parallel. The default is 4. Higher \
           values speed up clean builds on multi-core machines; lower values \
           reduce memory pressure."
        [ "j"; "jobs" ])

(* Common CLI flag: [--with PKG], repeatable. Available on every command
   that solves; adds extra packages (optionally with version
   constraints, e.g. "fmt>=0.9") to the solver's root set so they end
   up in the built prefix alongside whatever the command would otherwise
   solve. *)
let with_deps_term =
  Arg.(
    value & opt_all string []
    & info ~docv:"PKG"
        ~doc:
          "Include an extra dependency in the solve. The argument is a plain \
           package name, an opam atom such as $(b,fmt>=0.9) or \
           $(b,dune.3.20.0), or a git URL. A URL is cloned and every \
           $(b,*.opam) file at its root becomes a pin. May be given more than \
           once."
        [ "with" ])

(* Common CLI flag: [--toolchain HANDLE]. Resolution + install logic
   lives in {!setup_toolchain} below. *)
let toolchain_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"HANDLE"
        ~doc:
          "Resolve the toolchain named $(docv) from the reporepo and pin its \
           compiler set into the consumer solve. $(b,oi config) lists \
           available toolchains. Relocatable toolchains build into the \
           consumer prefix; non-relocatable ones (oxcaml) install once into \
           \\$XDG_CACHE_HOME/oi/toolchains/ on first use."
        [ "toolchain" ])

(* Convert a CLI [--with-repo URL] entry into a [Project.extra_repo] with a
   deterministic hashed name. We keep the CLI shape (a list of bare URLs)
   and invent a stable local clone directory name here at the boundary. *)
let cli_extra_repo_of_url url_s : Oi.Project.extra_repo =
  let hash = Digest.string url_s |> Digest.to_hex in
  let name = "extra-" ^ String.sub hash 0 10 in
  { name; url = url_s }

(* Lookup [name] in the environment, returning [default] when it's
   absent or set to an empty string. Used to thread [OI_*] env vars
   through commands while keeping their fallbacks in one place. *)
let getenv_or ~default name =
  match Sys.getenv_opt name with Some v when v <> "" -> v | _ -> default

(* Reporepo path + clone URL, honouring [OI_REPOREPO] / [OI_REPOREPO_URL]
   env overrides. Looked up fresh per call so tests can set the env per
   invocation. *)
let reporepo_path () =
  getenv_or ~default:Oi.Source.Reporepo.default_path "OI_REPOREPO"

let reporepo_url () =
  getenv_or ~default:Oi.Source.Reporepo.default_url "OI_REPOREPO_URL"

(* Format-style debug logger for overlay / reporepo plumbing. *)
let log_overlay fmt = Fmt.kstr (fun s -> Logs.debug (fun m -> m "%s" s)) fmt

(* A [--with-repo] token is a URL if it contains a scheme-like prefix
   or a path separator; otherwise it's treated as an overlay handle
   and looked up in the reporepo. *)
let is_url_like s =
  List.exists
    (fun p -> String.starts_with ~prefix:p s)
    [ "http://"; "https://"; "git+"; "git://"; "git@"; "file://"; "./"; "/" ]
  || String.contains s '/'

(* Resolve a list of overlay handles to a flat list of extra-repo
   entries, including their transitive overlay deps. Later handles in
   the input list are given highest priority: they come first in the
   output so the solver's first-wins fold favours them. *)
let overlay_extras_of_handles ~fs ~sys handles =
  if handles = [] then []
  else begin
    let path = reporepo_path () in
    let url = reporepo_url () in
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

(* A ([@handle/pkg...]) shortcut parsed out of a TARGET or [--with]
   token, once the handle has been routed into [with_repos] and the
   package spec is ready for the solver. Carries the handle alongside
   the package name and any user-supplied constraint so we can later
   pin the package to whatever version the named overlay ships. *)
type handle_pin = {
  handle : string;
  pkg : OpamPackage.Name.t;
  user_constr : OpamFormula.version_constraint option;
}

(* Highest version of [pkg] found across [dirs] (each expected to be
   a [packages/] tree). [None] when the package is absent from all of
   them. The directory layout is standard opam: [packages/<pkg>/<pkg.ver>/opam]. *)
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

(* Kinds of "$(b,@handle)-prefixed target" a registry build accepts:
   a plain target, an overlay-scoped package, or "everything the
   overlay ships". [oi run] only accepts the first two; [oi registry
   build] additionally understands [@handle] alone as "all of it". *)
type build_target =
  | Plain_target of string
  | Overlay_pkg of string * string (* (handle, pkg_spec) *)
  | Overlay_all of string (* handle alone, expand to every overlay pkg *)

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

(* Detect an [@handle/pkg[constr]] prefix on a run [TARGET] or a
   [--with] token. Returns [(handle, stripped_spec)] or [None] if
   there's no [@] prefix. Raises [Error.config_error] when the
   handle is present but the package part is empty. *)
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

(* Strip [@handle/pkg] prefixes out of a list of tokens. Each hit
   routes [handle] into [with_repos], replaces the token with its
   stripped form, and records a {!handle_pin} so the caller can
   pin the package to the overlay's version.

   Shared between run, plan, and anything else that solves for a
   caller-supplied list of targets. Returns
   [(stripped_tokens, updated_with_repos, handle_pins)]. *)
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

(* Turn a list of {!handle_pin}s into an [= VERSION] constraint map
   suitable for merging into the solver's [constraints] argument. A
   handle_pin without an explicit user constraint gets pinned to the
   highest version the overlay ships. [cli_extras] is the caller's
   [--with-repo] extras (including any handles just appended by
   {!extract_handle_pins}); it needs to be fully materialised on disk
   before this runs so the overlay's [packages/] tree is readable. *)
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

(* Merge CLI [--with-repo] URLs and project-declared extras. Project
   extras win on a name collision (CLI URLs are synthesised names and
   cannot collide with a user-chosen name unless the user picked an
   [extra-xxxxxxxxxx] prefix, which would be surprising); we instead
   dedup by name, preferring the project entry. *)
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

let default_registry = "https://oi.ci.dev"

let registry_term =
  let doc =
    Fmt.str
      "Remote layer registry URL (default: %s). Layers are fetched as \
       <URL>/<os_key>/<hash>.tar.zst before building from source."
      default_registry
  in
  Arg.(
    value & opt string default_registry & info ~docv:"URL" ~doc [ "registry" ])

let remote_of_registry = function
  | "" -> None
  | url -> Some (`Http_remote url : D10.Layer.remote)

let remote_index_max_age = 3600.0 (* 1 hour *)

(* Join a registry base URL and a relative path with a single [/], regardless
   of whether the user supplied a trailing slash. [rel] is expected not to
   start with one. *)
let url_join registry rel =
  let n = String.length registry in
  let stripped =
    if n > 0 && registry.[n - 1] = '/' then String.sub registry 0 (n - 1)
    else registry
  in
  stripped ^ "/" ^ rel

(* Ensure the remote registry's index.db is cached locally. Downloads it if
   missing or older than [remote_index_max_age]. Returns the local path on
   success.

   The download is atomic: we curl to a [.tmp] sibling and only rename
   into place once the download finished cleanly. A Ctrl-C mid-transfer
   leaves only the half-written [.tmp] behind, which the next invocation
   overwrites. That protects us from the sqlite [CORRUPT] error you
   otherwise get when the previous run wrote a half-database at the
   live path. *)
let ensure_remote_index ~sys ~fs ~cache ~os_key ~registry =
  if registry = "" then None
  else
    let cache_root = Oi.Cache.root_s cache in
    let os_dir = cache_root / "layers" / os_key in
    let local_path = os_dir / "remote-index.db" in
    let tmp_path = local_path ^ ".tmp" in
    let fresh =
      try
        let st = Unix.stat local_path in
        Unix.gettimeofday () -. st.Unix.st_mtime < remote_index_max_age
      with Unix.Unix_error _ -> false
    in
    if fresh then Some local_path
    else begin
      let url = url_join registry (os_key / "index.db") in
      let dst = Eio.Path.(fs / tmp_path) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / os_dir);
      (try Unix.unlink tmp_path with Unix.Unix_error _ -> ());
      Logs.app (fun m ->
          m "Fetching registry index from %s (this may take a moment)..." url);
      if D10.Sysops.Curl.fetch sys ~url ~dst then begin
        (try Unix.rename tmp_path local_path
         with Unix.Unix_error _ -> (
           try Unix.unlink tmp_path with Unix.Unix_error _ -> ()));
        Some local_path
      end
      else begin
        (try Unix.unlink tmp_path with Unix.Unix_error _ -> ());
        if Sys.file_exists local_path then begin
          Logs.warn (fun m ->
              m "Failed to fetch registry index, using stale cache");
          Some local_path
        end
        else begin
          Logs.warn (fun m ->
              m "Failed to fetch registry index from %s" registry);
          None
        end
      end
    end

(* Merge the remote index into the local index, creating the local index
   if it doesn't exist. If the remote sqlite file is corrupt — typically
   the aftermath of a Ctrl-C during the previous run's download — we
   unlink it so the next invocation re-fetches a clean copy instead of
   failing forever. *)
let merge_remote_into_local ~index_path ~remote_path =
  let db = D10.Index.open_ ~path:index_path in
  (try D10.Index.merge_remote db ~remote_path
   with Failure msg -> (
     Logs.warn (fun m ->
         m
           "Remote index merge failed (%s); removing %s so the next run \
            re-downloads it"
           msg remote_path);
     try Sys.remove remote_path with Sys_error _ -> ()));
  D10.Index.close db

(* Count [hash/] directories directly under [layers/<os_key>/]. Each
   corresponds to one stored layer. Returns 0 if the directory does
   not yet exist. *)
let count_on_disk_layers ~os_layer_dir =
  match Sys.readdir os_layer_dir with
  | exception Sys_error _ -> 0
  | entries ->
      Array.fold_left
        (fun n name ->
          if Sys.is_directory (os_layer_dir / name) then n + 1 else n)
        0 entries

(* Ensure the local index exists and is not stale. A stale index is
   the common cause of [oi search] missing a just-built layer: the
   index is built once when [oi search] / [oi run] first needs it, but
   subsequent builds store layers on disk without touching it. Cheap
   staleness check: compare [layers/<os_key>/] directory count with
   the row count in the index. A mismatch triggers a full rebuild.
   Call before any index query in oi run / oi search. *)
let ensure_local_index ~sys ~fs ~clock ~cache ~os_key =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let index_path = layers_dir / "index.db" in
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Oi.Cache.root cache; os_key }
  in
  let rebuild reason =
    Logs.info (fun m -> m "%s local index for %s" reason os_key);
    let db = D10.Index.open_ ~path:index_path in
    D10.Index.rebuild d10 db;
    D10.Index.close db
  in
  if not (Sys.file_exists index_path) then rebuild "Building"
  else begin
    let disk = count_on_disk_layers ~os_layer_dir:layers_dir in
    let db = D10.Index.open_ ~path:index_path in
    let indexed, _, _ = D10.Index.stats db ~os_key in
    D10.Index.close db;
    (* [disk] may dip below [indexed] when layers have been merged from
       a remote registry index but not yet downloaded, so only rebuild
       when the disk count exceeds what the index knows about. *)
    if disk > indexed then
      rebuild (Fmt.str "Refreshing (%d on-disk vs %d indexed)" disk indexed)
  end;
  index_path

(* If [name] is the name of a binary that at least one cached layer
   provides (via the merged local+remote layer index), return the
   package name that ships it and the overlay handle it came from
   (if any). [None] when the index has no row for [name]. The same
   binary can be shipped by different packages across overlays; we
   pick the first result, matching [oi run]'s lookup.

   The overlay handle is load-bearing: a binary like [pipeline] may
   only be provided by a package that lives in [@mtelvers]'s overlay,
   and solving for [pipeline] without the overlay in [with_repos]
   fails with "no known implementations". Callers should add
   [Some handle] to their repo set before solving. *)
let binary_to_package ~sys ~fs ~clock ~cache ~os_key ~registry name =
  let clk = (clock :> D10.Config.clk) in
  let index_path = ensure_local_index ~sys ~fs ~clock:clk ~cache ~os_key in
  (match ensure_remote_index ~sys ~fs ~cache ~os_key ~registry with
  | Some remote_path -> merge_remote_into_local ~index_path ~remote_path
  | None -> ());
  if not (Sys.file_exists index_path) then None
  else
    let db = D10.Index.open_ ~path:index_path in
    let results = D10.Index.find_binary db ~binary:name ~os_key in
    D10.Index.close db;
    match results with
    | (pkg, _, _, overlay) :: _ ->
        let handle =
          match overlay with Some (h, _) -> Some h | None -> None
        in
        Some (pkg, handle)
    | [] -> None

(* Resolve the string passed to [oi show --os] into an
   [Oi.Solver.Ctx.conf]. The parser is
   {!Dockerfile_opam.Distro.distro_of_tag}, which knows every tag
   dockerfile-opam supports ([alpine-3.23], [ubuntu-22.04],
   [debian-13], [fedora-43], [centos-9], [debian-stable],
   [archlinux], ...), plus bare forms ([alpine], [ubuntu], [fedora],
   [opensuse], ...) that map to each distro's [Latest] variant. A
   bare distro just picks up whatever dockerfile-opam considers the
   latest release of that distribution; a tagged form pins to the
   given version.

   (os-family, os-distribution, os-version) is read off the parsed
   {!Dockerfile_opam.Distro.t} via
   {!Registry_docker.opam_vars_of_distro}, the same mapping the
   registry-docker generator uses. [os] comes from
   {!Dockerfile_opam.Distro.os_family_of_distro}, mapped to opam's
   three values: Linux -> [linux], Cygwin -> [cygwin], Windows ->
   [win32].

   Inputs dockerfile-opam doesn't recognise fall through to the
   legacy "just rewrite [os]" behaviour with a warning. That covers
   bare opam os values ([linux], [macos], [freebsd], [win32],
   [cygwin]) and anything truly unknown. *)
let resolve_os_override (conf_host : Oi.Solver.Ctx.conf) os_str =
  let known_os_values =
    [ "linux"; "macos"; "freebsd"; "openbsd"; "netbsd"; "win32"; "cygwin" ]
  in
  let opam_os_of_docker_family = function
    | `Linux -> "linux"
    | `Cygwin -> "cygwin"
    | `Windows -> "win32"
  in
  match try Dockerfile_opam.Distro.distro_of_tag os_str with _ -> None with
  | Some d ->
      let vars = Registry_docker.opam_vars_of_distro d in
      let os =
        opam_os_of_docker_family (Dockerfile_opam.Distro.os_family_of_distro d)
      in
      {
        conf_host with
        os;
        os_family = vars.os_family;
        os_distribution = vars.os_distribution;
        os_version = vars.os_version;
      }
  | None ->
      if not (List.mem os_str known_os_values) then
        Logs.warn (fun m ->
            m
              "--os=%s is not a known dockerfile-opam distro tag or opam os \
               value; only the 'os' variable was changed so depext filters \
               keyed on os-family or os-distribution will not reflect this \
               override. Try a tagged form such as debian-13, centos-9, or \
               alpine-3.23."
              os_str);
      { conf_host with os = os_str }

(* -- Helpers ------------------------------------------------------------- *)

(* Eio.Process.spawn uses execvp-style lookup, which resolves bare
   executable names against the *caller's* PATH — not the PATH inside
   [~env]. Resolve the first token against the target env's PATH here
   so the child actually finds binaries from the assembled prefix. *)
let path_from_env env =
  Array.find_map
    (fun s ->
      if String.starts_with ~prefix:"PATH=" s then
        Some (String.sub s 5 (String.length s - 5))
      else None)
    env

let is_executable p =
  try
    Unix.access p [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let resolve_in_env ~env exe =
  if String.contains exe '/' then exe
  else
    match path_from_env env with
    | None -> exe
    | Some path ->
        String.split_on_char ':' path
        |> List.find_map (function
          | "" -> None
          | d ->
              let candidate = d / exe in
              if is_executable candidate then Some candidate else None)
        |> Stdlib.Option.value ~default:exe

(* Run a command and return its exit code (never raises on non-zero exit) *)
let run_exec proc_mgr ~env cmd =
  let cmd =
    match cmd with exe :: rest -> resolve_in_env ~env exe :: rest | [] -> cmd
  in
  Eio.Switch.run @@ fun sw ->
  let child = Eio.Process.spawn ~sw proc_mgr ~env cmd in
  match Eio.Process.await child with `Exited n -> n | `Signaled n -> 128 + n

(* Wrap command body to catch structured errors *)
let pp_one_exn fmt = function
  | Oi.Error.E e -> Oi.Error.pp fmt e
  | Failure msg -> Fmt.pf fmt "%a %s" Fmt.(styled `Red string) "error:" msg
  | e ->
      Fmt.pf fmt "%a %s"
        Fmt.(styled `Red string)
        "error:" (Printexc.to_string e)

(* Test whether an exception (possibly wrapped) is rooted in our
   signal-handler cancel path or opam's [Sys.Break]. Either should
   render as a clean "Interrupted." exit, not a scary traceback. *)
let rec is_interrupt = function
  | Oi.Signals.Interrupted | Sys.Break -> true
  | Eio.Cancel.Cancelled e -> is_interrupt e
  | Eio.Exn.Io _ -> false
  | _ -> false

let with_error_handling f =
  try f () with
  | exn when is_interrupt exn ->
      Fmt.epr "Interrupted.@.";
      exit 130
  | Eio.Exn.Multiple exns when List.exists (fun (e, _) -> is_interrupt e) exns
    ->
      Fmt.epr "Interrupted.@.";
      exit 130
  | (Oi.Error.E _ | Failure _) as exn ->
      Fmt.epr "%a@." pp_one_exn exn;
      exit 1
  | Eio.Exn.Multiple exns ->
      List.iter (fun (e, _bt) -> Fmt.epr "%a@." pp_one_exn e) exns;
      exit 1

(* Boilerplate wrapper: every top-level command body should be run
   inside a root [Eio.Switch] so that [Signals.install] has something
   concrete to cancel, and so that resources registered with the
   switch (subprocesses, daemons) unwind cleanly on Ctrl-C.

   Use as:
     [with_eio_root @@ fun env sw -> ...body using sw...]

   The returned closure is still expected to be called inside
   [with_error_handling]. *)
(* Forced to the POSIX backend rather than [Eio_main.run] so that
   builds under Linux don't pick up [eio_linux] / io_uring — we want
   the same syscall surface everywhere, and io_uring interacts poorly
   with some of the subprocess / signal paths we rely on. If you need
   a different backend, swap this for [Eio_main.run] and thread
   [eio_main] back into [bin/dune]. *)
let with_eio_root f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Oi.Signals.install ~sw;
  f env sw

(* Standard per-command bootstrap. Returns the fields most commands derive
   from the Eio environment, plus the configured cache. *)
let bootstrap env cache_dir =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let stdout = Eio.Stdenv.stdout env in
  let stderr = Eio.Stdenv.stderr env in
  let sys = D10.Sysops.create ~stdout ~stderr ~proc_mgr ~fs () in
  let platform = Osrel.detect ~proc_mgr ~fs in
  let os_key = D10.Os_key.(to_string (of_platform platform)) in
  let cache = Oi.Cache.create ~root:cache_dir fs in
  (proc_mgr, fs, clock, sys, platform, os_key, cache)

(* Does the Eio path exist? Follows symlinks. *)
let path_exists fs path =
  try
    ignore (Eio.Path.stat ~follow:true Eio.Path.(fs / path));
    true
  with Eio.Exn.Io _ -> false

(* Resolve the current working directory once, as a canonical absolute path.
   Returns the string form (for env vars and opam APIs that take strings)
   and an [Eio.Path.t] rooted at [fs] (for filesystem operations).
   Using [Unix.realpath] up front avoids the relative "." that
   [Eio.Stdenv.cwd] would yield via [Eio.Path.native_exn], which leaks
   into OCAMLFIND_CONF / OCAMLLIB and breaks dune. *)
let resolved_cwd fs =
  let s = Unix.realpath "." in
  (s, Eio.Path.(fs / s))

(* Detect a project-local [_oi/tools/] that {!install_tools} has written
   under [cwd]. Returns [Some path] only when [tools/bin/] is populated,
   so callers that prepend it to PATH don't add a dangling directory. *)
let tools_dir_for ~cwd =
  let tools = cwd / "_oi" / "tools" in
  match Sys.is_directory (tools / "bin") with
  | true -> Some tools
  | false | (exception Sys_error _) -> None

(* Parse a CLI target as either "name", "name.version", or an opam atom
   like "name>=1.0" / "name=1.0". Returns the bare name and an optional
   version constraint for the solver. *)
let parse_pkg_target s =
  match OpamPackage.of_string_opt s with
  | Some pkg -> (OpamPackage.name pkg, Some (`Eq, OpamPackage.version pkg))
  | None -> OpamFormula.atom_of_string s

(* -- Platform config ------------------------------------------------------ *)

let ocaml_version = "5.4.1"

(* -- run ----------------------------------------------------------------- *)

let run_script ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache ~data_dir
    ?remote script_path cli_deps args =
  let file_deps = Oi.Project.Script.parse_deps_from_file ~fs script_path in
  let all_deps = Oi.Project.Script.dedup (file_deps @ cli_deps) in
  if all_deps = [] then
    Oi.Error.msg
      "No dependencies found. Add [@@@opam pkg1 pkg2] to the first line or use \
       --with=pkg";
  let script_hash = Oi.Project.Script.script_hash script_path all_deps in
  let run_dir = Oi.Cache.run_dir cache ~hash:script_hash in
  let run_dir_s = Eio.Path.native_exn run_dir in
  let cached_bin = run_dir_s / "main.exe" in
  if path_exists fs cached_bin then
    exit
      (run_exec proc_mgr
         ~env:
           (Oi.Solver.Env.make_env ~prefix
              ~dune_cache_root:(Oi.Cache.dune_root cache) ())
         (cached_bin :: args))
  else begin
    let packages_dirs = Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir () in
    let ocaml_name = OpamPackage.Name.of_string "ocaml" in
    let dep_names =
      List.filter_map
        (fun (d : Oi.Project.Script.dep) ->
          if OpamPackage.Name.equal d.name ocaml_name then None else Some d.name)
        all_deps
    in
    let constraints = Oi.Project.Script.constraints all_deps in
    if dep_names <> [] then begin
      let cache_root = Oi.Cache.root_s cache in
      let build_prefix = cache_root / "build" / "prefix" in
      let ctx =
        Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs ~conf ()
      in
      let pkgs =
        match
          Oi.Solver.solve ~fs ~cache_root ctx ~packages_dirs ~constraints
            dep_names
        with
        | Ok pkgs -> pkgs
        | Error msg ->
            Fmt.epr "No solution: %s@." msg;
            exit 1
      in
      let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
      let plan = Oi.Plan.build ctx ~d10 ~packages_dirs pkgs in
      let exec_plan =
        Oi.Plan.resolve ctx ~packages_dirs ~cache_root ~os_key
          ~ocaml_version:conf.ocaml_version plan
      in
      let cache_urls = Oi.Pipeline.cache_urls ~cache ~remote in
      Oi.Execute.run ~cache_urls ~proc_mgr ~fs
        ~clock:(clock :> D10.Config.clk)
        ~sys ~os_key exec_plan;
      Oi.Pipeline.record_sources ~sys ~cache exec_plan
    end;
    let build_dir = run_dir_s in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / build_dir);
    Oi.Project.Script.generate_project ~script:script_path ~deps:all_deps
      ~dir:build_dir;
    let build_env =
      Oi.Solver.Env.make_env ~prefix ~dune_cache_root:(Oi.Cache.dune_root cache)
        ()
    in
    Eio.Process.run proc_mgr ~env:build_env
      [ "/bin/sh"; "-c"; Fmt.str "cd %s && dune build main.exe 2>&1" build_dir ];
    let built = build_dir / "_build" / "default" / "main.exe" in
    if path_exists fs built then begin
      let content = Eio.Path.load Eio.Path.(fs / built) in
      Eio.Path.save ~create:(`Or_truncate 0o755)
        Eio.Path.(fs / cached_bin)
        content
    end;
    let exe = if path_exists fs cached_bin then cached_bin else built in
    exit (run_exec proc_mgr ~env:build_env (exe :: args))
  end

let run_cmd =
  let run () data_dir cache_dir refresh dry_run registry toolchain target
      with_deps with_repos jobs args =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf = Oi.Pipeline.make_conf ~platform ~ocaml_version in
    let toolchain =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:true
        toolchain
    in
    let remote = remote_of_registry registry in
    let dune_cache_root = Oi.Cache.dune_root cache in
    (* Preserve [binary_name] (the original token) for the final
       [bin/<name>] exec lookup. Resolution proceeds solve-first,
       search-after: we first try [target] verbatim as a package
       name (with any [--with] deps included). If that solve
       produces [bin/<binary_name>] in the prefix, we're done. Only
       on miss do we consult the layer index for binary→package
       mapping. The old approach of pre-rewriting [target] via the
       index baked in a [@default/X] handle pin before any solve,
       which then overrode user constraints like
       [--with=utop.2.16.0+ox1]. *)
    let binary_name = target in
    (* [TARGET] and every [--with] token accept the
       [@handle/pkg[constraint]] shortcut. The handle is routed into
       [with_repos] so the overlay joins the solve; the stripped
       package spec takes its place; each handle_pin is then pinned
       to the overlay's version below. For [TARGET] the package name
       feeds the solve, but [binary_name] (captured above) still
       drives the final [bin/<name>] lookup. *)
    let target, with_repos, with_deps, target_pin =
      match split_handle_prefix target with
      | None -> (target, with_repos, with_deps, None)
      | Some (h, pkg_spec) ->
          let pkg, user_constr = OpamFormula.atom_of_string pkg_spec in
          let pin = { handle = h; pkg; user_constr } in
          ( OpamPackage.Name.to_string pkg,
            with_repos @ [ h ],
            with_deps @ [ pkg_spec ],
            Some pin )
    in
    let with_deps, with_repos, with_pins =
      extract_handle_pins ~with_repos with_deps
    in
    (* URL-projects in [--with=…]: clone each URL into the pin cache,
       scan its *.opam files, and merge the contribution as pins +
       solver roots + overlays + extra_repos. *)
    let extra_deps, url_project =
      Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    let extra_constraints = Oi.Project.Script.constraints extra_deps in
    (* Resolve the cwd once; reused for project-extras loading and the
       script-file existence check below. *)
    let cwd_s, cwd = resolved_cwd fs in
    (* Load project extras (if any *.opam in cwd). A missing/unreadable
       directory degrades to "no extras"; malformed metadata still raises
       [Error.E] so the user sees the problem. *)
    let project_extras, project_pins, project_overlays =
      match Oi.Project.load ~fs cwd_s with
      | exception Sys_error _ -> ([], [], [])
      | exception Eio.Exn.Io _ -> ([], [], [])
      | p -> (p.extra_repos, p.pins, p.overlays)
    in
    let project_extras = project_extras @ url_project.extra_repos in
    let project_pins = project_pins @ url_project.pins in
    let project_overlays = project_overlays @ url_project.overlays in
    (* Treat [@HANDLE] entries from the project's [x-repos:] as if
       they had been passed via [--with-repo]. Project-declared
       overlays go earlier in the list so CLI-supplied ones take
       priority (first-wins at repos level; later arguments stack
       atop). When [--toolchain] is set, drop any project overlays
       tagged for a different toolchain — the explicit flag wins. *)
    let project_overlays =
      Oi.Pipeline.filter_compatible_overlays ~reporepo_path:(reporepo_path ())
        ~toolchain project_overlays
    in
    let with_repos = project_overlays @ with_repos in
    let cli_extras = cli_extra_repos ~fs ~sys with_repos in
    let all_extras = merge_extras ~cli:cli_extras ~project:project_extras in
    (* Pin each [@handle/pkg] (from TARGET or [--with]) to whatever
       version the overlay ships, so a dev-tagged version (e.g.
       [2.0.0~dev]) that would otherwise sort below a stable repo's
       version still wins when the user explicitly asked for it. The
       overlay is cloned upfront so we can scan its [packages/] tree;
       the subsequent solve reuses the same clone. *)
    let handle_pins = Stdlib.Option.to_list target_pin @ with_pins in
    let handle_constraints =
      handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
    in
    let extra_constraints =
      OpamPackage.Name.Map.union
        (fun a _ -> a)
        handle_constraints extra_constraints
    in
    let solve_assemble_run pkg_names =
      Logs.info (fun m ->
          m "Solving for packages: %s" (String.concat ", " pkg_names));
      let names = List.map OpamPackage.Name.of_string pkg_names in
      let layer_hashes =
        Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
          ~os_key ~dry_run ~extra_repos:all_extras ~pins:project_pins ~refresh
          ?remote ?jobs ?toolchain ~constraints:extra_constraints names
      in
      Logs.info (fun m -> m "Got %d layer hashes" (List.length layer_hashes));
      let prefix =
        Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      in
      Logs.info (fun m -> m "Assembled prefix at %s" prefix);

      let bin = prefix / "bin" / binary_name in
      Logs.info (fun m -> m "Looking for binary: %s" bin);
      let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info toolchain in
      let env_vars () =
        Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
      in
      if path_exists fs bin then begin
        Logs.info (fun m -> m "Found binary, executing");
        exit (run_exec proc_mgr ~env:(env_vars ()) (bin :: args))
      end
      else
        (* Non-relocatable toolchains keep their compiler binaries
           (ocamlc, ocamlfind, ocamlbuild, ...) at the fixed
           toolchain prefix rather than in the consumer prefix —
           [Opam_ctx.create] marks those packages as already
           installed so they're not rebuilt into the consumer side.
           Look for the binary there too before declaring it
           missing. The consumer prefix's env still applies (PATH /
           OCAMLPATH / CAML_LD_LIBRARY_PATH layer the toolchain's
           [bin]/[lib] in already). *)
        let tc_bin =
          match toolchain with
          | Some (info : Oi.Toolchain.info) when not info.relocatable ->
              let p = info.install_prefix / "bin" / binary_name in
              if path_exists fs p then Some p else None
          | _ -> None
        in
        match tc_bin with
        | Some p ->
            Logs.info (fun m ->
                m "Found %s in toolchain prefix: %s" binary_name p);
            exit (run_exec proc_mgr ~env:(env_vars ()) (p :: args))
        | None ->
            (* List what binaries are available in the prefix *)
            let bin_dir = prefix / "bin" in
            (try
               let bins = Eio.Path.read_dir Eio.Path.(fs / bin_dir) in
               Logs.info (fun m ->
                   m "Available binaries in prefix: %s"
                     (String.concat ", " (List.sort String.compare bins)))
             with Eio.Exn.Io _ ->
               Logs.info (fun m -> m "No bin/ directory in prefix"));
            false
    in
    (* HTTP(S) script URLs: fetch to a fresh tmp file keeping the [.ml]
       suffix, then treat the local copy as the script path for the
       rest of the run. Run caching keys on the script's content hash,
       so the nondeterministic path doesn't defeat the build cache. *)
    let target =
      let is_url s =
        String.starts_with ~prefix:"http://" s
        || String.starts_with ~prefix:"https://" s
      in
      if is_url target then begin
        let local = Filename.temp_file "oi-script-" ".ml" in
        Logs.info (fun m -> m "Fetching %s to %s" target local);
        if
          not (D10.Sysops.Curl.fetch sys ~url:target ~dst:Eio.Path.(fs / local))
        then Oi.Error.not_found target "failed to fetch %s" target;
        local
      end
      else target
    in
    (* Only .ml files are treated as scripts *)
    if Filename.check_suffix target ".ml" then begin
      if not (path_exists cwd target) then
        Oi.Error.not_found target "file not found: %s" target;
      (* For scripts, solve deps first to get a prefix with the compiler *)
      let all_script_deps =
        Oi.Project.Script.parse_deps_from_file ~fs target @ extra_deps
      in
      let ocaml_name = OpamPackage.Name.of_string "ocaml" in
      let dep_opam_names =
        List.filter_map
          (fun (d : Oi.Project.Script.dep) ->
            if OpamPackage.Name.equal d.name ocaml_name then None
            else Some d.name)
          all_script_deps
      in
      let constraints = Oi.Project.Script.constraints all_script_deps in
      let layer_hashes =
        if dep_opam_names = [] then []
        else
          Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
            ~os_key ~dry_run ~extra_repos:all_extras ~pins:project_pins ~refresh
            ?remote ?jobs ?toolchain ~constraints dep_opam_names
      in
      if dry_run && dep_opam_names = [] then
        (* No deps to solve, but still in dry-run mode — just exit *)
        exit 0;
      let prefix =
        Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      in
      run_script ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache
        ~data_dir ?remote target extra_deps args
    end
    else begin
      (* Include --with deps in every solve *)
      let ocaml_name = OpamPackage.Name.of_string "ocaml" in
      let extra_names =
        List.filter_map
          (fun (d : Oi.Project.Script.dep) ->
            if OpamPackage.Name.equal d.name ocaml_name then None
            else Some (Oi.Project.Script.name_s d))
          extra_deps
        @ url_project.roots
      in
      let solve_assemble_run_with pkg_names =
        solve_assemble_run (pkg_names @ extra_names)
      in
      (* Step 0a: solve [target] as a package name alongside [--with]
         deps. Catches the common case where binary and package name
         match (utop, dune, odoc, ...). The catch logs in verbose
         mode — when no later step succeeds either the user gets a
         generic "no package provides" error, and the [-v] log is
         where they find the real solver diagnostic. *)
      let try_step label f =
        try f ()
        with Oi.Error.E e ->
          Logs.info (fun m -> m "%s failed: %a" label Oi.Error.pp e);
          false
      in
      let from_target =
        try_step (Fmt.str "solve %s" target) (fun () ->
            solve_assemble_run_with [ target ])
      in
      (* Step 0b: best-effort fallback when [target] isn't an opam
         package. Solve whatever else we have (extras, toolchain
         roots, pins), assemble, and look for [bin/<target>] there.
         Catches toolchain-supplied binaries like [ocamlc]. *)
      let from_with =
        if not from_target then
          try_step "fallback solve (extras + toolchain roots)" (fun () ->
              solve_assemble_run extra_names)
        else from_target
      in
      (* Explicit [@handle/pkg] target: never silently fall through
         to the layer-index lookup. The user named the source of
         truth; substituting a different package (irmin-cli for
         irmin, say) would be wrong. *)
      if Stdlib.Option.is_some target_pin && not from_with then
        Oi.Error.not_found binary_name
          "overlay-pinned package does not provide bin/%s. Check 'oi config' \
           or the overlay's opam file."
          binary_name;
      if not from_with then begin
        (* Dash-split prefixes: "a-b-c" → ["a-b-c"; "a-b"; "a"] *)
        let dash_prefixes name =
          let parts = String.split_on_char '-' name in
          let rec aux acc prefix = function
            | [] -> List.rev acc
            | p :: rest ->
                let prefix = match prefix with "" -> p | s -> s ^ "-" ^ p in
                aux (prefix :: acc) prefix rest
          in
          List.rev (aux [] "" parts)
        in
        (* Step 1: layer-index lookup. Solving for [target] verbatim
           didn't yield [bin/<target>] — either [target] isn't a
           package name, or the package that owns the binary is
           named differently (e.g. [ocluster-admin] is shipped by
           [ocluster]). Ask the index for the provider, route the
           overlay (when present) through [@handle/pkg] so the
           overlay is added to [with_repos], then re-solve. *)
        let clk = (Eio.Stdenv.clock env :> D10.Config.clk) in
        let from_index =
          match
            binary_to_package ~sys ~fs ~clock:clk ~cache ~os_key ~registry
              binary_name
          with
          | Some (pkg_name, _) when pkg_name <> binary_name ->
              (* Layer index says [bin/<binary_name>] is shipped by
                 [pkg_name] (and optionally an overlay handle).
                 [solve_assemble_run_with] takes raw package names —
                 [@handle/pkg] tokens were never parsed here. The base
                 [@default] is always in scope already, and any
                 user-relevant overlay handle is in scope via
                 [--with-repo] / [x-repos] anyway, so passing just
                 [pkg_name] works for the common cases without
                 lying through an unparsed [@handle/...] string. *)
              Logs.info (fun m ->
                  m "Index: bin/%s provided by package %s" binary_name pkg_name);
              try_step (Fmt.str "solve %s" pkg_name) (fun () ->
                  solve_assemble_run_with [ pkg_name ])
          | _ -> false
        in
        if not from_index then begin
          (* Step 2: Try target name and dash-split prefixes. Skip any
             prefix already in [extra_names] (Step 0 solved that already)
             and skip any prefix that doesn't exist as a package in any
             configured repo — a missing package name cannot possibly
             provide the binary, and attempting to solve for it wastes
             a full solver run. *)
          let pin_dir =
            Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh project_pins
          in
          let packages_dirs =
            Stdlib.Option.to_list pin_dir
            @ Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras
            @ Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ()
          in
          let package_exists name =
            List.exists (fun dir -> Sys.file_exists (dir / name)) packages_dirs
          in
          let prefixes =
            dash_prefixes target
            |> List.filter (fun p -> not (List.mem p extra_names))
            |> List.filter package_exists
          in
          if prefixes = [] then
            Oi.Error.not_found target "no package provides bin/%s" target
          else begin
            Logs.info (fun m ->
                m "Trying packages: %s" (String.concat ", " prefixes));
            let found =
              List.exists
                (fun name ->
                  try_step (Fmt.str "solve %s (dash-prefix)" name) (fun () ->
                      solve_assemble_run_with [ name ]))
                prefixes
            in
            if not found then
              Oi.Error.not_found target "no package provides bin/%s" target
          end
        end
      end
    end
  in
  let target =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"TARGET"
          ~doc:
            "Either the name of a binary to run, the path to an OCaml $(b,.ml) \
             script, or an $(b,http) or $(b,https) URL pointing at a remote \
             $(b,.ml) script."
          [])
  in
  let dry_run =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print the packages that would be built, but do not fetch, \
             compile, or run anything."
          [ "n"; "dry-run" ])
  in
  let args =
    Arg.(
      value & pos_right 0 string []
      & info ~docv:"ARG"
          ~doc:
            "Arguments passed through to $(b,TARGET). Use $(b,--) to separate \
             them from $(b,oi)'s own flags."
          [])
  in
  let info =
    Cmd.info "run" ~doc:"Run an OCaml script or any opam-packaged binary"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi run) resolves the dependencies of $(b,TARGET), installs \
             them into a shared local cache, and executes $(b,TARGET) with the \
             right environment. The first invocation of a given target does \
             the work; every subsequent invocation with the same dependency \
             set reuses the cache and starts in a few milliseconds.";
          `P
            "$(b,TARGET) is one of three things: the name of a binary \
             installed by some opam package, the path to a local OCaml \
             $(b,.ml) script, or an $(b,http) or $(b,https) URL pointing at a \
             remote OCaml script.";
          `S "BINARY TARGETS";
          `P
            "When $(b,TARGET) names a binary, $(b,oi) looks up the opam \
             package that ships it and installs that package on demand. If the \
             binary name is not found directly, dash-separated prefixes are \
             tried in turn, so $(b,ocluster-admin) falls back to looking up \
             $(b,ocluster). Any packages you pass with $(b,--with) are \
             searched first, which lets you pin the provider explicitly when \
             two packages ship the same binary name.";
          `Pre
            "  oi run utop\n\
            \  oi run ocamlformat -- --help\n\
            \  oi run --with=crockford roguedoi";
          `S "SCRIPT TARGETS";
          `P
            "When $(b,TARGET) is a $(b,.ml) file, $(b,oi) reads its first line \
             to discover the dependencies, builds them together with the \
             script, and caches the result. Re-running the same script without \
             edits is almost instantaneous; editing the script invalidates the \
             cache entry and triggers a rebuild. Remote $(b,http) or \
             $(b,https) URLs are fetched on every invocation and rebuilt only \
             when the contents differ from the cached copy.";
          `P
            "Declare dependencies with an $(b,@@@opam) attribute at the top of \
             the file:";
          `Pre "  [@@@opam fmt cmdliner lwt>=5.0]";
          `P
            "Each token names an opam package. An optional version constraint \
             uses the usual relational operators ($(b,>=), $(b,>), $(b,<=), \
             $(b,<), $(b,=)). A dotted suffix picks a findlib sub-library, for \
             example $(b,ppx_deriving.show). Any package whose name starts \
             with $(b,ppx_) is wired in as a PPX preprocessor.";
          `Pre
            "  oi run my_script.ml\n\
            \  oi run my_script.ml --with=tls -- arg1 arg2\n\
            \  oi run https://gist.example.com/hello.ml";
          `S "OVERLAYS";
          `P
            "An overlay is a curated collection of opam packages pinned to \
             specific git commits (see $(b,oi repo)). Prefix a target or a \
             $(b,--with) value with $(i,@HANDLE/) to take that package from \
             the named overlay, or use bare $(i,@HANDLE) to pull the entire \
             overlay into the solve. Overlays stack, which is how you compose \
             two users' collections into one solve.";
          `Pre
            "  oi run @avsm/owntracks\n\
            \  oi run @samoht/irmin\n\
            \  oi run --with=@avsm/crockford roguedoi";
          `P
            "Add $(b,x-repos: [\"@HANDLE\"]) inside an opam file to make the \
             overlay apply automatically to every $(b,oi) command run in that \
             project. The same field also accepts plain repository URLs as an \
             unpinned escape hatch; a leading $(b,@) marks reporepo handles.";
          `S "GIT URLS";
          `P
            "Passing $(b,--with=URL) clones the repository and treats every \
             $(b,*.opam) file at its root as a pinned solver root. The \
             recognised URL schemes are $(b,http://), $(b,https://), \
             $(b,git+), $(b,git@), $(b,git://), and $(b,ssh://). Append \
             $(b,#REF) to pin a specific branch, tag, or commit.";
          `Pre
            "  oi run --with=https://github.com/owner/project.git target\n\
            \  oi run --with=git+https://example.org/foo.git#branch foo";
          `S "VERSION CONSTRAINTS";
          `P
            "Pin a $(b,--with) dependency to a specific version with \
             $(b,pkg.VERSION) or $(b,pkg=VERSION). The same relational \
             operators as the script format are accepted.";
          `Pre
            "  oi run --with=dune.3.20.0 -- dune --version\n\
            \  oi run --with=fmt>=0.9 my_script.ml";
          `S "DRY RUN";
          `P
            "The $(b,-n) and $(b,--dry-run) flags print the plan $(b,oi) would \
             execute and exit. Each package in the tree carries a tag that \
             tells you where the bytes would come from:";
          `I ("$(b,binary)", "Already present in the local cache.");
          `I ("$(b,remote)", "Available from the configured registry.");
          `I ("$(b,source)", "Would be compiled from source.");
          `I
            ( "$(b,virtual)",
              "A placeholder such as $(b,conf-pkg-config) with nothing to \
               build." );
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ dry_run $ registry_term $ toolchain_term $ target $ with_deps_term
      $ with_repos_term $ jobs_term $ args)

(* -- plan ---------------------------------------------------------------- *)

(* Rendering helpers for [oi show]'s default succinct page. *)

(* Format the top-block "Target:" line. For a CLI-supplied target we
   print it verbatim (e.g. "utop", "@avsm/tangled"); for the
   local-project case we show the first declared package name plus a
   count when there is more than one. *)
let show_target_label ~targets ~project_deps =
  match targets with
  | [] -> (
      match project_deps with
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
  | h :: _ when not (is_url_like h) -> Some ("@" ^ h)
  | _ -> None

(* Split the action plan's nodes into (cached, source) counts. *)
let show_counts action_plan =
  List.fold_left
    (fun (c, s) (n : Oi.Plan.node) ->
      match n.method_ with
      | Oi.Plan.Binary -> (c + 1, s)
      | Oi.Plan.Source -> (c, s + 1))
    (0, 0)
    (Oi.Plan.nodes action_plan)

(* Compute the depexts declared by every package in the plan (both
   cached and source), along with the host installation status. The
   full closure is what the old [oi depexts] reported and is the right
   answer for scripting use ("what would this need from apt if I were
   building from scratch?"). When [--os] is set the host check isn't
   meaningful and we return [None] for the status. *)
let show_depexts ~ctx ~packages_dirs ~action_plan ~os_override =
  let all_pkgs =
    List.map (fun (n : Oi.Plan.node) -> n.pkg) (Oi.Plan.nodes action_plan)
  in
  let entries =
    match os_override with
    | None -> Oi.Depexts.compute ctx ~packages_dirs all_pkgs
    | Some _ ->
        let conf = Oi.Solver.Ctx.conf ctx in
        Oi.Depexts.compute_for_conf ~conf ~packages_dirs all_pkgs
  in
  let all =
    List.fold_left
      (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
      OpamSysPkg.Set.empty entries
  in
  let status =
    if os_override <> None then None else Some (Oi.Depexts.status all)
  in
  (all, status)

(* Read the first *.opam file in [cwd] directly, for the no-target
   case where we want to surface the project's own metadata rather
   than a dependency's. Returns [(pkg, opam)] where the package name
   is taken from the filename (minus the [.opam] suffix) and the
   version is a placeholder since a project's own opam file is
   typically versionless. *)
let read_first_local_opam ~cwd =
  let entries = try Sys.readdir cwd |> Array.to_list with _ -> [] in
  let opams =
    entries
    |> List.filter (fun n -> Filename.check_suffix n ".opam")
    |> List.sort String.compare
  in
  match opams with
  | [] -> None
  | first :: _ -> (
      let path = Filename.concat cwd first in
      let name = Filename.chop_suffix first ".opam" in
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
  | From_node of Oi.Plan.node
  | From_project_opam of OpamPackage.t * OpamFile.OPAM.t

let show_primary_meta ~action_plan ~targets ~project_deps ~cwd =
  let find_name name =
    try Some (Oi.Plan.find action_plan (OpamPackage.Name.of_string name))
    with _ -> None
  in
  match targets with
  | first :: _ -> (
      match find_name first with Some n -> Some (From_node n) | None -> None)
  | [] -> (
      match read_first_local_opam ~cwd with
      | Some (pkg, opam) -> Some (From_project_opam (pkg, opam))
      | None -> (
          match project_deps with
          | first :: _ ->
              Stdlib.Option.map (fun n -> From_node n) (find_name first)
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
      Fmt.pr "%a %s@,"
        Fmt.(styled `Bold string)
        (Fmt.str "%-11s" (label ^ ":"))
        v

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
    try Oi.Source.Reporepo.load ~path:(reporepo_path ())
    with Oi.Error.E _ -> []
  in
  let base_handles =
    match toolchain with
    | Some (info : Oi.Toolchain.info) -> info.handle :: info.dep_handles
    | None ->
        Oi.Source.Reporepo.base_entries ()
        |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
  in
  let extra_handles = List.filter (fun h -> not (is_url_like h)) with_repos in
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

(* Render the default succinct info page. *)
let show_render_info ~target_label ~target_version ~target_opam ~overlay ~os_key
    ~ocaml_version ~n_cached ~n_source ~all_depexts ~dep_status ~repositories
    ~binaries =
  let n_total = n_cached + n_source in
  Fmt.pr "@[<v>";
  let target_line =
    match target_version with
    | "" -> target_label
    | v -> Fmt.str "%s %s" target_label v
  in
  show_meta_line "Target" target_line;
  let description =
    match target_opam with
    | None -> ""
    | Some (pkg, opam) ->
        let synopsis, license, homepage, dev_repo, maintainer, tags, description
            =
          show_package_meta pkg opam
        in
        show_meta_line "Synopsis" synopsis;
        show_meta_line "License" license;
        show_meta_line "Homepage" homepage;
        (* Only surface dev-repo when it adds information beyond the
           homepage. Many opam files repeat the same github URL for
           both, which just makes the info page noisier. *)
        if dev_repo <> homepage then show_meta_line "Source" dev_repo;
        show_meta_line "Maintainer" maintainer;
        show_meta_line "Tags" tags;
        description
  in
  (match binaries with
  | [] -> ()
  | bs -> show_meta_line "Binaries" (String.concat ", " bs));
  (match overlay with
  | None -> ()
  | Some tag ->
      show_meta_line "Overlay" (Fmt.str "%a" Fmt.(styled `Cyan string) tag));
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
      Fmt.pr "            %a@,"
        Fmt.(styled `Faint string)
        "(host check skipped because --os is set)"
  | Some st ->
      (* Every depext declared, with the uninstalled ones marked.
         Missing tokens are styled in yellow so they stand out even
         when "(missing)" is the only textual marker. *)
      let render p =
        let name = OpamSysPkg.to_string p in
        if OpamSysPkg.Set.mem p st.missing then
          Fmt.str "%a" Fmt.(styled `Yellow string) (name ^ " (missing)")
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
        Fmt.pr "            %a@,"
          Fmt.(styled `Faint string)
          (Fmt.str "Run: sudo apt install %s" (String.concat " " missing_names)));
  (match repositories with
  | [] -> ()
  | rows ->
      Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Repositories:";
      (* Two columns: [@handle (version)] left-padded to the longest
         token so URLs line up. *)
      let left = List.map (fun (h, v, _) -> Fmt.str "@%s (%s)" h v) rows in
      let col = List.fold_left (fun m s -> max m (String.length s)) 0 left in
      List.iter2
        (fun (_, _, url) l ->
          Fmt.pr "  %a  %s@,"
            Fmt.(styled `Cyan string)
            (Fmt.str "%-*s" col l) url)
        rows left);
  (match description with
  | "" -> ()
  | body ->
      Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Description:";
      String.split_on_char '\n' body
      |> List.iter (fun line -> Fmt.pr "  %s@," line));
  Fmt.pr "@]@."

let show_cmd =
  let run () data_dir cache_dir refresh registry toolchain targets with_repos
      with_deps tree only_depexts os_override =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let _ = registry in
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf_host = Oi.Pipeline.make_conf ~platform ~ocaml_version in
    let conf =
      match os_override with
      | None -> conf_host
      | Some os -> resolve_os_override conf_host os
    in
    let toolchain =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:false
        toolchain
    in
    let conf, tc_ctx = Oi.Pipeline.toolchain_views toolchain conf in
    (* Toolchain overlay's packages_dirs drive the consumer solve too:
       when set, they REPLACE [get_packages_dirs] rather than stack
       on top, otherwise the default flow would add [relocatable]
       whose [ocaml-base-compiler.5.5.0] conflicts with the toolchain
       pin. *)
    let tc_pkg_dirs =
      match toolchain with
      | None -> None
      | Some (info : Oi.Toolchain.info) -> Some info.packages_dirs
    in
    let cwd_s, _ = resolved_cwd fs in
    (* No pre-rewrite of the targets: solve them as-is. The layer
       index is consulted later only when the solve doesn't yield a
       matching package — same policy as [oi run]. Pre-rewriting
       would inject a [@default/X] handle pin that overrides any
       user [--with=X.VERSION] constraint. *)
    (* One [extract_handle_pins] pass handles both user-typed
       [@handle/pkg] and the rewrites we just introduced: the
       handle is routed into [with_repos], the stripped package spec
       replaces the original token, and a [handle_pin] is recorded
       so the overlay version gets pinned later. *)
    let targets, with_repos, target_pins =
      extract_handle_pins ~with_repos targets
    in
    let with_deps, with_repos, with_pins =
      extract_handle_pins ~with_repos with_deps
    in
    let handle_pins = target_pins @ with_pins in
    let extra_deps, url_project =
      Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    (* Only consult the local project's declarations when the user did
       not name an explicit target; otherwise [oi show pkg] inside a
       project would silently pull the project's own deps into the
       solve and produce misleading output. *)
    let project_extras, project_pins, project_overlays, project_deps =
      if targets <> [] then ([], [], [], [])
      else
        match Oi.Project.load ~fs cwd_s with
        | exception Sys_error _ -> ([], [], [], [])
        | exception Eio.Exn.Io _ -> ([], [], [], [])
        | p -> (p.extra_repos, p.pins, p.overlays, p.deps)
    in
    let project_extras = project_extras @ url_project.extra_repos in
    let project_pins = project_pins @ url_project.pins in
    let project_overlays = project_overlays @ url_project.overlays in
    let project_overlays =
      Oi.Pipeline.filter_compatible_overlays ~reporepo_path:(reporepo_path ())
        ~toolchain project_overlays
    in
    let with_repos = project_overlays @ with_repos in
    let cli_extras = cli_extra_repos ~fs ~sys with_repos in
    let all_extras = merge_extras ~cli:cli_extras ~project:project_extras in
    let extra_pkg_dirs =
      Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras
    in
    let pin_dir =
      Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh project_pins
    in
    let base_pkg_dirs =
      match tc_pkg_dirs with
      | Some dirs -> dirs
      | None -> Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ()
    in
    let packages_dirs =
      Stdlib.Option.to_list pin_dir @ extra_pkg_dirs @ base_pkg_dirs
    in
    let extra_constraints = Oi.Project.Script.constraints extra_deps in
    let handle_constraints =
      handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
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
    let names =
      List.map OpamPackage.Name.of_string targets
      @ project_dep_names @ extra_names @ url_names
    in
    if names = [] then
      Oi.Error.config_error
        "oi show: nothing to show (no TARGET, no --with, and no *.opam files \
         in %s)"
        cwd_s;
    let cache_root = Oi.Cache.root_s cache in
    let build_prefix = cache_root / "build" / "prefix" in
    let ctx =
      Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs ~conf
        ?toolchain:tc_ctx ()
    in
    let pkgs =
      match
        Oi.Solver.solve ~fs ~cache_root ctx ~packages_dirs
          ~constraints:extra_constraints names
      with
      | Ok pkgs -> pkgs
      | Error msg ->
          (* "No known implementations at all" usually means the user
             typed a name that isn't actually an opam package - a
             common confusion when a project's display name differs
             from its package name (e.g. "ocurrent" vs [current]).
             Walk the packages_dirs for substring matches and include
             them in the error so the fix is obvious. *)
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
          (* Bidirectional substring match: a package is a candidate if
             either the typed target contains the package's name (e.g.
             target="ocurrent" matches package "current") or the
             package's name contains the typed target (e.g.
             target="curr" matches "current"). Case-insensitive. Both
             sides need at least four letters: shorter names (like
             [re]) otherwise match as spurious fragments of unrelated
             targets. *)
          let suggest_for target =
            let lower = String.lowercase_ascii target in
            if String.length lower < 4 then []
            else
              List.concat_map
                (fun dir -> try Sys.readdir dir |> Array.to_list with _ -> [])
                packages_dirs
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
    in
    let d10 =
      Oi.Pipeline.make_d10 ~sys ~fs
        ~clock:(clock :> D10.Config.clk)
        ~cache ~os_key
    in
    let action_plan = Oi.Plan.build ctx ~d10 ~packages_dirs pkgs in
    if tree then begin
      let plan =
        Oi.Plan.resolve ctx ~packages_dirs ~cache_root ~os_key
          ~ocaml_version:conf.ocaml_version action_plan
      in
      Fmt.pr "%a@." Oi.Plan.pp plan
    end
    else
      let all_depexts, dep_status =
        show_depexts ~ctx ~packages_dirs ~action_plan ~os_override
      in
      if only_depexts then
        (* Always print every depext, one per line, with no status
           marking. Intended for piping into a package manager; the
           caller handles which ones are already installed. *)
        let _ = dep_status in
        OpamSysPkg.Set.iter
          (fun p -> Fmt.pr "%s@." (OpamSysPkg.to_string p))
          all_depexts
      else
        let target_label = show_target_label ~targets ~project_deps in
        let overlay = show_overlay_label ~with_repos in
        let n_cached, n_source = show_counts action_plan in
        let primary =
          show_primary_meta ~action_plan ~targets ~project_deps ~cwd:cwd_s
        in
        let target_version, target_opam, target_layer_hash =
          match primary with
          | None -> ("", None, None)
          | Some (From_node n) ->
              ( OpamPackage.Version.to_string (OpamPackage.version n.pkg),
                Some (n.pkg, n.opam),
                Some n.layer_hash )
          | Some (From_project_opam (pkg, opam)) ->
              (* Project *.opam files rarely pin a real version;
                 "dev" isn't useful on a user-facing line, so we
                 suppress the version column here. *)
              ("", Some (pkg, opam), None)
        in
        let repositories = show_repositories ?toolchain ~with_repos () in
        let binaries =
          match target_layer_hash with
          | None -> []
          | Some h -> show_package_binaries ~cache_root ~os_key ~layer_hash:h
        in
        show_render_info ~target_label ~target_version ~target_opam ~overlay
          ~os_key ~ocaml_version:conf.ocaml_version ~n_cached ~n_source
          ~all_depexts ~dep_status ~repositories ~binaries
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Opam package, binary name, or $(b,@HANDLE/PKG). Omitted: read \
             $(b,*.opam) in the current directory."
          [])
  in
  let tree =
    Arg.(
      value & flag
      & info ~doc:"Print the full per-package build plan." [ "tree" ])
  in
  let only_depexts =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print system packages, one per line, suitable for piping to \
             $(b,apt), $(b,apk), or $(b,dnf)."
          [ "only-depexts" ])
  in
  let os_override =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"OS"
          ~doc:
            "Evaluate depexts for $(b,OS) instead of the host. Accepts any tag \
             $(b,dockerfile-opam) recognises ($(b,alpine-3.23), \
             $(b,ubuntu-22.04), $(b,fedora-43), $(b,alpine), $(b,ubuntu), \
             ...). The host-installed check is skipped."
          [ "os" ])
  in
  let info =
    Cmd.info "show" ~doc:"Summarise a package's build plan and depexts"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve for $(b,TARGET) and print its metadata, package count, \
             reporepo pins, and declared system dependencies. No sources are \
             fetched and no builds run.";
          `P "With no $(b,TARGET), reads $(b,*.opam) in the current directory.";
          `S "MODES";
          `I
            ( "(default)",
              "Summary page: opam metadata, overlay tag, package counts, \
               binaries, depexts with uninstalled ones marked, and the pinned \
               reporepo overlays." );
          `I
            ( "$(b,--tree)",
              "Full per-package build plan: layer hashes, source URLs, \
               resolved build and install commands." );
          `I
            ( "$(b,--only-depexts)",
              "Every declared depext, one per line, no formatting. For piping \
               into a system package manager." );
          `S Manpage.s_examples;
          `Pre
            "  oi show utop\n\
            \  oi show --tree utop\n\
            \  sudo apt install \\$(oi show --only-depexts @avsm/tangled)\n\
            \  oi show --only-depexts --os=fedora-43";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ toolchain_term $ targets $ with_repos_term
      $ with_deps_term $ tree $ only_depexts $ os_override)

(* -- env ----------------------------------------------------------------- *)

let env_cmd =
  let run () data_dir cache_dir refresh with_repos with_deps jobs toolchain =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let dune_cache_root = Oi.Cache.dune_root cache in
    (* Detect _oi/ project directory. A pre-existing _oi/prefix is
       reused as-is UNLESS the user passes --with-repo or --with, which
       demands a fresh solve to honour the additions. *)
    let cwd_s, cwd = resolved_cwd fs in
    let oi_prefix = cwd_s / "_oi" / "prefix" in
    let want_extras =
      with_repos <> [] || with_deps <> [] || toolchain <> None
    in
    let conf = Oi.Pipeline.make_conf ~platform ~ocaml_version in
    let tc_info =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:true
        toolchain
    in
    let conf, tc_ctx = Oi.Pipeline.toolchain_views tc_info conf in
    let prefix =
      if (not want_extras) && path_exists cwd "_oi/prefix" then oi_prefix
      else begin
        (* Fall back to a minimal compiler-only prefix, optionally
           extended with CLI extras + with-deps. *)
        Oi.Pipeline.init_opam_root ~fs ~data_dir;
        ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
        let extra_cli, url_project =
          Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
        in
        let url_overlays =
          Oi.Pipeline.filter_compatible_overlays
            ~reporepo_path:(reporepo_path ()) ~toolchain:tc_info
            url_project.overlays
        in
        let extras =
          merge_extras
            ~cli:(cli_extra_repos ~fs ~sys (with_repos @ url_overlays))
            ~project:url_project.extra_repos
        in
        let extra_constraints = Oi.Project.Script.constraints extra_cli in
        let extra_names =
          List.filter_map
            (fun (d : Oi.Project.Script.dep) ->
              if OpamPackage.Name.to_string d.name = "ocaml" then None
              else Some d.name)
            extra_cli
          @ List.map OpamPackage.Name.of_string url_project.roots
        in
        let layer_hashes =
          Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
            ~os_key ~refresh ~extra_repos:extras ~pins:url_project.pins ?jobs
            ?toolchain:tc_info ~constraints:extra_constraints
            (OpamPackage.Name.of_string "ocaml" :: extra_names)
        in
        Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      end
    in
    let vars =
      Oi.Solver.Env.env_vars ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
    in
    let current_path =
      try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin"
    in
    let tools = tools_dir_for ~cwd:cwd_s in
    let path_prefix =
      match tools with
      | None -> prefix / "bin"
      | Some t -> (t / "bin") ^ ":" ^ (prefix / "bin")
    in
    List.iter
      (fun (k, v) ->
        let v = if k = "PATH" then path_prefix ^ ":" ^ current_path else v in
        Fmt.pr "export %s=\"%s\"@." k v)
      vars
  in
  let info =
    Cmd.info "env" ~doc:"Print shell exports for the project environment"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Print $(b,export) statements pointing $(b,PATH), $(b,OCAMLLIB), \
             and the other OCaml env vars at the project prefix. The \
             non-$(b,direnv) equivalent of the $(b,.envrc) $(b,oi sync) \
             writes.";
          `Pre "  eval \"\\$(oi env)\"";
          `P
            "Implicitly syncs first if $(b,_oi/prefix/) is missing or stale. \
             Passing $(b,--with), $(b,--with-repo), or $(b,--toolchain) forces \
             a re-sync with those extras folded in.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ with_repos_term $ with_deps_term $ jobs_term $ toolchain_term)

(* -- init ---------------------------------------------------------------- *)

(* -- search -------------------------------------------------------------- *)

(* Glob match with [*] wildcard. Used for package-name filtering in
   [oi search]; binary-name filtering goes through SQL's [LIKE] inside
   [D10.Index]. *)
let glob_matches ~pattern name =
  if not (String.contains pattern '*') then pattern = name
  else
    let segs = String.split_on_char '*' pattern in
    let anchored_start = not (String.starts_with ~prefix:"*" pattern) in
    let anchored_end = not (String.ends_with ~suffix:"*" pattern) in
    let rec walk pos = function
      | [] -> true
      | [ last ] ->
          if anchored_end then
            String.ends_with ~suffix:last name
            && String.length name - String.length last >= pos
          else
            let _ = last in
            true
      | "" :: rest -> walk pos rest
      | seg :: rest -> (
          let rec find_from start =
            if start + String.length seg > String.length name then None
            else if String.sub name start (String.length seg) = seg then
              Some start
            else find_from (start + 1)
          in
          match find_from pos with
          | None -> false
          | Some i when anchored_start && pos = 0 && i <> 0 -> false
          | Some i -> walk (i + String.length seg) rest)
    in
    walk 0 (List.filter (fun s -> s <> "") segs)
    && ((not anchored_start)
       || List.hd segs = ""
       || String.starts_with ~prefix:(List.hd segs) name)

(* Scan every [repos/overlay-<h>-<v>/packages/] tree under [data_dir]
   for package names matching [pattern]. Returns a list of
   [(handle, version_tag, pkg_name, pkg_version)] rows, one per
   [<name>/<name.version>/opam] found. [version_tag] is the overlay
   version that the clone was pinned at.

   Keeping every version here lets the caller pick latest-per-
   (overlay, name) or expose all with [--all-versions]. *)
let scan_declared_packages ~data_dir ~pattern ~overlay_filter =
  let repos = data_dir / "repos" in
  if not (Sys.file_exists repos) then []
  else
    let entries = Sys.readdir repos |> Array.to_list in
    let rows = ref [] in
    List.iter
      (fun entry ->
        if String.starts_with ~prefix:"overlay-" entry then
          (* Parse overlay-<handle>-<version>. The version always
             matches [YYYYMMDD.N] so it contains no dashes; the
             handle is everything between "overlay-" and the last
             dash. *)
          let rest =
            String.sub entry (String.length "overlay-")
              (String.length entry - String.length "overlay-")
          in
          match String.rindex_opt rest '-' with
          | None -> ()
          | Some i ->
              let handle = String.sub rest 0 i in
              let version =
                String.sub rest (i + 1) (String.length rest - i - 1)
              in
              let keep =
                match overlay_filter with
                | [] -> true
                | xs -> List.mem handle xs
              in
              if keep then
                let pkgs_dir = repos / entry / "packages" in
                if Sys.file_exists pkgs_dir then
                  Array.iter
                    (fun name ->
                      if glob_matches ~pattern name then
                        let name_dir = pkgs_dir / name in
                        if Sys.is_directory name_dir then
                          Array.iter
                            (fun pkg_s ->
                              match OpamPackage.of_string_opt pkg_s with
                              | None -> ()
                              | Some p ->
                                  let v =
                                    OpamPackage.Version.to_string
                                      (OpamPackage.version p)
                                  in
                                  rows := (handle, version, name, v) :: !rows)
                            (Sys.readdir name_dir))
                    (Sys.readdir pkgs_dir))
      entries;
    List.rev !rows

(* Rank of a search-result state. Used to collapse redundant rows
   for the same (kind, overlay, name, version): if a package is
   built locally AND declared in the overlay, we keep [Local] and
   drop [Declared]. *)
type state = Local | Remote | Declared

let state_rank = function Local -> 0 | Remote -> 1 | Declared -> 2

let state_label = function
  | Local -> "local"
  | Remote -> "remote"
  | Declared -> "declared"

let state_styled st =
  let style =
    match st with Local -> `Green | Remote -> `Cyan | Declared -> `Yellow
  in
  Fmt.str "%a" Fmt.(styled style string) (state_label st)

(* Latest version per key, or every version when [all_versions] is set.
   Versions are compared via [OpamPackage.Version.compare]. *)
let trim_to_latest ~all_versions rows key version =
  if all_versions then rows
  else
    let by_key = Hashtbl.create 32 in
    List.iter
      (fun r ->
        let k = key r in
        let v = version r in
        match Hashtbl.find_opt by_key k with
        | None -> Hashtbl.replace by_key k r
        | Some prev
          when OpamPackage.Version.compare
                 (OpamPackage.Version.of_string v)
                 (OpamPackage.Version.of_string (version prev))
               > 0 ->
            Hashtbl.replace by_key k r
        | Some _ -> ())
      rows;
    Hashtbl.fold (fun _ r acc -> r :: acc) by_key []

(* One row of search output. Same shape for [bin] and [pkg] kinds so the
   caller can print them in a single uniform table. *)
type search_row = {
  kind : [ `Bin | `Pkg ];
  overlay : string; (* "@handle" or "-" *)
  binary : string option; (* filled for [Bin]; [None] for [Pkg] *)
  pkg_name : string;
  pkg_version : string;
  state : state;
  hash : string option; (* present for Local / Remote, absent for Declared *)
}

let search_cmd =
  let run () data_dir cache_dir registry all_versions overlay_filter long
      pattern =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, _platform, os_key, cache =
      bootstrap env cache_dir
    in
    (* Accept [@handle/PATTERN] as a shortcut for
       [--overlay=handle PATTERN]. Combines with any [--overlay] flags
       the user already passed. *)
    let pattern, overlay_filter =
      match split_handle_prefix pattern with
      | None -> (pattern, overlay_filter)
      | Some (h, rest) -> (rest, h :: overlay_filter)
    in
    let clk = (clock :> D10.Config.clk) in
    let index_path = ensure_local_index ~sys ~fs ~clock:clk ~cache ~os_key in
    (match ensure_remote_index ~sys ~fs ~cache ~os_key ~registry with
    | Some remote_path -> merge_remote_into_local ~index_path ~remote_path
    | None -> ());
    let db = D10.Index.open_ ~path:index_path in
    let d10 : D10.Config.t =
      { sys; fs; clock = clk; root = Oi.Cache.root cache; os_key }
    in
    let overlay_of = function
      | None -> "-"
      | Some (h, _) ->
          if overlay_filter <> [] && not (List.mem h overlay_filter) then "-"
            (* shouldn't happen after later filter, but defensive *)
          else "@" ^ h
    in
    (* Binary matches from the index. Each hit emits one [Bin] row. *)
    let bin_rows =
      List.map
        (fun (bin, pkg_name, pkg_ver, hash, overlay) ->
          let st = if D10.Layer.succeeded d10 ~hash then Local else Remote in
          {
            kind = `Bin;
            overlay = overlay_of overlay;
            binary = Some bin;
            pkg_name;
            pkg_version = pkg_ver;
            state = st;
            hash = Some hash;
          })
        (D10.Index.search_binary db ~pattern ~os_key)
    in
    (* Built-package matches from the index. *)
    let pkg_built_rows =
      List.map
        (fun (pkg_name, pkg_ver, hash, overlay) ->
          let st = if D10.Layer.succeeded d10 ~hash then Local else Remote in
          {
            kind = `Pkg;
            overlay = overlay_of overlay;
            binary = None;
            pkg_name;
            pkg_version = pkg_ver;
            state = st;
            hash = Some hash;
          })
        (D10.Index.search_package db ~pattern ~os_key)
    in
    (* Declared-package matches scanned from overlay clones. *)
    let pkg_declared_rows =
      scan_declared_packages ~data_dir ~pattern ~overlay_filter
      |> List.map (fun (handle, _ov_version, name, version) ->
          {
            kind = `Pkg;
            overlay = "@" ^ handle;
            binary = None;
            pkg_name = name;
            pkg_version = version;
            state = Declared;
            hash = None;
          })
    in
    (* Apply overlay filter everywhere. The [@default] tag sits on the
       base opam-repository clone, so passing [--overlay=default] keeps
       base-repo rows. *)
    let filter_by_overlay rows =
      match overlay_filter with
      | [] -> rows
      | xs ->
          List.filter
            (fun r ->
              let h =
                if String.length r.overlay > 1 && r.overlay.[0] = '@' then
                  String.sub r.overlay 1 (String.length r.overlay - 1)
                else r.overlay
              in
              List.mem h xs)
            rows
    in
    let all_rows =
      filter_by_overlay bin_rows
      @ filter_by_overlay pkg_built_rows
      @ filter_by_overlay pkg_declared_rows
    in
    (* Collapse redundant rows for the same package: a locally built
       package also appears as a [Declared] row from the overlay scan;
       keep the strongest state ([Local] > [Remote] > [Declared]). *)
    let strongest_per_key =
      let by_key = Hashtbl.create 32 in
      List.iter
        (fun r ->
          let k = (r.kind, r.overlay, r.pkg_name, r.pkg_version, r.binary) in
          match Hashtbl.find_opt by_key k with
          | None -> Hashtbl.replace by_key k r
          | Some prev when state_rank r.state < state_rank prev.state ->
              Hashtbl.replace by_key k r
          | Some _ -> ())
        all_rows;
      Hashtbl.fold (fun _ r acc -> r :: acc) by_key []
    in
    (* Collapse to latest version per (kind, overlay, name, binary)
       unless [--all-versions]. *)
    let displayed =
      trim_to_latest ~all_versions strongest_per_key
        (fun r -> (r.kind, r.overlay, r.pkg_name, r.binary))
        (fun r -> r.pkg_version)
    in
    (* Stable ordering for readable output: bins first, then pkgs,
       alphabetic by name, version descending. *)
    let sorted =
      List.sort
        (fun a b ->
          let c =
            match (a.kind, b.kind) with
            | `Bin, `Pkg -> -1
            | `Pkg, `Bin -> 1
            | _ -> 0
          in
          if c <> 0 then c
          else
            let c = String.compare a.pkg_name b.pkg_name in
            if c <> 0 then c
            else
              let c =
                match (a.binary, b.binary) with
                | Some x, Some y -> String.compare x y
                | None, Some _ -> 1
                | Some _, None -> -1
                | None, None -> 0
              in
              if c <> 0 then c
              else
                OpamPackage.Version.compare
                  (OpamPackage.Version.of_string b.pkg_version)
                  (OpamPackage.Version.of_string a.pkg_version))
        displayed
    in
    if sorted = [] then Fmt.pr "No matches for %s@." pattern
    else begin
      let short_hash = function
        | None -> ""
        | Some h -> String.sub h 0 (min 12 (String.length h))
      in
      List.iter
        (fun r ->
          let kind_s = match r.kind with `Bin -> "bin" | `Pkg -> "pkg" in
          let nv =
            match r.binary with
            | Some b -> Fmt.str "%s (%s.%s)" b r.pkg_name r.pkg_version
            | None -> Fmt.str "%s.%s" r.pkg_name r.pkg_version
          in
          Fmt.pr "%-4s %-14s %-48s %-12s %s@." kind_s r.overlay nv
            (short_hash r.hash) (state_styled r.state);
          if long then
            match r.hash with
            | None -> ()
            | Some h ->
                let deps = D10.Index.deps db ~hash:h in
                if deps = [] then
                  Fmt.pr "  %a@." Fmt.(styled `Faint string) "(no deps)"
                else
                  List.iter
                    (fun (dep_name, dep_ver, dep_hash) ->
                      Fmt.pr "  %a %s.%s@."
                        Fmt.(styled `Faint string)
                        (String.sub dep_hash 0
                           (min 12 (String.length dep_hash)))
                        dep_name dep_ver)
                    deps)
        sorted
    end;
    D10.Index.close db
  in
  let pattern =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PATTERN"
          ~doc:
            "The name or glob to search for. The $(b,*) character is a \
             wildcard. Matching is against binary names and opam package \
             names. Prefix with $(b,@HANDLE/) to restrict the search to a \
             single overlay without passing $(b,--overlay) separately."
          [])
  in
  let all_versions =
    Arg.(
      value & flag
      & info
          ~doc:
            "List every cached version of each match. By default only the \
             latest version per overlay is shown."
          [ "all-versions" ])
  in
  let overlay =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:
            "Restrict results to an overlay. May be given more than once to \
             include several overlays. Equivalent to the $(b,@HANDLE/PATTERN) \
             shorthand."
          [ "overlay" ])
  in
  let long =
    Arg.(
      value & flag
      & info
          ~doc:
            "For built matches, print the direct dependencies of each result. \
             Declared-only rows have no build and therefore no dependency \
             list."
          [ "l"; "long" ])
  in
  let info =
    Cmd.info "search"
      ~doc:"Find binaries and opam packages across caches and overlays"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Look up $(b,PATTERN) across the local layer cache, the remote \
             registry, and every reporepo overlay's package list. One row per \
             match.";
          `S "COLUMNS";
          `I
            ( "$(b,KIND)",
              "$(b,bin) (binary in some layer's $(b,fs/bin/)) or $(b,pkg) \
               (opam metadata)." );
          `I
            ( "$(b,OVERLAY)",
              "$(b,@handle) the match came from, or $(b,-) for pin-depends / \
               untagged layers." );
          `I
            ( "$(b,NAME.VERSION)",
              "Package, with the binary name prefixed for $(b,bin) rows." );
          `I ("$(b,HASH)", "Short layer hash.");
          `I
            ( "$(b,STATE)",
              "$(b,local) (cached), $(b,remote) (fetchable), or $(b,declared) \
               (metadata only, no build)." );
          `S "OPTIONS";
          `I
            ( "$(b,--all-versions)",
              "Every cached/declared version (default: latest only)." );
          `I
            ( "$(b,--overlay=HANDLE)",
              "Filter to one overlay. $(b,@HANDLE/PATTERN) is shorthand." );
          `I ("$(b,-l)", "Print direct deps of each built match.");
          `S Manpage.s_examples;
          `Pre
            "  oi search dune\n\
            \  oi search 'ocaml*'\n\
            \  oi search @avsm/irmin\n\
            \  oi search --overlay=avsm --overlay=default 'fmt*'\n\
            \  oi search --all-versions -l jsont";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ registry_term
      $ all_versions $ overlay $ long $ pattern)

(* -- tool installation --------------------------------------------------- *)

let short_hash h = String.sub h 0 (min 12 (String.length h))

let warn_tool spec fmt =
  Fmt.kstr
    (fun s ->
      Fmt.epr "%a tool %s: %s@."
        Fmt.(styled `Yellow string)
        "WARN" (spec : Oi.Project.Tool.spec).name s)
    fmt

(* Return the layer hash whose [layer.json] declares package name
   [want_name] (any version). Tools get assembled from only their own
   leaf layer — the transitive deps (ocaml, dune, ocamlfind…) are
   already present in the shared d10 cache but are not needed at
   runtime by a native-compiled tool binary, so we leave them out of
   [_oi/tools/] to keep it small and focused. *)
let leaf_hash_for ~fs ~cache ~os_key ~want_name hashes =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let leaf hash =
    match
      D10.Layer.load_meta Eio.Path.(fs / layers_dir / hash / "layer.json")
    with
    | Some m -> (
        match OpamPackage.of_string_opt m.package with
        | Some p
          when OpamPackage.Name.to_string (OpamPackage.name p) = want_name ->
            Some hash
        | _ -> None)
    | None -> None
  in
  List.find_map leaf hashes

(* Solve and install every probed dev tool into [cwd/_oi/tools/]. Each
   tool is its own independent solve so its dep closure never leaks
   into the main project's OCAMLLIB / OCAMLPATH. A tool that fails to
   solve (e.g. pinned to an older ocaml) warns and is skipped; other
   tools still install. Returns the assembled path if at least one
   tool made it in, or [None] if nothing to install. *)
let install_tools ?(quiet = false) ?refresh ?jobs ~proc_mgr ~fs ~clock ~sys
    ~cache ~data_dir ~conf ~os_key ~extra_repos ~pins ?remote ~cwd () =
  let say fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Fmt.pr "%s@." s) fmt
  in
  let install_one (r : Oi.Project.Tool.result) =
    let spec = r.spec in
    let name = OpamPackage.Name.of_string spec.name in
    let constraints =
      match r.version with
      | None -> OpamPackage.Name.Map.empty
      | Some v ->
          OpamPackage.Name.Map.singleton name
            (`Eq, OpamPackage.Version.of_string v)
    in
    try
      let hashes =
        Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
          ~os_key ~extra_repos ~pins ?refresh ?remote ?jobs ~constraints
          [ name ]
      in
      match leaf_hash_for ~fs ~cache ~os_key ~want_name:spec.name hashes with
      | None ->
          warn_tool spec "layer for leaf package not found";
          None
      | Some h ->
          say "Tool %s: %d dep(s) built, leaf layer %s" spec.name
            (List.length hashes - 1)
            (short_hash h);
          Some h
    with
    | Oi.Error.E e ->
        warn_tool spec "%a" Oi.Error.pp e;
        None
    | exn ->
        warn_tool spec "%s" (Printexc.to_string exn);
        None
  in
  match Oi.Project.Tool.(hits (probe ~fs cwd)) with
  | [] ->
      say "No dev tools to install";
      None
  | hits -> (
      let leaves = List.filter_map install_one hits in
      match leaves with
      | [] -> None
      | _ ->
          let tools_dir = cwd / "_oi" / "tools" in
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / tools_dir);
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / tools_dir);
          let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
          let unique = List.sort_uniq String.compare leaves in
          D10.Prefix.assemble d10 ~layer_hashes:unique
            ~dst:Eio.Path.(fs / tools_dir);
          say "Tools assembled at %s (%d tool(s), %d leaf layer(s))" tools_dir
            (List.length leaves) (List.length unique);
          Some tools_dir)

(* -- sync ---------------------------------------------------------------- *)

(* Run a full sync in [cwd]: solve the deps declared in *.opam files,
   build/fetch layers, assemble [cwd]/_oi/prefix, and (re)write .envrc.
   Returns the path to the assembled prefix. When [quiet] is true,
   narration goes to Logs.info (hidden at default verbosity); otherwise
   it prints to stdout. *)
let do_sync ?(quiet = false) ?(refresh = false) ?(with_repos = [])
    ?(with_deps = []) ?jobs ?toolchain ~proc_mgr ~fs ~clock ~sys ~platform
    ~os_key ~cache ~data_dir ~registry ~cwd () =
  let say fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Fmt.pr "%s@." s) fmt
  in
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let project = Oi.Project.load ~fs cwd in
  let extra_cli, url_project =
    Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
  in
  let deps = project.deps in
  if deps = [] && extra_cli = [] && url_project.roots = [] then
    Oi.Error.config_error "No .opam files found in %s." cwd;
  say "Dependencies from opam files: %s" (String.concat ", " deps);
  if url_project.roots <> [] then
    say "URL-supplied packages: %s" (String.concat ", " url_project.roots);
  let conf = Oi.Pipeline.make_conf ~platform ~ocaml_version in
  (* Resolve the toolchain before assembling the overlay list so the
     overlay-compatibility filter below can see what was requested. *)
  let toolchain =
    Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:true
      toolchain
  in
  let conf = Oi.Toolchain.apply_conf toolchain conf in
  let project_overlays =
    Oi.Pipeline.filter_compatible_overlays ~reporepo_path:(reporepo_path ())
      ~toolchain
      (project.overlays @ url_project.overlays)
  in
  if project_overlays <> [] then
    say "Project overlays (from x-repos): %s"
      (String.concat ", " project_overlays);
  let with_repos = project_overlays @ with_repos in
  let all_extras =
    merge_extras
      ~cli:(cli_extra_repos ~fs ~sys with_repos)
      ~project:(project.extra_repos @ url_project.extra_repos)
  in
  if all_extras <> [] then
    say "Extra repositories: %s"
      (String.concat ", "
         (List.map
            (fun (e : Oi.Project.extra_repo) -> Fmt.str "%s (%s)" e.name e.url)
            all_extras));
  let remote = remote_of_registry registry in
  let extra_constraints = Oi.Project.Script.constraints extra_cli in
  let extra_names =
    List.filter_map
      (fun (d : Oi.Project.Script.dep) ->
        if OpamPackage.Name.to_string d.name = "ocaml" then None
        else Some d.name)
      extra_cli
  in
  let url_names = List.map OpamPackage.Name.of_string url_project.roots in
  let names =
    List.map OpamPackage.Name.of_string deps @ extra_names @ url_names
  in
  let layer_hashes =
    Oi.Pipeline.build ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf ~os_key
      ~extra_repos:all_extras
      ~pins:(project.pins @ url_project.pins)
      ~refresh ~constraints:extra_constraints ?remote ?jobs ?toolchain names
  in
  let oi_dir = cwd / "_oi" in
  let prefix = oi_dir / "prefix" in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / oi_dir);
  let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
  D10.Prefix.assemble d10 ~layer_hashes ~dst:Eio.Path.(fs / prefix);
  let tools =
    install_tools ~quiet ?refresh:(Some refresh) ?jobs ~proc_mgr ~fs ~clock ~sys
      ~cache ~data_dir ~conf ~os_key ~extra_repos:all_extras ~pins:project.pins
      ?remote ~cwd ()
  in
  let envrc_path = Eio.Path.(fs / cwd / ".envrc") in
  let dune_cache_root = Oi.Cache.dune_root cache in
  let envrc = Oi.Solver.Env.envrc_content ~prefix ?tools ~dune_cache_root () in
  (try Eio.Path.unlink envrc_path with Eio.Exn.Io _ -> ());
  Eio.Path.save ~create:(`Exclusive 0o644) envrc_path envrc;
  say "Wrote .envrc (run 'direnv allow' to activate)";
  say "Prefix assembled at %s (%d packages)" prefix (List.length layer_hashes);
  prefix

(* True if [cwd]/_oi/prefix is missing, or any *.opam in [cwd] has been
   modified more recently than the prefix directory. *)
let needs_sync ~cwd ~prefix =
  match Unix.stat prefix with
  | exception Unix.Unix_error _ -> true
  | st ->
      let prefix_mtime = st.Unix.st_mtime in
      let opam_files =
        try
          Sys.readdir cwd |> Array.to_list
          |> List.filter (fun f ->
              Filename.check_suffix f ".opam"
              && Filename.chop_suffix f ".opam" <> "")
        with Sys_error _ -> []
      in
      List.exists
        (fun f ->
          try (Unix.stat (cwd / f)).Unix.st_mtime > prefix_mtime
          with Unix.Unix_error _ -> false)
        opam_files

let sync_cmd =
  let run () data_dir cache_dir refresh registry with_repos with_deps jobs
      toolchain =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let cwd, _ = resolved_cwd fs in
    ignore
      (do_sync ~refresh ~with_repos ~with_deps ?jobs ?toolchain ~proc_mgr ~fs
         ~clock ~sys ~platform ~os_key ~cache ~data_dir ~registry ~cwd ())
  in
  let info =
    Cmd.info "sync" ~doc:"Install project dependencies into _oi/prefix/"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve the $(b,*.opam) files in the current directory, install the \
             resulting packages into $(b,_oi/prefix/), and write $(b,.envrc) \
             for $(b,direnv).";
          `P
            "Activate by running $(b,direnv allow), sourcing $(b,.envrc), or \
             $(b,eval \"\\$(oi env)\"). $(b,oi exec) auto-syncs when the \
             prefix is missing or older than any $(b,*.opam); explicit $(b,oi \
             sync) is for after a manifest edit.";
          `S "DEV TOOLS";
          `P
            "Sync also installs editor tooling into $(b,_oi/tools/bin/), \
             prepended to $(b,PATH):";
          `I ("$(b,odoc)", "Documentation generator.");
          `I ("$(b,merlin)", "Editor backend for type and error reporting.");
          `I ("$(b,ocaml-lsp-server)", "Language server for editors.");
          `I ("$(b,mdx)", "When $(b,dune-project) uses it.");
          `I
            ( "$(b,ocamlformat)",
              "Pinned to the version $(b,.ocamlformat) requests." );
          `P "$(b,oi config) lists the tools the next sync will install.";
          `S "OPTIONS";
          `I
            ( "$(b,--with=PKG)",
              "Add an extra dep to the solve (same forms as $(b,oi run))." );
          `I
            ( "$(b,--with-repo=URL|HANDLE)",
              "Layer an extra opam repository onto the solve." );
          `I
            ( "$(b,--toolchain=NAME)",
              "Pin the compiler. Project overlays tagged for a different \
               toolchain are dropped from scope." );
          `I ("$(b,-j N)", "Cap parallel builds (default 4).");
          `I
            ( "$(b,--refresh)",
              "Force-refetch repos, pins, and URL clones. Caches refresh on \
               their own after 24h." );
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ with_deps_term $ jobs_term
      $ toolchain_term)

(* -- add ----------------------------------------------------------------- *)

(* "op version" split from an opam [version_constraint], as two raw
   strings suitable for {!Oi.Project.Dune.add_dependency}. *)
let constr_to_op_ver (op, ver) =
  (OpamFormula.string_of_relop op, OpamPackage.Version.to_string ver)

let add_cmd =
  let run () data_dir cache_dir refresh registry with_repos toolchain package
      pkg_spec =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let cwd, _ = resolved_cwd fs in
    (* Fail fast before the sync's 10-second repo refresh. *)
    let dp = Oi.Project.Dune.load ~fs ~cwd in
    if not (Oi.Project.Dune.generate_opam_files dp) then
      Oi.Error.config_error
        "dune-project does not have (generate_opam_files): oi add only \
         supports projects where dune owns the *.opam files";
    (match (package, Oi.Project.Dune.package_names dp) with
    | Some p, names when not (List.mem p names) ->
        Oi.Error.config_error
          "no (package (name %s) …) stanza in dune-project (declared: %s)" p
          (if names = [] then "none" else String.concat ", " names)
    | Some _, _ | _, [ _ ] | _, [] -> ()
    | None, many ->
        Oi.Error.config_error
          "multiple packages in dune-project (%s); re-run with -p PKG to pick \
           one"
          (String.concat ", " many));
    let dep = Oi.Project.Script.parse_cli_dep pkg_spec in
    let dep_name = OpamPackage.Name.to_string dep.name in
    let op_ver = Stdlib.Option.map constr_to_op_ver dep.constraint_ in
    let render_constraint = function
      | None -> ""
      | Some (op, ver) -> Fmt.str " %s %s" op ver
    in
    (* Phase 1: prove the solve succeeds with the new dep included. If
       solve fails, [do_sync] raises before we touch any project files. *)
    Fmt.pr "Solving %s%s into the project...@." dep_name
      (render_constraint op_ver);
    ignore
      (do_sync ~refresh ~with_repos ~with_deps:[ pkg_spec ] ?toolchain ~proc_mgr
         ~fs ~clock ~sys ~platform ~os_key ~cache ~data_dir ~registry ~cwd ());
    (* Phase 2: edit dune-project. Reload in case something touched it
       during the sync (shouldn't, but cheap to be defensive). *)
    let dp = Oi.Project.Dune.load ~fs ~cwd in
    let dp' =
      Oi.Project.Dune.add_dependency dp ?package ~name:dep_name
        ~constraint_:op_ver ()
    in
    Oi.Project.Dune.save ~fs dp';
    Fmt.pr "Updated dune-project: added %s%s@." dep_name
      (render_constraint op_ver);
    (* Phase 3: regenerate *.opam via dune build inside the assembled
       prefix — dune itself comes from [_oi/prefix/bin]. *)
    let prefix = cwd / "_oi" / "prefix" in
    let tools = tools_dir_for ~cwd in
    let env =
      Oi.Solver.Env.make_env ~prefix ?tools
        ~dune_cache_root:(Oi.Cache.dune_root cache) ()
    in
    Fmt.pr "Running dune build to regenerate *.opam...@.";
    ( Eio.Switch.run @@ fun sw ->
      let child =
        Eio.Process.spawn ~sw proc_mgr ~env
          ~cwd:Eio.Path.(fs / cwd)
          [ prefix / "bin" / "dune"; "build" ]
      in
      match Eio.Process.await child with
      | `Exited 0 -> ()
      | `Exited n ->
          Oi.Error.msg
            "dune build exited with code %d; dune-project was updated but \
             *.opam regeneration failed"
            n
      | `Signaled n -> Oi.Error.msg "dune build killed by signal %d" n );
    (* Phase 4: re-sync so the prefix reflects the committed *.opam. *)
    Fmt.pr "Re-syncing to pick up regenerated *.opam...@.";
    ignore
      (do_sync ~quiet:true ~refresh:false ~with_repos ~with_deps:[] ?toolchain
         ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache ~data_dir ~registry
         ~cwd ());
    Fmt.pr "Done.@."
  in
  let pkg_spec =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PKG"
          ~doc:
            "The opam package to add. A plain name ($(b,fmt)) takes the \
             version the solver picks; a dotted form ($(b,fmt.0.9.5)) or a \
             relop ($(b,fmt>=0.9)) pins the dependency to a specific version."
          [])
  in
  let package =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"NAME"
          ~doc:
            "The name of the $(b,\\(package …\\)) stanza in $(b,dune-project) \
             that receives the new dependency. Required only when the project \
             declares more than one package."
          [ "p"; "package" ])
  in
  let info =
    Cmd.info "add" ~doc:"Add a new dependency to the current project"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Bring $(b,PKG) into the current project in four steps: solve with \
             $(b,PKG) added; if the solve succeeds, edit $(b,dune-project); \
             run $(b,dune build) so dune regenerates $(b,*.opam); re-sync to \
             reconcile the prefix.";
          `P
            "Failed solves leave the tree untouched, so $(b,oi add) doubles as \
             a compatibility probe.";
          `P
            "Requires $(b,\\(generate_opam_files\\)) in $(b,dune-project). \
             Pass $(b,-p NAME) to pick a stanza when the project declares more \
             than one package.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ toolchain_term $ package $ pkg_spec)

(* -- exec ---------------------------------------------------------------- *)

let exec_cmd =
  let run () data_dir cache_dir refresh registry with_repos with_deps jobs
      toolchain cmd args =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let cwd, _ = resolved_cwd fs in
    let prefix = cwd / "_oi" / "prefix" in
    (* Any --with-repo / --with / --toolchain flag forces a re-sync
       even if the prefix is fresh, so the extras and toolchain make
       it into the build. *)
    let forced = with_repos <> [] || with_deps <> [] || toolchain <> None in
    if forced || needs_sync ~cwd ~prefix then begin
      Logs.info (fun m -> m "Syncing %s before exec" cwd);
      ignore
        (do_sync ~quiet:true ~refresh ~with_repos ~with_deps ?jobs ?toolchain
           ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache ~data_dir
           ~registry ~cwd ())
    end;
    let tools = tools_dir_for ~cwd in
    let conf = Oi.Pipeline.make_conf ~platform ~ocaml_version in
    let tc_info =
      Oi.Pipeline.resolve_toolchain ~fs ~sys ~data_dir ~conf ~install:false
        toolchain
    in
    let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info tc_info in
    let env_arr =
      Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ?tools
        ~dune_cache_root:(Oi.Cache.dune_root cache) ()
    in
    exit (run_exec proc_mgr ~env:env_arr (cmd :: args))
  in
  let cmd =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"CMD" ~doc:"The command to execute." [])
  in
  let args =
    Arg.(
      value & pos_right 0 string []
      & info ~docv:"ARG"
          ~doc:
            "Arguments passed through to $(b,CMD). Use $(b,--) to separate \
             them from $(b,oi)'s own flags."
          [])
  in
  let info =
    Cmd.info "exec" ~doc:"Run a command in the project environment"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Run $(b,CMD) in the same environment $(b,oi sync) sets up: \
             $(b,_oi/prefix/bin) first on $(b,PATH), OCaml env vars pointing \
             at the prefix, dev tools ($(b,ocamlformat), \
             $(b,ocaml-lsp-server), $(b,odoc), $(b,merlin), $(b,mdx)) on \
             $(b,PATH).";
          `P
            "Auto-syncs first when the prefix is missing or older than any \
             $(b,*.opam). $(b,--with), $(b,--with-repo), and $(b,--toolchain) \
             force a re-sync.";
          `Pre
            "  oi exec dune build\n\
            \  oi exec -- ocamlformat --check .\n\
            \  oi exec utop";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ with_deps_term $ jobs_term
      $ toolchain_term $ cmd $ args)

(* -- config -------------------------------------------------------------- *)

let config_cmd =
  let run () cache_dir data_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, _clock, sys, _platform, os_key, _cache =
      bootstrap env cache_dir
    in
    Fmt.pr "@[<v>%a@," Fmt.(styled `Bold string) "Platform";
    Fmt.pr "  os-key:     %s@," os_key;
    Fmt.pr "  ocaml:      %s (relocatable)@," ocaml_version;
    Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Directories";
    Fmt.pr "  data:       %s@," data_dir;
    Fmt.pr "  cache:      %s@," cache_dir;
    Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Registry";
    Fmt.pr "  url:        %s@," default_registry;
    Fmt.pr "  index TTL:  %gs@," remote_index_max_age;
    Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Toolchains";
    Fmt.pr "  install root:  %s@," (Oi.Toolchain.default_root ());
    List.iter
      (fun (s : Oi.Toolchain.summary) ->
        let url_with_ref =
          match s.ref_ with Some r -> Fmt.str "%s#%s" s.url r | None -> s.url
        in
        let mode_tag =
          if s.relocatable then
            Fmt.str "[%a]" Fmt.(styled `Green string) "relocatable"
          else Fmt.str "[%a]" Fmt.(styled `Yellow string) "fixed-prefix"
        in
        Fmt.pr "  %a  %s  %s@,"
          Fmt.(styled `Bold string)
          s.handle mode_tag url_with_ref;
        if s.depends <> [] then
          Fmt.pr "    depends:    %s@," (String.concat ", " s.depends);
        Fmt.pr "    roots:      %s@," (String.concat ", " s.roots);
        if s.relocatable then ()
        else
          match s.installs with
          | [] ->
              Fmt.pr "    status:     %a@,"
                Fmt.(styled `Faint string)
                "not installed"
          | xs ->
              List.iter
                (fun (path, ready) ->
                  let status =
                    if ready then
                      Fmt.str "%a" Fmt.(styled `Green string) "ready"
                    else Fmt.str "%a" Fmt.(styled `Yellow string) "partial"
                  in
                  Fmt.pr "    install:    %s  %s@," status path)
                xs)
      (Oi.Toolchain.available ());
    Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Base overlays (from reporepo)";
    let base = Oi.Source.Reporepo.base_entries () in
    if base = [] then
      Fmt.pr
        "  %a no 'relocatable' overlay in reporepo %s. Run 'oi repo add' to \
         bootstrap.@,"
        Fmt.(styled `Yellow string)
        "(none)" (reporepo_path ())
    else
      List.iter
        (fun (e : Oi.Source.Reporepo.entry) ->
          let name = "overlay-" ^ e.handle ^ "-" ^ e.version in
          let dir = Oi.Source.Repo.repo_dir ~data_dir name in
          let status =
            if Sys.file_exists (dir / ".git") then
              let hash =
                try D10.Sysops.Git.head_short sys ~dir:Eio.Path.(fs / dir)
                with _ -> "?"
              in
              Fmt.str "%a (%s)" Fmt.(styled `Green string) "cloned" hash
            else Fmt.str "%a" Fmt.(styled `Yellow string) "not cloned"
          in
          Fmt.pr "  %a.%s  %s  %s@,"
            Fmt.(styled `Bold string)
            e.handle e.version status e.url)
        base;
    Fmt.pr "@]@.";
    let cwd_s, _ = resolved_cwd fs in
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
        (* Dev tools: run the probe registry against cwd and print one
           row per tool. Shown in every project (even one with no
           hits), so it's obvious when merlin / odoc would end up in
           [_oi/tools/] after the next sync. *)
        let tool_results = Oi.Project.Tool.probe ~fs cwd_s in
        Fmt.pr "@.Dev tools:@.";
        List.iter
          (fun (r : Oi.Project.Tool.result) ->
            let mark =
              if r.hit then Fmt.str "%a" Fmt.(styled `Green string) "hit"
              else Fmt.str "%a" Fmt.(styled `Faint string) "miss"
            in
            Fmt.pr "  %-18s %-4s %s@." r.spec.name mark r.detail)
          tool_results
  in
  let info =
    Cmd.info "config" ~doc:"Show oi's view of this machine and project"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Print how $(b,oi) sees the current machine and project. First \
             stop when a solve behaves unexpectedly.";
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
            ( "$(b,Project extras)",
              "Any $(b,x-repos:) and $(b,pin-depends:) entries declared in the \
               current directory's $(b,*.opam) files. Only shown when at least \
               one is present." );
          `I
            ( "$(b,Dev tools)",
              "The editor and documentation tools that the next $(b,oi sync) \
               would install. A $(b,hit) means the tool has been requested by \
               the project; a $(b,miss) means it will not be installed." );
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term $ data_dir_term)

(* -- clean --------------------------------------------------------------- *)

(* dir_size and pp_size are now in Oi.Cache *)

let clean_cmd =
  let run () cache_dir data_dir all toolchains sources binaries dune_cache repos
      dry_run =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, _clock, sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let clean_any =
      all || toolchains || sources || binaries || dune_cache || repos
    in
    if not clean_any then begin
      Fmt.pr "@[<v>%a@,@," Fmt.(styled `Bold string) "Cleanable items:";
      let items = Oi.Cache.cleanable_items cache ~data_dir in
      List.iter
        (fun (item : Oi.Cache.item) ->
          let path_s = Eio.Path.native_exn item.path in
          if Sys.file_exists path_s then
            Fmt.pr "  --%-20s %a  %s@," item.label Oi.Cache.pp_size
              (Oi.Cache.size ~sys item.path)
              item.description
          else
            Fmt.pr "  --%-20s %a  %s@," item.label
              Fmt.(styled `Faint string)
              "(empty)" item.description)
        items;
      Fmt.pr "@,Use --all to clean everything, or select specific items.@]@."
    end
    else begin
      let items = Oi.Cache.cleanable_items cache ~data_dir in
      let find_item label =
        List.find_opt (fun (i : Oi.Cache.item) -> i.label = label) items
      in
      let rm label =
        match find_item label with
        | None -> ()
        | Some item ->
            let path_s = Eio.Path.native_exn item.path in
            if Sys.file_exists path_s then begin
              let sz = Oi.Cache.size ~sys item.path in
              if dry_run then
                Fmt.pr "Would remove %s (%a) %s@." label Oi.Cache.pp_size sz
                  path_s
              else begin
                Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / path_s);
                Fmt.pr "Removed %s (%a)@." label Oi.Cache.pp_size sz
              end
            end
      in
      if all || toolchains then rm "toolchains";
      if all || sources then rm "sources";
      if all || binaries then rm "layers";
      if all || binaries then rm "runs";
      if all || dune_cache then rm "dune";
      if all || repos then rm "repos";
      Fmt.pr "Done.@."
    end
  in
  let all =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove every category at once (caches, builds, configuration, \
             cloned repositories)."
          [ "all" ])
  in
  let toolchains =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove fixed-prefix toolchain installs under \
             \\$XDG_CACHE_HOME/oi/toolchains/. Reinstalled on the next \
             $(b,--toolchain=NAME) invocation that needs them."
          [ "toolchains" ])
  in
  let sources =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove cached source tarballs and pinned source clones from the \
             mirror."
          [ "sources" ])
  in
  let binaries =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove the binary layer cache and the per-script build \
             directories."
          [ "layers" ])
  in
  let dune_cache =
    Arg.(
      value & flag
      & info ~doc:"Remove dune's shared cross-project build cache." [ "dune" ])
  in
  let repos =
    Arg.(
      value & flag
      & info
          ~doc:
            "Remove the local clones of the opam package repositories and of \
             any $(b,--with-repo) extras."
          [ "repos" ])
  in
  let dry_run =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print the items that would be removed and their sizes, but do not \
             delete anything."
          [ "n"; "dry-run" ])
  in
  let info =
    Cmd.info "clean" ~doc:"Free up disk space by deleting cached data"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Remove rebuildable cache data. With no flags, lists each \
             category, its disk usage, and the flag that deletes it. Flags are \
             additive — $(b,oi clean --sources --layers) is fine. Nothing \
             under a project's $(b,_oi/) is touched.";
          `I
            ( "$(b,--toolchains)",
              "Fixed-prefix toolchain installs (oxcaml). Rebuilt on next use."
            );
          `I
            ( "$(b,--sources)",
              "Cached source tarballs and pin source clones. Re-fetched from \
               upstream on next solve." );
          `I
            ( "$(b,--layers)",
              "Pre-built binary layer cache and per-script build dirs. Forces \
               source rebuilds on next $(b,oi run)." );
          `I ("$(b,--dune)", "Dune's shared build cache.");
          `I
            ( "$(b,--repos)",
              "Reporepo overlay clones and $(b,--with-repo) extras. Re-cloned \
               on next solve." );
          `I
            ( "$(b,--all)",
              "Every category above, plus the assembled-prefix cache and \
               script-run dirs. Full reset." );
          `P
            "$(b,-n) / $(b,--dry-run) reports which paths would be removed \
             without deleting. Recommended before $(b,--all).";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ cache_dir_term $ data_dir_term $ all $ toolchains
      $ sources $ binaries $ dune_cache $ repos $ dry_run)

(* -- registry list ------------------------------------------------------- *)

let registry_list_cmd =
  let run () cache_dir _data_dir target =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, _clock, sys, _platform, os_key, _cache =
      bootstrap env cache_dir
    in
    let layers_dir = cache_dir / "layers" / os_key in
    match target with
    | None ->
        (* Show overview of all layers *)
        Fmt.pr "@[<v>%a %s@,@," Fmt.(styled `Bold string) "Layer cache" os_key;
        if not (Sys.file_exists layers_dir) then Fmt.pr "  (empty)@,"
        else begin
          let entries =
            Sys.readdir layers_dir |> Array.to_list |> List.sort String.compare
          in
          let total_size = ref 0L in
          List.iter
            (fun hash ->
              let info =
                D10.Layer.load_meta
                  Eio.Path.(fs / layers_dir / hash / "layer.json")
              in
              match info with
              | Some i ->
                  let status =
                    if i.exit_status = 0 then
                      Fmt.str "%a" Fmt.(styled `Green string) "ok"
                    else
                      Fmt.str "%a (exit %d)"
                        Fmt.(styled `Red string)
                        "fail" i.exit_status
                  in
                  let fs_dir = layers_dir / hash / "fs" in
                  let sz = Oi.Cache.size ~sys Eio.Path.(fs / fs_dir) in
                  total_size := Int64.add !total_size sz;
                  Fmt.pr "  %a  %s  %a  %s@,"
                    Fmt.(styled `Faint string)
                    (String.sub hash 0 (min 12 (String.length hash)))
                    status Oi.Cache.pp_size sz i.package
              | None ->
                  Fmt.pr "  %a  %a@,"
                    Fmt.(styled `Faint string)
                    (String.sub hash 0 (min 12 (String.length hash)))
                    Fmt.(styled `Yellow string)
                    "(no metadata)")
            entries;
          Fmt.pr "@,%a %d layers, %a total@,"
            Fmt.(styled `Bold string)
            "Summary:" (List.length entries) Oi.Cache.pp_size !total_size
        end;
        Fmt.pr "@]@."
    | Some pkg_name ->
        (* Show details for a specific package *)
        Fmt.pr "@[<v>%a %s@,@," Fmt.(styled `Bold string) "Package" pkg_name;
        (* Find matching layers *)
        let found = ref false in
        if Sys.file_exists layers_dir then begin
          let entries = Sys.readdir layers_dir |> Array.to_list in
          List.iter
            (fun hash ->
              let info =
                D10.Layer.load_meta
                  Eio.Path.(fs / layers_dir / hash / "layer.json")
              in
              match info with
              | Some i
                when String.length i.package >= String.length pkg_name
                     && String.sub i.package 0 (String.length pkg_name)
                        = pkg_name ->
                  found := true;
                  Fmt.pr "  %a %s@," Fmt.(styled `Bold string) "Layer" hash;
                  Fmt.pr "  package:     %s@," i.package;
                  Fmt.pr "  status:      %s@,"
                    (if i.exit_status = 0 then "ok"
                     else Fmt.str "failed (exit %d)" i.exit_status);
                  Fmt.pr "  created:     %s@,"
                    (let t = Unix.gmtime i.created in
                     Fmt.str "%04d-%02d-%02d %02d:%02d:%02d UTC"
                       (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour
                       t.tm_min t.tm_sec);
                  Fmt.pr "  deps:        %s@,"
                    (if i.deps = [] then "(none)" else String.concat ", " i.deps);
                  Fmt.pr "  parent hash: %s@,"
                    (if i.hashes = [] then "(none)"
                     else
                       String.concat ", "
                         (List.map
                            (fun h -> String.sub h 0 (min 12 (String.length h)))
                            i.hashes));
                  let fs_dir = layers_dir / hash / "fs" in
                  if Sys.file_exists fs_dir then begin
                    let sz = Oi.Cache.size ~sys Eio.Path.(fs / fs_dir) in
                    Fmt.pr "  size:        %a@," Oi.Cache.pp_size sz;
                    (* List files in fs/ *)
                    let files = ref [] in
                    let rec scan dir =
                      if Sys.file_exists dir && Sys.is_directory dir then
                        Array.iter
                          (fun name ->
                            let path = dir / name in
                            if Sys.is_directory path then scan path
                            else
                              let rel =
                                String.sub path
                                  (String.length fs_dir + 1)
                                  (String.length path - String.length fs_dir - 1)
                              in
                              files := rel :: !files)
                          (Sys.readdir dir)
                    in
                    scan fs_dir;
                    let files = List.sort String.compare !files in
                    Fmt.pr "  files:       %d@," (List.length files);
                    if List.length files <= 20 then
                      List.iter (fun f -> Fmt.pr "    %s@," f) files
                    else begin
                      List.iteri
                        (fun i f -> if i < 10 then Fmt.pr "    %s@," f)
                        files;
                      Fmt.pr "    ... (%d more)@," (List.length files - 10)
                    end
                  end;
                  Fmt.pr "@,"
              | _ -> ())
            entries
        end;
        if not !found then Fmt.pr "  No layers found for %s@," pkg_name;
        Fmt.pr "@]@."
  in
  let target =
    Arg.(
      value
      & pos 0 (some string) None
      & info ~docv:"PKG"
          ~doc:
            "Name of a single cached package to inspect. When omitted, the \
             command prints an overview of every package in the cache."
          [])
  in
  let info =
    Cmd.info "list" ~doc:"List the pre-built packages in the local cache"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry list) reports the state of the local cache of \
             pre-built packages. With no argument it prints a summary: the \
             number of cached packages, the total disk used, and any packages \
             that failed to build and are being retained for inspection. Use \
             it as a quick sanity check before a big build or publication.";
          `P
            "Pass a $(b,PKG) name to drill into a specific entry. The output \
             then lists every cached version of that package, the build hash \
             for each, the direct dependencies that were compiled into it, and \
             the files it installed into its prefix.";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ cache_dir_term $ data_dir_term $ target)

(* -- registry index ------------------------------------------------------ *)

let registry_index_cmd =
  let run () cache_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, _platform, _os_key, _cache =
      bootstrap env cache_dir
    in
    let layers_root = cache_dir / "layers" in
    let total_layers = ref 0 in
    let total_bins = ref 0 in
    let total_files = ref 0 in
    if Sys.file_exists layers_root then
      Array.iter
        (fun entry ->
          let dir = layers_root / entry in
          if Sys.is_directory dir && entry.[0] <> '.' then begin
            let index_path = dir / "index.db" in
            let db = D10.Index.open_ ~path:index_path in
            D10.Index.rebuild
              {
                D10.Config.sys;
                fs;
                clock :> D10.Config.clk;
                root = Eio.Path.(fs / cache_dir);
                os_key = entry;
              }
              db;
            let nl, nb, nf = D10.Index.stats db ~os_key:entry in
            D10.Index.close db;
            Fmt.pr "  %s: %d layers, %d binaries, %d files@." entry nl nb nf;
            total_layers := !total_layers + nl;
            total_bins := !total_bins + nb;
            total_files := !total_files + nf
          end)
        (Sys.readdir layers_root);
    Fmt.pr "Total: %d layers, %d binaries, %d files@." !total_layers !total_bins
      !total_files
  in
  let info =
    Cmd.info "index" ~doc:"Rebuild the fast-lookup index over the local cache"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi) maintains a small SQLite database next to the cache that \
             maps every installed binary and cached package back to the layer \
             that provides it. The database is what makes $(b,oi search) and \
             the binary-name lookup in $(b,oi run) fast. This command rebuilds \
             the database from scratch by walking every cached package.";
          `P
            "A rebuild is not needed in normal use. Run it if $(b,oi search) \
             starts missing a result that you know is cached, or after editing \
             the cache directory by hand.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

(* -- registry ------------------------------------------------------------ *)

(* Remove a sqlite scratch file together with its WAL/SHM siblings.
   sqlite in WAL journal_mode leaves [-wal] and [-shm] files next to
   the main [.db] on close, and plain [Sys.remove] on just the [.db]
   leaves orphans behind — visible in the published sources/ tree. *)
let remove_sqlite_scratch path =
  List.iter
    (fun p -> try Sys.remove p with Sys_error _ -> ())
    [ path; path ^ "-wal"; path ^ "-shm"; path ^ "-journal" ]

(* Collapse any WAL/SHM sidecars next to [path] into the main database.
   Runs [PRAGMA journal_mode=DELETE], which checkpoints outstanding WAL
   pages into the main file and removes the [-wal]/[-shm] files. Used
   at the tail of [registry export] so the published index.db files
   are self-contained — rsync'ing the sources/ tree doesn't need to
   copy or create WAL siblings on the remote. *)
let finalize_sqlite_for_publish path =
  if Sys.file_exists path then begin
    (try
       let db = Sqlite3.db_open path in
       Fun.protect
         ~finally:(fun () -> ignore (Sqlite3.db_close db))
         (fun () -> ignore (Sqlite3.exec db "PRAGMA journal_mode=DELETE"))
     with _ -> ());
    (* sqlite's WAL→DELETE transition truncates the [-wal] but may
       leave the zero-byte [-shm] sidecar behind. At this point both
       are orphans — the main db owns no WAL state — so unlink any
       leftovers directly. *)
    List.iter
      (fun suffix -> try Sys.remove (path ^ suffix) with Sys_error _ -> ())
      [ "-wal"; "-shm"; "-journal" ]
  end

(* Fetch [registry]/<rel> to [dst] via curl. Returns true on success,
   false otherwise (404, network error, empty response). The caller
   decides how to react (typically: skip the remote merge). *)
let fetch_remote_to ~sys ~fs ~registry ~rel ~dst =
  if registry = "" then false
  else begin
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / Filename.dirname dst);
    D10.Sysops.Curl.fetch sys ~url:(url_join registry rel)
      ~dst:Eio.Path.(fs / dst)
  end

(* Body of [oi registry export]; kept as its own function so other
   callers (tests, future commands) can drive it without going
   through cmdliner. *)
let do_registry_export ~fs ~clock ~sys ~os_key ~cache ~registry ~output =
  let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
  let dst = Eio.Path.(fs / output) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let count = D10.Layer.export_all d10 ~dst in
  Fmt.pr "Exported %d layer(s) to %s@." count output;
  (* Rebuild the index.db only for this container's os_key. Sibling os_key
     subdirs may exist alongside ours when [dst] is a shared volume (e.g.
     docker-compose bind mount) — leave their indices alone. *)
  if Sys.file_exists (output / os_key) then begin
    let index_path = output / os_key / "index.db" in
    (try Sys.remove index_path with Sys_error _ -> ());
    let db = D10.Index.open_ ~path:index_path in
    D10.Index.rebuild d10 db;
    (* If a remote registry is configured, fetch its current
       <os_key>/index.db into a scratch file and merge those rows
       in. This keeps rows for layers that live on the remote but
       haven't been rebuilt locally this run — important for rsync:
       without it, the published index would shrink to just what the
       caller happens to have cached. *)
    if registry <> "" then begin
      let scratch = output / os_key / ".remote-index.db" in
      if
        fetch_remote_to ~sys ~fs ~registry ~rel:(os_key / "index.db")
          ~dst:scratch
      then begin
        (try D10.Index.merge_remote db ~remote_path:scratch
         with Failure msg ->
           Logs.warn (fun m -> m "Failed to merge remote layer index: %s" msg));
        remove_sqlite_scratch scratch
      end
      else
        Logs.info (fun m ->
            m "No remote layer index at %s/%s/index.db (skipping merge)"
              registry os_key)
    end;
    let nl, nb, _ = D10.Index.stats db ~os_key in
    D10.Index.close db;
    finalize_sqlite_for_publish index_path;
    Fmt.pr "  %s: %d layers, %d binaries@." os_key nl nb
  end;
  (* Sources are OS-independent — publish them once at the registry
     top level (sources/), not per os_key. A sibling [oi registry
     export] from a different arch/distro will merge into the same
     tree: blobs are content-addressed so collisions are correctness-
     preserving. *)
  let n_sources = Oi.Source.Mirror.export ~cache ~dst in
  if registry <> "" then begin
    let scratch = output / "sources" / ".remote-index.db" in
    if fetch_remote_to ~sys ~fs ~registry ~rel:"sources/index.db" ~dst:scratch
    then begin
      let index_path = output / "sources" / "index.db" in
      (try Oi.Source.Mirror.merge_remote ~fs ~index_path ~remote_path:scratch
       with Failure msg ->
         Logs.warn (fun m -> m "Failed to merge remote sources index: %s" msg));
      remove_sqlite_scratch scratch
    end
    else
      Logs.info (fun m ->
          m "No remote sources index at %s/sources/index.db (skipping merge)"
            registry)
  end;
  finalize_sqlite_for_publish (output / "sources" / "index.db");
  if n_sources > 0 then
    Fmt.pr "  sources: %d blob(s) at %s/sources/@." n_sources output

let registry_export_cmd =
  let run () cache_dir registry output =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, _platform, os_key, cache =
      bootstrap env cache_dir
    in
    do_registry_export ~fs ~clock ~sys ~os_key ~cache ~registry ~output
  in
  let output =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"DIR"
          ~doc:
            "The directory the published registry will be written into. \
             Created if it does not exist."
          [])
  in
  let info =
    Cmd.info "export"
      ~doc:"Publish the local cache to a directory for HTTP serving or rsync"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry export) copies every package in the local cache \
             into $(b,DIR) in the on-disk layout that an $(b,oi) client \
             expects from a remote registry. The result is a tree of \
             compressed layer archives along with a sqlite $(b,index.db) and a \
             sha256 $(b,OINDEX.txt) per platform. Serve the tree with any \
             static HTTP server, or $(b,rsync) it to another machine. No \
             $(b,oi) code runs on the server.";
          `P
            "Source tarballs are written once at the top of $(b,DIR) under \
             $(b,sources/), rather than once per platform, because the source \
             for a package is the same regardless of which distribution will \
             compile it.";
          `P
            "The index records the overlay handle and version that produced \
             each layer, so clients that want to scope to a specific overlay \
             can query the index directly. There is no separate per-overlay \
             tree to fetch. Use $(b,oi search) against the published registry \
             to see what overlays it covers.";
          `P
            "When $(b,--registry URL) is given, $(b,oi) downloads the existing \
             registry's index first and merges its rows into the one being \
             published. This is the safe way to $(b,rsync) back to a shared \
             registry: without the merge, your export would overwrite entries \
             contributed by other machines.";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ cache_dir_term $ registry_term $ output)

(* Render the per-target summary emitted at the end of [oi registry build].
   One row per target, in the order the user asked for them. Columns are
   truncated/padded so the table stays readable even with long overlay
   package names. Group-level counts are repeated across all targets that
   shared a group — two targets under the same solver solution get the same
   "packages / built / cached" figures, which matches how the build itself
   treated them. *)
let print_build_summary ~targets ~target_handle ~solve_failures ~target_group
    ~group_results =
  let module R = struct
    type t =
      | Skipped of string * string (* solver failure + log path *)
      | Ok of int * int * int
      | Failed of int * int * int * string * (string * string) list
      | Depext_fail of
          int
          * int
          * int
          * OpamSysPkg.Set.t
          * (string * OpamSysPkg.Set.t) list
          * string
  end in
  let result_for t =
    match Hashtbl.find_opt solve_failures t with
    | Some (msg, log_path) -> R.Skipped (msg, log_path)
    | None -> (
        match Hashtbl.find_opt target_group t with
        | None -> R.Skipped ("unknown", "")
        | Some gi -> (
            match Hashtbl.find_opt group_results gi with
            | None -> R.Skipped ("group not built", "")
            | Some (`Ok (p, b, c)) -> R.Ok (p, b, c)
            | Some (`Fail (p, b, c, msg, failures)) ->
                R.Failed (p, b, c, msg, failures)
            | Some (`Depext_fail (p, b, c, missing, per_pkg, log)) ->
                R.Depext_fail (p, b, c, missing, per_pkg, log)))
  in
  let handle_for t =
    match Hashtbl.find_opt target_handle t with Some h -> "@" ^ h | None -> ""
  in
  let rows = List.map (fun t -> (t, handle_for t, result_for t)) targets in
  let n_ok, n_failed, n_depext, n_skipped =
    List.fold_left
      (fun (o, f, d, s) (_, _, r) ->
        match r with
        | R.Ok _ -> (o + 1, f, d, s)
        | R.Failed _ -> (o, f + 1, d, s)
        | R.Depext_fail _ -> (o, f, d + 1, s)
        | R.Skipped _ -> (o, f, d, s + 1))
      (0, 0, 0, 0) rows
  in
  let status_col r =
    match r with
    | R.Ok _ -> Fmt.str "%a" Fmt.(styled `Green string) "ok"
    | R.Failed _ -> Fmt.str "%a" Fmt.(styled `Red string) "fail"
    | R.Depext_fail _ -> Fmt.str "%a" Fmt.(styled `Yellow string) "depext-fail"
    | R.Skipped _ -> Fmt.str "%a" Fmt.(styled `Yellow string) "skip"
  in
  (* Shorten multi-line solver diagnostics to the first line so the
     table stays readable. Full detail is still logged per-target
     below. *)
  let first_line s =
    match String.split_on_char '\n' s with [] -> "" | h :: _ -> h
  in
  let detail_col r =
    match r with
    | R.Ok (p, b, c) -> Fmt.str "%d pkg (%d built, %d cached)" p b c
    | R.Failed (p, b, c, _, _) ->
        Fmt.str "%d pkg (%d built, %d cached), build failed" p b c
    | R.Depext_fail (p, b, c, missing, _, _) ->
        Fmt.str "%d pkg (%d cached, %d need build), missing system pkgs: %s" p c
          b
          (missing |> OpamSysPkg.Set.elements
          |> List.map OpamSysPkg.to_string
          |> String.concat " ")
    | R.Skipped (msg, _) -> Fmt.str "skipped (%s)" (first_line msg)
  in
  let target_width =
    List.fold_left (fun w (t, _, _) -> max w (String.length t)) 12 rows
  in
  let handle_width =
    List.fold_left (fun w (_, h, _) -> max w (String.length h)) 0 rows
  in
  let styled_handle h =
    if h = "" then String.make handle_width ' '
    else
      (* [Fmt.str] with styling inflates the visible length with ANSI
         codes; pad first, then colour. *)
      let padded = Fmt.str "%-*s" handle_width h in
      Fmt.str "%a" Fmt.(styled `Cyan string) padded
  in
  Fmt.pr "@.";
  List.iter
    (fun (target, handle, r) ->
      if handle_width = 0 then
        Fmt.pr "  %-6s %-*s  %s@." (status_col r) target_width target
          (detail_col r)
      else
        Fmt.pr "  %-6s %s  %-*s  %s@." (status_col r) (styled_handle handle)
          target_width target (detail_col r);
      match r with
      | R.Failed (_, _, _, _, failures) ->
          List.iter
            (fun (pkg, log_path) ->
              Fmt.pr "         %a %s: %s@."
                Fmt.(styled `Faint string)
                "↳ log" pkg log_path)
            failures
      | R.Depext_fail (_, _, _, _, per_pkg, log_path) ->
          List.iter
            (fun (pkg, set) ->
              Fmt.pr "         %a %s: %s@."
                Fmt.(styled `Faint string)
                "↳ needs" pkg
                (set |> OpamSysPkg.Set.elements
                |> List.map OpamSysPkg.to_string
                |> String.concat " "))
            per_pkg;
          Fmt.pr "         %a %s@."
            Fmt.(styled `Faint string)
            "↳ depext log:" log_path
      | R.Skipped (_, log_path) when log_path <> "" ->
          Fmt.pr "         %a %s@."
            Fmt.(styled `Faint string)
            "↳ solver log:" log_path
      | _ -> ())
    rows;
  Fmt.pr "@.%d ok, %d failed, %d depext-fail, %d skipped@." n_ok n_failed
    n_depext n_skipped;
  (* Dump per-target build-failure output at debug level so `-v` still
     shows the reason, without dumping a compiler transcript by
     default. *)
  List.iter
    (fun (target, _handle, r) ->
      match r with
      | R.Failed (_, _, _, msg, _) -> Log.info (fun m -> m "%s: %s" target msg)
      | R.Skipped (msg, _) when String.contains msg '\n' ->
          Log.info (fun m -> m "%s: %s" target msg)
      | _ -> ())
    rows

let registry_build_cmd =
  let run () data_dir cache_dir refresh dry_run all only skip registry
      with_repos with_deps jobs toolchain_override targets =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    (* Timestamp for filtering stale log files out of the end-of-run
       "transient fetch errors" listing. Any [fetch-*.log] with mtime
       older than this was left over by a previous invocation. *)
    let run_start_time = Unix.time () in
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf = Oi.Pipeline.make_conf ~platform ~ocaml_version in
    let remote = remote_of_registry registry in
    (* When [--all] is set, walk every overlay in the reporepo and
       derive targets from each one:
       - skip [default] (ocaml/opam-repository) — its ~10k packages
         are never what [--all] should mean;
       - skip toolchain-definition entries ([x-oi-toolchain-name]
         set, url-less) — they're metadata views over other overlays,
         not buildable themselves;
       - if the overlay has [x-root-packages], emit one [@handle/pkg]
         per entry;
       - otherwise fall back to [@handle], which expands to every
         package the overlay's clone ships.
       [--only] restricts to named handles; [--skip] excludes them.
       [default] can still be included by explicitly listing it via
       [--only default]. *)
    let reporepo_target_groups =
      if not all then []
      else begin
        let path = reporepo_path () in
        Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh ~path
          ~url:(reporepo_url ());
        let entries = Oi.Source.Reporepo.load ~path in
        let only_set =
          if only = [] then None else Some (List.sort_uniq compare only)
        in
        let skip_set = List.sort_uniq compare skip in
        let handles =
          List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle) entries
          |> List.sort_uniq String.compare
        in
        List.concat_map
          (fun h ->
            let default_skipped =
              h = "default"
              &&
              match only_set with
              | None -> true
              | Some s -> not (List.mem h s)
            in
            if default_skipped then begin
              Log.info (fun m ->
                  m "--all: skipping %s (pass --only default to include)" h);
              []
            end
            else
              let included =
                (match only_set with None -> true | Some s -> List.mem h s)
                && not (List.mem h skip_set)
              in
              if not included then []
              else
                match Oi.Source.Reporepo.latest entries ~handle:h with
                | None -> []
                | Some e when e.toolchain_name <> None ->
                    Log.info (fun m ->
                        m "--all: skipping toolchain definition %s" h);
                    []
                | Some e ->
                    if e.root_packages = [] then begin
                      Log.info (fun m ->
                          m
                            "--all: overlay %s has no x-root-packages, \
                             expanding to every package in the overlay"
                            h);
                      [ [ "@" ^ h ] ]
                    end
                    else
                      List.map
                        (fun group ->
                          List.map (fun p -> "@" ^ h ^ "/" ^ p) group)
                        e.root_packages)
          handles
      end
    in
    (* Each CLI-supplied target is its own (singleton) solve group,
       preserving the previous behaviour where [oi registry build a b]
       solved [a] and [b] independently. Reporepo groups may be
       multi-element (compiler variants etc.). The tokens are still
       raw — [@handle]-only entries haven't been fanned out to the
       overlay's packages yet (we need the clone first). *)
    let token_groups =
      List.map (fun t -> [ t ]) targets @ reporepo_target_groups
    in
    let tokens = List.concat token_groups in
    if tokens = [] then
      begin if all then
        Oi.Error.config_error
          "--all expanded to nothing in %s (all overlays filtered by \
           --skip/--only, or the reporepo only contains 'default')"
          (reporepo_path ())
      else
        Oi.Error.config_error
          "no targets to build (pass PKG arguments or --all)"
      end;
    (* Classify each input into a plain target or an overlay form.
       Overlay forms collect handles to thread through [with_repos]
       so the later [cli_extra_repos] run clones them up front. The
       "build everything in this overlay" form is expanded once the
       clones exist. *)
    let parsed = List.map parse_build_target tokens in
    let with_repos =
      let handles =
        List.filter_map
          (function
            | Plain_target _ -> None
            | Overlay_pkg (h, _) | Overlay_all h -> Some h)
          parsed
        |> List.sort_uniq String.compare
      in
      with_repos @ handles
    in
    let extra_cli, url_project =
      Oi.Pipeline.materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    (* Split handles into two scopes:
       - [global_handles] apply to every solve (explicit [--with-repo]
         + any URL-project [x-repos] @-handles).
       - [token_handles] come from [@h/pkg] tokens and only apply to
         their group's solve.
       [with_repos] at this point already contains both, so recover
       [global_handles] by subtracting the token-derived set. *)
    let token_handles =
      List.filter_map
        (function
          | Overlay_pkg (h, _) | Overlay_all h -> Some h
          | Plain_target _ -> None)
        parsed
    in
    let global_handles =
      let tokens = List.sort_uniq String.compare token_handles in
      List.filter (fun h -> not (List.mem h tokens)) with_repos
      @ url_project.overlays
    in
    (* Clone every relevant overlay (global + token) upfront so
       per-group resolution below just reads already-materialised
       packages dirs. We don't keep the merged paths list — packages
       dirs are recomputed per-group from the handle subset. *)
    let all_handles =
      List.sort_uniq String.compare (global_handles @ token_handles)
    in
    let cli_extras_records =
      merge_extras
        ~cli:(cli_extra_repos ~fs ~sys all_handles)
        ~project:url_project.extra_repos
    in
    let _ : string list =
      Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh cli_extras_records
    in
    (* URL-project pins materialize into a synthetic packages/ tree
       the solver consumes ahead of everything else, so the URL's
       dev-version of each local package wins over any stable version
       from the opam-repository. *)
    let pin_dir =
      Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh url_project.pins
    in
    (* Expand [@handle] into every package the overlay's clone
       provides. List just the top-level names under the overlay's
       [packages/] dir — the solver will pick specific versions. If
       the overlay was force-bumped to a new version since the last
       [ensure_extra] call (e.g. the user just ran [oi repo add
       --force] pointing at a new URL), clone it on the fly rather
       than fail out. *)
    let overlay_packages handle =
      let entries = Oi.Source.Reporepo.load ~path:(reporepo_path ()) in
      match Oi.Source.Reporepo.latest entries ~handle with
      | None -> Oi.Error.config_error "no overlay %s in reporepo" handle
      | Some e ->
          let name = "overlay-" ^ e.handle ^ "-" ^ e.version in
          let clone_dir = data_dir / "repos" / name in
          let pkgs_dir = clone_dir / "packages" in
          if not (Sys.file_exists pkgs_dir) then begin
            let url = if e.commit = "" then e.url else e.url ^ "#" ^ e.commit in
            Log.info (fun m -> m "On-demand clone of %s from %s" name url);
            try
              Oi.Source.Repo.ensure_extra ~fs ~data_dir ~refresh
                [ { Oi.Project.name; url } ]
              |> ignore
            with exn ->
              Oi.Error.config_error
                "overlay %s@.%s failed to clone from %s:@.  %s" handle e.version
                url (Printexc.to_string exn)
          end;
          if not (Sys.file_exists pkgs_dir) then
            Oi.Error.config_error
              "overlay %s clone at %s has no packages/ tree (the upstream repo \
               at %s may not be an opam-repository layout)"
              handle clone_dir e.url;
          Sys.readdir pkgs_dir |> Array.to_list
          |> List.filter (fun n -> Sys.is_directory (pkgs_dir / n))
          |> List.sort String.compare
    in
    (* Remember which handle each target came from so the summary
       table can render a column for it. Targets from plain PKG
       arguments have no handle. If two overlays contribute the same
       bare package name the later one wins for display purposes; the
       solver still sees them as a single target. *)
    let target_handle : (string, string) Hashtbl.t = Hashtbl.create 16 in
    (* Expand each raw group into package-name groups. A group
       containing just an [@handle] fallback (no [x-root-packages])
       fans out into one singleton group per package the overlay
       ships — "build everything in the overlay" isn't a single-solve
       concept. Groups composed of [@handle/pkg] or plain [pkg] tokens
       keep their shape so multi-package solve groups (compiler
       variants) survive intact.

       Each result carries its own [handles] — the set of overlay
       handles that should be visible to the solver for this group.
       [@avsm/karakeep] yields a group with handles [avsm], nothing
       else. That scope is what keeps [@avsm] solves from picking up
       conflicting packages out of [@samoht]'s overlay. *)
    let raw_target_groups :
        (string list (* targets *) * string list (* handles *)) list =
      List.concat_map
        (fun raw_group ->
          match List.map parse_build_target raw_group with
          | [ Overlay_all h ] ->
              let ps = overlay_packages h in
              List.iter (fun p -> Hashtbl.replace target_handle p h) ps;
              Log.info (fun m ->
                  m "Overlay %s: %d package(s) to build" h (List.length ps));
              List.map (fun p -> ([ p ], [ h ])) ps
          | classified ->
              let names =
                List.map
                  (function
                    | Plain_target t -> t
                    | Overlay_pkg (h, pkg_spec) ->
                        Hashtbl.replace target_handle pkg_spec h;
                        pkg_spec
                    | Overlay_all h ->
                        Oi.Error.config_error
                          "@%s cannot appear inside a multi-package solve \
                           group; use @%s/PKG or list packages explicitly"
                          h h)
                  classified
              in
              let handles =
                List.filter_map
                  (function
                    | Plain_target _ -> None
                    | Overlay_pkg (h, _) | Overlay_all h -> Some h)
                  classified
                |> List.sort_uniq String.compare
              in
              [ (names, handles) ])
        token_groups
    in
    let target_groups = raw_target_groups in
    let targets = List.concat_map fst target_groups in
    (* [--dry-run --all] prints the expanded target list (with handles
       and the latest version each overlay ships) and stops before
       solving. Useful to audit what [--all] would attempt without
       paying the solver's cost. Non-[--all] dry-runs keep the existing
       per-group build-plan tree output downstream. *)
    if dry_run && all then begin
      let entries = Oi.Source.Reporepo.load ~path:(reporepo_path ()) in
      let n = List.length target_groups in
      let n_targets = List.length targets in
      (* Display-only grouping: per-target solves are kept for speed
         and failure isolation (opam-0install scales badly with root
         count), but we bucket the dry-run output by handle signature
         so the reader sees one row per reporepo overlay set, not one
         per root package. *)
      let display_groups =
        let tbl : (string list, string list ref) Hashtbl.t = Hashtbl.create 8 in
        let order = ref [] in
        List.iter
          (fun (targets, handles) ->
            let key = List.sort_uniq String.compare handles in
            match Hashtbl.find_opt tbl key with
            | Some acc -> acc := !acc @ targets
            | None ->
                Hashtbl.add tbl key (ref targets);
                order := key :: !order)
          target_groups;
        List.rev_map
          (fun key ->
            let ts = !(Hashtbl.find tbl key) |> List.sort_uniq String.compare in
            (ts, key))
          !order
      in
      let n_display = List.length display_groups in
      Fmt.pr "@.%a@."
        Fmt.(styled `Bold string)
        (Fmt.str
           "--all would build %d target%s in %d solve group%s (grouped into %d \
            handle scope%s):"
           n_targets
           (if n_targets = 1 then "" else "s")
           n
           (if n = 1 then "" else "s")
           n_display
           (if n_display = 1 then "" else "s"));
      let handle_dir = Hashtbl.create 8 in
      let dir_for_handle h =
        match Hashtbl.find_opt handle_dir h with
        | Some v -> v
        | None ->
            let v =
              match Oi.Source.Reporepo.latest entries ~handle:h with
              | None -> None
              | Some e ->
                  let d =
                    data_dir / "repos"
                    / ("overlay-" ^ e.handle ^ "-" ^ e.version)
                    / "packages"
                  in
                  if Sys.file_exists d then Some d else None
            in
            Hashtbl.replace handle_dir h v;
            v
      in
      let bare_name t =
        let stop = [ '='; '<'; '>'; '.'; '{' ] in
        let len = String.length t in
        let rec find i =
          if i >= len then len
          else if List.mem t.[i] stop then i
          else find (i + 1)
        in
        String.sub t 0 (find 0)
      in
      let version_for target handles =
        List.find_map
          (fun h ->
            match dir_for_handle h with
            | Some d -> latest_version_in_dirs ~pkg:(bare_name target) [ d ]
            | None -> None)
          handles
      in
      let overlays_summary_for handles =
        let eff =
          List.sort_uniq compare ("relocatable" :: (global_handles @ handles))
        in
        let resolved =
          let roots =
            List.map
              (fun h : Oi.Source.Reporepo.root ->
                { handle = h; version = None })
              eff
          in
          try Oi.Source.Reporepo.resolve entries ~roots
          with Oi.Error.E _ -> []
        in
        String.concat ", "
          (List.map
             (fun (e : Oi.Source.Reporepo.entry) ->
               "@" ^ e.handle ^ "." ^ e.version)
             resolved)
      in
      let group_label handles =
        match handles with
        | [] -> "(no overlay)"
        | hs -> String.concat "+" (List.map (fun h -> "@" ^ h) hs)
      in
      let sort_key (_, handles) = group_label handles in
      let sorted_groups =
        List.sort
          (fun a b -> String.compare (sort_key a) (sort_key b))
          display_groups
      in
      let label_w =
        List.fold_left
          (fun w (_, handles) -> max w (String.length (group_label handles)))
          0 sorted_groups
      in
      List.iter
        (fun (targets, handles) ->
          let label = group_label handles in
          Fmt.pr "@.  %a %-*s  %a@."
            Fmt.(styled `Cyan string)
            "▸" label_w label
            Fmt.(styled `Faint string)
            (overlays_summary_for handles);
          let sorted_targets = List.sort String.compare targets in
          let with_versions =
            List.map
              (fun t ->
                match version_for t handles with
                | Some v -> t ^ "." ^ v
                | None -> t)
              sorted_targets
          in
          Fmt.pr "      %s@." (String.concat ", " with_versions))
        sorted_groups;
      Fmt.pr "@.";
      exit 0
    end;
    if targets = [] && url_project.roots = [] then
      Oi.Error.config_error "no targets to build";
    let cache_root = Oi.Cache.root_s cache in
    let build_prefix = cache_root / "build" / "prefix" in
    let d10 = Oi.Pipeline.make_d10 ~sys ~fs ~clock ~cache ~os_key in
    let base_packages_dirs =
      Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ()
    in
    (* Per-group packages_dirs: each solve group sees ONLY the overlay
       handles it actually uses (plus its transitive base deps via
       reporepo [depends:] resolution, plus any globally-scoped handles
       from [--with-repo] / URL-project [x-repos] @-handles). This is what
       keeps [@avsm/karakeep] from accidentally picking up packages
       out of [@samoht]'s overlay — the samoht clone is on disk but
       isn't in this group's solver search path. *)
    let reporepo_entries_cache =
      try Oi.Source.Reporepo.load ~path:(reporepo_path ()) with _ -> []
    in
    (* Toolchain resolution per group: the [--toolchain=NAME] flag, if
       given, wins. Otherwise inspect the [x-oi-toolchain] field on
       the latest entry of each token handle (i.e. handles that come
       from [@h/pkg] tokens, not global [--with-repo] handles); if a
       single toolchain is declared, use it. Multiple conflicting
       declarations short-circuit with a clear error. The result is
       memoised so that 30 [@avsm/...] groups share one
       [Toolchain.resolve] / [ensure_installed] pass. *)
    let resolved_toolchains : (string, Oi.Toolchain.info) Hashtbl.t =
      Hashtbl.create 4
    in
    let resolve_toolchain handle =
      match Hashtbl.find_opt resolved_toolchains handle with
      | Some i -> i
      | None ->
          let info = Oi.Toolchain.resolve ~fs ~sys ~data_dir ~conf ~handle in
          Oi.Toolchain.ensure_installed ~fs info;
          Hashtbl.add resolved_toolchains handle info;
          info
    in
    let toolchain_for_handles handles =
      match toolchain_override with
      | Some h -> Some (resolve_toolchain h)
      | None -> (
          let names =
            List.filter_map
              (fun h ->
                match
                  Oi.Source.Reporepo.latest reporepo_entries_cache ~handle:h
                with
                | Some (e : Oi.Source.Reporepo.entry) -> e.toolchain
                | None -> None)
              handles
            |> List.sort_uniq String.compare
          in
          match names with
          | [] -> None
          | [ n ] -> Some (resolve_toolchain n)
          | many ->
              Oi.Error.config_error
                "overlays in scope declare conflicting x-oi-toolchain values: \
                 %s — pass --toolchain=NAME to disambiguate"
                (String.concat ", " many))
    in
    let overlay_entries_for_handles handles =
      let roots =
        List.rev handles
        |> List.map (fun h : Oi.Source.Reporepo.root ->
            { handle = h; version = None })
      in
      try
        Oi.Source.Reporepo.resolve reporepo_entries_cache ~roots
        (* [resolve] returns deps-first; reverse so dependents win on
           name collisions under the solver's first-wins fold. *)
        |> List.rev
      with Oi.Error.E _ -> []
    in
    let packages_dirs_for_handles ?toolchain handles =
      (* Drop globally-scoped overlays whose [x-oi-toolchain] is
         incompatible with this group's toolchain. Per-group token
         handles are kept verbatim — the user named those explicitly
         via [@h/pkg] and expects them in scope regardless. *)
      let global_handles =
        Oi.Pipeline.filter_compatible_overlays ~reporepo_path:(reporepo_path ())
          ~toolchain global_handles
      in
      let effective =
        global_handles @ handles |> List.sort_uniq String.compare
      in
      let overlay_entries = overlay_entries_for_handles effective in
      let overlay_dirs =
        List.map
          (fun (e : Oi.Source.Reporepo.entry) ->
            let name = "overlay-" ^ e.handle ^ "-" ^ e.version in
            Oi.Source.Repo.repo_dir ~data_dir name / "packages")
          overlay_entries
      in
      (* When a toolchain is active, [info.packages_dirs] (the
         toolchain's own clone plus its declared base overlays) takes
         the place of [base_packages_dirs] — same logic as [oi run
         --toolchain]. The toolchain's chosen base set is what should
         be in scope, not the reporepo's default
         [relocatable]/[default] pair (e.g. an [oxcaml] solve must not
         see [relocatable]). *)
      let base =
        match (toolchain : Oi.Toolchain.info option) with
        | None -> base_packages_dirs
        | Some i -> i.packages_dirs
      in
      let seen = Hashtbl.create 8 in
      let dedup xs =
        List.filter
          (fun d ->
            if Hashtbl.mem seen d then false
            else begin
              Hashtbl.replace seen d ();
              true
            end)
          xs
      in
      dedup (Stdlib.Option.to_list pin_dir @ overlay_dirs @ base)
    in
    (* [--with] adds extra packages to every target's root set plus any
       version constraints they carry. *)
    let base_constraints = Oi.Project.Script.constraints extra_cli in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Project.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_cli
      @ List.map OpamPackage.Name.of_string url_project.roots
    in
    let target_groups =
      target_groups @ List.map (fun r -> ([ r ], [])) url_project.roots
    in
    let targets = List.concat_map fst target_groups in
    (* Per-target result tracking; the final summary walks [targets] in
       order and looks each name up here. A target either fails to
       solve (status stored directly), or lands in some group. Groups
       are keyed by index; their build result (ok / failed, with the
       package counts) is written into [group_results] when the group
       finishes. *)
    let solve_failures : (string, string * string) Hashtbl.t =
      Hashtbl.create 16
    in
    (* Dump the solver's diagnostic so the user can grep for conflicting
       version constraints. The file name's short hash comes from the
       (targets, handles) tuple so two solve groups whose first target
       name collides don't clobber each other's logs. *)
    let write_solve_failure_log ~targets ~handles ~msg =
      let key =
        String.concat " " (targets @ List.map (fun h -> "@" ^ h) handles)
      in
      let hash = Digest.to_hex (Digest.string key) in
      let first = match targets with t :: _ -> t | [] -> "solve" in
      let path =
        Oi.Cache.Logs.path ~cache_root ~kind:"solve" ~name:first ~hash
      in
      let handles_str =
        if handles = [] then "(base only)"
        else String.concat ", " (List.map (fun h -> "@" ^ h) handles)
      in
      let trailing_nl =
        if msg = "" || msg.[String.length msg - 1] = '\n' then "" else "\n"
      in
      let body =
        Fmt.str "targets: %s\nhandles: %s\n\n%s%s"
          (String.concat ", " targets)
          handles_str msg trailing_nl
      in
      Oi.Cache.Logs.write ~fs ~cache_root path body;
      path
    in
    let target_group : (string, int) Hashtbl.t = Hashtbl.create 16 in
    let group_results :
        ( int,
          [ `Ok of int * int * int
          | `Fail of int * int * int * string * (string * string) list
          | `Depext_fail of
            int
            * int
            * int
            * OpamSysPkg.Set.t
            * (string * OpamSysPkg.Set.t) list
            * string ] )
        Hashtbl.t =
      Hashtbl.create 16
    in
    (* 1. Solve each solve-group against that group's scoped
       [packages_dirs] — handles from the group's own tokens plus any
       global [--with-repo] ones. Different groups see different
       overlays so [@avsm/...] never solves against [@samoht/...]. *)
    let solutions =
      let n_groups = List.length target_groups in
      let group_label group = String.concat " " group in
      let solve_one (group, handles) =
        let toolchain = toolchain_for_handles handles in
        let pkg_dirs = packages_dirs_for_handles ?toolchain handles in
        let group_conf, tc_ctx = Oi.Pipeline.toolchain_views toolchain conf in
        let gctx =
          Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkg_dirs
            ~conf:group_conf ?toolchain:tc_ctx ()
        in
        let items = List.map parse_pkg_target group in
        let names = List.map fst items in
        let constraints =
          List.fold_left
            (fun acc (name, c) ->
              match c with
              | None -> acc
              | Some c -> OpamPackage.Name.Map.add name c acc)
            base_constraints items
        in
        match
          Oi.Solver.solve ~fs ~cache_root gctx ~packages_dirs:pkg_dirs
            ~constraints (names @ extra_names)
        with
        | Ok pkgs ->
            Log.info (fun m ->
                let tc_label =
                  match toolchain with
                  | None -> ""
                  | Some i -> Fmt.str " (toolchain %s)" i.handle
                in
                m "Solved %s%s: %d packages" (group_label group) tc_label
                  (List.length pkgs));
            Some (group, handles, pkg_dirs, pkgs, toolchain, group_conf, tc_ctx)
        | Error msg ->
            let log_path =
              write_solve_failure_log ~targets:group ~handles ~msg
            in
            List.iter
              (fun t -> Hashtbl.replace solve_failures t (msg, log_path))
              group;
            Log.debug (fun m ->
                m "solve failed: %s: %s" (group_label group) msg);
            None
      in
      if n_groups <= 1 then List.filter_map solve_one target_groups
      else
        let config = Progress.Config.v ~persistent:false () in
        let bar =
          let open Progress.Line in
          pair ~sep:(const " ")
            (list [ spinner (); brackets (count_to n_groups) ])
            (rpad 40 string)
        in
        let acc = ref [] in
        Progress.with_reporter ~config bar (fun report ->
            List.iter
              (fun ((g, _) as group_and_handles) ->
                let label = group_label g in
                report (0, Fmt.str "solve %s" label);
                (match solve_one group_and_handles with
                | Some s -> acc := s :: !acc
                | None -> ());
                report (1, Fmt.str "solve %s" label))
              target_groups);
        List.rev !acc
    in
    if solutions = [] then Oi.Error.msg "no packages solved successfully";
    (* 2. Each solve group is its own build group (no cross-group
       merging). Dedup-by-layer-hash already happens inside the layer
       cache; merging here would only gain shared plan construction
       at the cost of failure cascades. *)
    let n_groups = List.length solutions in
    Log.info (fun m -> m "%d build group(s)" n_groups);
    (* Shared failure tracker across every build group: if [foo.1.2]
       fails in group 1, any later group that depends on the same
       layer hash skips it and marks its own dependents as failed
       rather than retrying the doomed build. Keyed by layer_hash so
       the same [name.version] resolved to a different layer (e.g.
       via a different dep set in another overlay) still gets its own
       attempt. *)
    let failed_layers : (string, string) Hashtbl.t = Hashtbl.create 64 in
    (* 3. Build each group, threading a single cross-group progress
       bar and accounting. The bar shows total packages processed
       (across every build group), the group counter, and live
       counts of each outcome so the user can see at a glance how
       many are built vs. cached vs. failing to build vs. skipped
       because a dep failed. *)
    let total_pkgs_estimate =
      List.fold_left
        (fun acc (_, _, _, pkgs, _, _, _) -> acc + List.length pkgs)
        0 solutions
    in
    let counters =
      object
        val mutable ok = 0
        val mutable cached = 0
        val mutable build_failed = 0
        val mutable dep_failed = 0
        val mutable cur_group = 0
        val mutable cur_pkg = ""
        method ok = ok
        method cached = cached
        method build_failed = build_failed
        method dep_failed = dep_failed
        method set_group g = cur_group <- g
        method set_pkg p = cur_pkg <- p
        method incr_ok = ok <- ok + 1
        method incr_cached = cached <- cached + 1
        method incr_build_failed = build_failed <- build_failed + 1
        method incr_dep_failed = dep_failed <- dep_failed + 1

        method status =
          Fmt.str "groups %d/%d  ok:%d cached:%d fail:%d dep:%d  %s" cur_group
            n_groups ok cached build_failed dep_failed cur_pkg
      end
    in
    let make_reporter report =
      Oi.Execute.
        {
          pkg_event =
            (fun e ->
              (match e with
              | Started { pkg; _ } -> counters#set_pkg pkg
              | Cached _ -> counters#incr_cached
              | Built _ -> counters#incr_ok
              | Build_failed { pkg; log } ->
                  counters#incr_build_failed;
                  Progress.interject_with (fun () ->
                      Fmt.epr "  %a %s → %s@."
                        Fmt.(styled (`Fg `Red) string)
                        "FAIL" pkg log)
              | Install_failed { pkg; log } ->
                  counters#incr_build_failed;
                  Progress.interject_with (fun () ->
                      Fmt.epr "  %a %s (install) → %s@."
                        Fmt.(styled (`Fg `Red) string)
                        "FAIL" pkg log)
              | Dep_failed _ -> counters#incr_dep_failed);
              let delta =
                match e with
                | Started _ -> 0
                | Cached _ | Built _ | Build_failed _ | Install_failed _
                | Dep_failed _ ->
                    1
              in
              report (delta, counters#status));
        }
    in
    let progress_config = Progress.Config.v ~persistent:false () in
    let progress_bar =
      let open Progress.Line in
      pair ~sep:(const " ")
        (list [ spinner (); brackets (count_to total_pkgs_estimate) ])
        (rpad 80 string)
    in
    let in_progress_reporter f =
      if dry_run then f None
      else
        Progress.with_reporter ~config:progress_config progress_bar (fun rep ->
            f (Some (make_reporter rep)))
    in
    in_progress_reporter @@ fun reporter ->
    List.iteri
      (fun gi
           ( group_targets_list,
             _handles,
             pkg_dirs,
             solution_pkgs,
             _toolchain,
             group_conf,
             tc_ctx ) ->
        counters#set_group (gi + 1);
        let group_targets = String.concat ", " group_targets_list in
        List.iter
          (fun t -> Hashtbl.replace target_group t gi)
          group_targets_list;
        (* Solution packages are already unique within a single solve,
           so no cross-solution dedup is needed. *)
        let merged_pkgs = solution_pkgs in
        let group_ctx =
          Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkg_dirs
            ~conf:group_conf ?toolchain:tc_ctx ()
        in
        let sorted_pkgs =
          Oi.Solver.topo_sort ~packages_dirs:pkg_dirs
            ~conf:(Oi.Solver.Ctx.conf group_ctx)
            merged_pkgs
        in
        if n_groups > 1 then
          Log.info (fun m ->
              m "Group %d/%d [%s]: %d packages" (gi + 1) n_groups group_targets
                (List.length sorted_pkgs))
        else
          Log.info (fun m -> m "%d unique packages" (List.length sorted_pkgs));
        let build_plan =
          Oi.Plan.build group_ctx ~d10 ~packages_dirs:pkg_dirs sorted_pkgs
        in
        let count_by f =
          List.length (List.filter f (Oi.Plan.nodes build_plan))
        in
        let n_build = count_by (fun (n : Oi.Plan.node) -> n.method_ = Source) in
        let n_cached =
          count_by (fun (n : Oi.Plan.node) -> n.method_ = Binary)
        in
        let n_pkgs = n_build + n_cached in
        if dry_run then begin
          let remote_has =
            match remote with
            | Some r ->
                let idx = D10.Layer.fetch_remote_index d10 ~remote:r in
                fun h -> Hashtbl.mem idx h
            | None -> fun _ -> false
          in
          Fmt.pr "%a@." (Oi.Plan.pp_tree ~remote_has) build_plan
        end
        else if n_build = 0 then begin
          (* Fast path for fully-cached groups: every layer is already
             in the d10 cache, so there's no reason to pay for
             [Plan.create] (resolves commands / install files),
             [Execute.run] (prefix wipe + layer restore), or the
             source-mirror promotion pass. Just emit Cached events for
             the reporter counters and call it done. This is the
             common case when re-running [oi registry build --all]
             against an already-populated cache. *)
          Log.info (fun m -> m "all %d packages cached" n_cached);
          (match reporter with
          | None -> ()
          | Some r ->
              List.iter
                (fun (n : Oi.Plan.node) ->
                  r.pkg_event
                    (Oi.Execute.Cached { pkg = OpamPackage.to_string n.pkg }))
                (Oi.Plan.nodes build_plan));
          Hashtbl.replace group_results gi (`Ok (n_pkgs, n_build, n_cached))
        end
        else begin
          Log.info (fun m -> m "%d to build, %d cached" n_build n_cached);
          (* Depext pre-flight: classify a group as [depext-fail] when
             its source-built packages need system packages the host is
             missing. Cheaper and clearer than letting the build start
             and fail mid-compile, and lets the user [apt install] the
             listed packages and re-run. Only runs when there is at
             least one source build in this group; pure restore groups
             never need depexts. *)
          let depext_classify () =
            let source_pkgs =
              Oi.Plan.nodes build_plan
              |> List.filter_map (fun (n : Oi.Plan.node) ->
                  match n.method_ with
                  | Oi.Plan.Source -> Some n.pkg
                  | Oi.Plan.Binary -> None)
            in
            if source_pkgs = [] then None
            else
              let entries =
                Oi.Depexts.compute group_ctx ~packages_dirs:pkg_dirs source_pkgs
              in
              let all =
                List.fold_left
                  (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
                  OpamSysPkg.Set.empty entries
              in
              if OpamSysPkg.Set.is_empty all then None
              else
                let st = Oi.Depexts.status all in
                if OpamSysPkg.Set.is_empty st.missing then None
                else
                  let per_pkg =
                    List.filter_map
                      (fun (e : Oi.Depexts.entry) ->
                        let m = OpamSysPkg.Set.inter e.sys_pkgs st.missing in
                        if OpamSysPkg.Set.is_empty m then None
                        else Some (OpamPackage.to_string e.pkg, m))
                      entries
                  in
                  Some (st.missing, per_pkg)
          in
          match depext_classify () with
          | Some (missing, per_pkg) ->
              let log_path =
                Oi.Cache.Logs.path ~cache_root ~kind:"depext"
                  ~name:(List.hd group_targets_list)
                  ~hash:(Digest.to_hex (Digest.string group_targets))
              in
              let body =
                let buf = Buffer.create 512 in
                Buffer.add_string buf "Group: ";
                Buffer.add_string buf group_targets;
                Buffer.add_string buf "\n\nMissing system packages:\n";
                OpamSysPkg.Set.iter
                  (fun p ->
                    Buffer.add_string buf "  ";
                    Buffer.add_string buf (OpamSysPkg.to_string p);
                    Buffer.add_char buf '\n')
                  missing;
                Buffer.add_string buf "\nRequired by:\n";
                List.iter
                  (fun (pkg, set) ->
                    Buffer.add_string buf "  ";
                    Buffer.add_string buf pkg;
                    Buffer.add_string buf ": ";
                    Buffer.add_string buf
                      (set |> OpamSysPkg.Set.elements
                      |> List.map OpamSysPkg.to_string
                      |> String.concat ", ");
                    Buffer.add_char buf '\n')
                  per_pkg;
                Buffer.contents buf
              in
              Oi.Cache.Logs.write ~fs ~cache_root log_path body;
              Log.info (fun m ->
                  m "depext-fail: %d missing system package(s); see %s"
                    (OpamSysPkg.Set.cardinal missing)
                    log_path);
              Hashtbl.replace group_results gi
                (`Depext_fail
                   (n_pkgs, n_build, n_cached, missing, per_pkg, log_path))
          | None ->
              let exec_plan =
                Oi.Plan.resolve group_ctx ~packages_dirs:pkg_dirs ~cache_root
                  ~os_key ~ocaml_version:conf.ocaml_version build_plan
              in
              (* After the build, any package in this group's plan whose
             layer hash is in [failed_layers] with a non-empty path
             either failed directly (its own build log) or inherited
             a log from a failed upstream dep (cascaded). Dedup by
             log path so a single upstream failure doesn't produce N
             repeated summary lines for its dependents. *)
              let collect_failures (exec_plan : Oi.Plan.t) =
                let seen = Hashtbl.create 16 in
                List.concat_map
                  (fun (g : Oi.Plan.group) ->
                    List.filter_map
                      (fun (p : Oi.Plan.package_plan) ->
                        match Hashtbl.find_opt failed_layers p.layer_hash with
                        | Some path
                          when path <> "" && not (Hashtbl.mem seen path) ->
                            Hashtbl.replace seen path ();
                            Some (p.pkg, path)
                        | _ -> None)
                      g.packages)
                  exec_plan.groups
              in
              let build_outcome :
                  [ `Ok | `Fail of string * (string * string) list ] =
                let build_plan =
                  Oi.Pipeline.fetch_remote_layers ?jobs ~remote ~d10
                    ~packages_dirs:pkg_dirs ~ctx:group_ctx ~pkgs:sorted_pkgs
                    build_plan
                in
                let exec_plan_ref = ref None in
                try
                  let exec_plan =
                    Oi.Plan.resolve group_ctx ~packages_dirs:pkg_dirs
                      ~cache_root ~os_key ~ocaml_version:conf.ocaml_version
                      build_plan
                  in
                  exec_plan_ref := Some exec_plan;
                  let cache_urls = Oi.Pipeline.cache_urls ~cache ~remote in
                  Oi.Execute.run ~cache_urls ?jobs ~failed_layers ?reporter
                    ~proc_mgr ~fs
                    ~clock:(clock :> D10.Config.clk)
                    ~sys ~os_key exec_plan;
                  `Ok
                with
                | Oi.Error.E e ->
                    let failures =
                      match !exec_plan_ref with
                      | Some p -> collect_failures p
                      | None -> []
                    in
                    `Fail (Fmt.str "%a" Oi.Error.pp e, failures)
                | Failure msg ->
                    let failures =
                      match !exec_plan_ref with
                      | Some p -> collect_failures p
                      | None -> []
                    in
                    `Fail (msg, failures)
              in
              Hashtbl.replace group_results gi
                (match build_outcome with
                | `Ok -> `Ok (n_pkgs, n_build, n_cached)
                | `Fail (msg, failures) ->
                    `Fail (n_pkgs, n_build, n_cached, msg, failures));
              (* Write-side mirror: only useful when we actually built
             something new; fully-cached groups have no fresh sources
             to promote. *)
              Oi.Pipeline.record_sources ~sys ~cache exec_plan
        end)
      solutions;
    if not dry_run then begin
      print_build_summary ~targets ~target_handle ~solve_failures ~target_group
        ~group_results;
      (* Point at any fetch-retry logs collected during the run so
         the user can investigate transient errors (git fetch
         failures, opam archive 500s) without them polluting the
         live output. *)
      let logs_dir = Oi.Cache.Logs.dir ~cache_root in
      if Sys.file_exists logs_dir then begin
        (* Only list logs that were (re-)written during THIS run. Stale
           [fetch-*.log] files left over from previous invocations
           would otherwise show up unrelated to the current build. *)
        let written_this_run p =
          try (Unix.stat p).Unix.st_mtime >= run_start_time
          with Unix.Unix_error _ -> false
        in
        let entries =
          try
            Sys.readdir logs_dir |> Array.to_list
            |> List.filter (fun n -> String.starts_with ~prefix:"fetch-" n)
            |> List.map (fun n -> logs_dir / n)
            |> List.filter written_this_run
            |> List.sort String.compare
          with Sys_error _ -> []
        in
        if entries <> [] then begin
          Fmt.pr "@.%a (%d):@."
            Fmt.(styled `Faint string)
            "transient fetch errors" (List.length entries);
          List.iter (fun p -> Fmt.pr "  %s@." p) entries
        end
      end
    end
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"PKG"
          ~doc:
            "The opam packages to build layers for. May be empty when \
             $(b,--all) is set."
          [])
  in
  let all =
    Arg.(
      value & flag
      & info
          ~doc:
            "Walk every overlay in the reporepo and build the packages each \
             one nominates. An overlay that declares $(b,x-root-packages) \
             contributes each entry as $(b,@HANDLE/PKG); an overlay without \
             that declaration contributes the $(b,@HANDLE) shortcut, which \
             asks for every package it ships. The $(b,default) overlay \
             (ocaml/opam-repository) is excluded by default because building \
             its ten thousand packages is rarely what you want. Combine with \
             $(b,--only) or $(b,--skip) to refine the handle set; pass \
             $(b,--only default) if you do want to build the whole of \
             opam-repository. Positional $(b,PKG) arguments are honoured in \
             addition to the derived list."
          [ "all" ])
  in
  let only =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:
            "Restrict $(b,--all) to the named overlay handles. May be given \
             more than once. Has no effect unless $(b,--all) is also set."
          [ "only" ])
  in
  let skip =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:
            "Exclude the named overlay handles from $(b,--all). May be given \
             more than once. Has no effect unless $(b,--all) is also set."
          [ "skip" ])
  in
  let dry_run =
    Arg.(
      value & flag
      & info
          ~doc:
            "Print the merged build plan for every target and exit. No sources \
             are fetched and no builds are run."
          [ "n"; "dry-run" ])
  in
  let info =
    Cmd.info "build"
      ~doc:"Build packages into the local cache for later publication"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve, compile, and cache every target. Use to prime a cache \
             before $(b,oi registry export). Multiple targets share solves and \
             dedup work — cheaper than a loop.";
          `P
            "Per-group toolchain auto-derives from each overlay's \
             $(b,x-oi-toolchain) tag. $(b,--toolchain=NAME) overrides for \
             every group; conflicting tags within one group error out.";
          `S "PACKAGE SPECIFICATIONS";
          `I
            ( "$(b,PKG)",
              "Plain opam package name; solver picks the best version from \
               enabled overlays." );
          `I
            ( "$(b,@HANDLE/PKG)",
              "Pin $(b,PKG) to the version overlay $(b,HANDLE) provides." );
          `I ("$(b,@HANDLE)", "Every package in overlay $(b,HANDLE).");
          `S "OPTIONS";
          `I
            ( "$(b,--all)",
              "Walk every overlay in the reporepo and build its \
               $(b,x-root-packages) as $(b,@HANDLE/PKG). Restrict with \
               $(b,--only=HANDLE) / exclude with $(b,--skip=HANDLE)." );
          `I
            ( "$(b,--with)",
              "Extra packages or URL projects as additional build targets." );
          `I
            ( "$(b,--toolchain=NAME)",
              "Force NAME for every group, ignoring per-overlay tags." );
          `I ("$(b,-j N)", "Cap parallel builds + fetches (default 4).");
          `I ("$(b,-n) / $(b,--dry-run)", "Print the plan only.");
          `S Manpage.s_examples;
          `Pre
            "  oi registry build --all\n\
            \  oi registry build --all --only=avsm\n\
            \  oi registry build --all ocaml-lsp-server";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ dry_run $ all $ only $ skip $ registry_term $ with_repos_term
      $ with_deps_term $ jobs_term $ toolchain_term $ targets)

(* -- registry docker ---------------------------------------------------- *)

(* Walk the reporepo and return the list of [(handle, root_groups)]
   pairs that should drive depext computation for [oi registry docker].
   Excludes the [default] handle (the full opam-repository), toolchain
   definitions ([x-oi-toolchain-name] set — they're metadata views,
   not buildable), and any overlay without [x-root-packages]. *)
let overlay_root_targets reporepo_entries =
  reporepo_entries
  |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
  |> List.sort_uniq String.compare
  |> List.filter_map (fun h ->
      if h = "default" then None
      else
        match Oi.Source.Reporepo.latest reporepo_entries ~handle:h with
        | Some e when e.toolchain_name = None && e.root_packages <> [] ->
            Some (h, e.root_packages)
        | _ -> None)

(* Resolve the effective [packages_dirs] for a single overlay handle:
   its own clone plus every overlay it depends on (via the reporepo's
   [depends:] entries), followed by the base opam/relocatable clones.
   Same first-wins ordering the registry build uses. *)
let packages_dirs_for_overlay ~data_dir ~base_packages_dirs ~reporepo_entries
    handle =
  let roots = [ { Oi.Source.Reporepo.handle; version = None } ] in
  let transitive =
    try Oi.Source.Reporepo.resolve reporepo_entries ~roots |> List.rev
    with Oi.Error.E _ -> []
  in
  let overlay_dirs =
    List.map
      (fun (e : Oi.Source.Reporepo.entry) ->
        let name = "overlay-" ^ e.handle ^ "-" ^ e.version in
        Oi.Source.Repo.repo_dir ~data_dir name / "packages")
      transitive
  in
  let seen = Hashtbl.create 8 in
  let dedup xs =
    List.filter
      (fun d ->
        if Hashtbl.mem seen d then false
        else begin
          Hashtbl.replace seen d ();
          true
        end)
      xs
  in
  dedup (overlay_dirs @ base_packages_dirs)

(* For each target distro, compute the union of depexts declared by
   every overlay's [x-root-packages]. Solves happen once per overlay
   root group under the host conf (all target distros are linux so
   the solve output is the same); depexts are then re-evaluated per
   distro using {!Oi.Depexts.compute_for_conf}, which only rewrites
   the filter env and does not require a fresh switch state. *)
let compute_overlay_depexts_per_distro ~fs ~sys ~cache ~data_dir ~refresh
    ~platform ~distros =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  let base_packages_dirs =
    Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ()
  in
  let path = reporepo_path () in
  Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh ~path ~url:(reporepo_url ());
  let reporepo_entries = try Oi.Source.Reporepo.load ~path with _ -> [] in
  let targets = overlay_root_targets reporepo_entries in
  let cache_root = Oi.Cache.root_s cache in
  let build_prefix = cache_root / "build" / "prefix" in
  let host_conf = Oi.Pipeline.make_conf ~platform ~ocaml_version in
  (* Solve each root group once. Failures are tolerated (overlay may
     be broken); that group simply contributes no depexts. *)
  let solves =
    List.concat_map
      (fun (handle, groups) ->
        let pkg_dirs =
          packages_dirs_for_overlay ~data_dir ~base_packages_dirs
            ~reporepo_entries handle
        in
        List.filter_map
          (fun group ->
            let ctx =
              Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkg_dirs
                ~conf:host_conf ()
            in
            let items = List.map parse_pkg_target group in
            let names = List.map fst items in
            let constraints =
              List.fold_left
                (fun acc (name, c) ->
                  match c with
                  | None -> acc
                  | Some c -> OpamPackage.Name.Map.add name c acc)
                OpamPackage.Name.Map.empty items
            in
            match
              Oi.Solver.solve ~fs ~cache_root ctx ~packages_dirs:pkg_dirs
                ~constraints names
            with
            | Ok solved -> Some (pkg_dirs, solved)
            | Error msg ->
                Logs.info (fun m ->
                    m "docker depexts: %s group failed to solve: %s" handle msg);
                None)
          groups)
      targets
  in
  List.map
    (fun distro ->
      let vars = Registry_docker.opam_vars_of_distro distro in
      let distro_conf =
        {
          host_conf with
          os = "linux";
          os_distribution = vars.os_distribution;
          os_family = vars.os_family;
          os_version = vars.os_version;
        }
      in
      let all =
        List.fold_left
          (fun acc (pkg_dirs, solved) ->
            let entries =
              Oi.Depexts.compute_for_conf ~conf:distro_conf
                ~packages_dirs:pkg_dirs solved
            in
            List.fold_left
              (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
              acc entries)
          OpamSysPkg.Set.empty solves
      in
      let names =
        OpamSysPkg.Set.elements all |> List.map OpamSysPkg.to_string
      in
      (distro, names))
    distros

let registry_docker_cmd =
  let default_distros : Registry_docker.Distro.t list =
    [
      `Alpine `Latest;
      `Debian `Stable;
      `Ubuntu `V22_04;
      `Ubuntu `V24_04;
      `Ubuntu `V25_10;
      `Fedora `Latest;
    ]
  in
  let run () data_dir cache_dir output_dir src_context refresh =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, _clock, sys, platform, _os_key, cache =
      bootstrap env cache_dir
    in
    (try Unix.mkdir output_dir 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
    let df_oi = Registry_docker.dockerfile_oi ~src_context in
    let oi_path = output_dir / "Dockerfile.oi" in
    Registry_docker.write_dockerfile oi_path df_oi;
    Fmt.pr "Computing overlay depexts for %d distros...@."
      (List.length default_distros);
    let per_distro_depexts =
      try
        compute_overlay_depexts_per_distro ~fs ~sys ~cache ~data_dir ~refresh
          ~platform ~distros:default_distros
      with _ -> List.map (fun d -> (d, [])) default_distros
    in
    let per_distro_paths =
      List.map
        (fun d ->
          let fname = Registry_docker.one_distro_filename d in
          let path = output_dir / fname in
          let overlay_depexts =
            Stdlib.Option.value
              (List.assoc_opt d per_distro_depexts)
              ~default:[]
          in
          let df = Registry_docker.dockerfile_one_distro ~overlay_depexts d in
          Registry_docker.write_dockerfile path df;
          (d, path, List.length overlay_depexts))
        default_distros
    in
    let compose_path = output_dir / "docker-compose.yml" in
    let compose_yaml =
      Registry_docker.docker_compose_yaml ~distros:default_distros
        ~registry_host_path:"./registry" ()
    in
    Registry_docker.write_file compose_path compose_yaml;
    Fmt.pr "Wrote:@.";
    Fmt.pr "  %s@." oi_path;
    List.iter
      (fun (_, path, n) ->
        if n = 0 then Fmt.pr "  %s@." path
        else Fmt.pr "  %s  (%d overlay depexts)@." path n)
      per_distro_paths;
    Fmt.pr "  %s@." compose_path;
    Fmt.pr "@.";
    Fmt.pr "Static oi release binary:@.";
    Fmt.pr "  docker buildx build -f %s --output type=local,dest=./oi-bin .@."
      oi_path;
    Fmt.pr "Run the registry build + export:@.";
    Fmt.pr "  docker compose up --build   # writes ./registry/<os_key>/@."
  in
  let output_dir =
    Arg.(
      value & opt string "."
      & info ~docv:"DIR"
          ~doc:
            "The directory to write the Dockerfiles and the \
             $(b,docker-compose.yml) into. Created if it does not already \
             exist."
          [ "o"; "output" ])
  in
  let src_context =
    Arg.(
      value & opt string "."
      & info ~docv:"DIR"
          ~doc:
            "Path to the $(b,oi) source tree, relative to the Docker build \
             context. Defaults to the context root."
          [ "src" ])
  in
  let info =
    Cmd.info "docker"
      ~doc:
        "Generate per-distro Dockerfiles and a docker-compose.yml that run oi \
         registry build + export"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry docker) writes a small project that will build \
             $(b,oi) in a static musl container and then run a $(b,registry \
             build --all) followed by a $(b,registry export) once per target \
             distribution. The generated files are $(b,Dockerfile.oi) (the \
             static $(b,oi) builder), one $(b,Dockerfile.<distro>) per target, \
             and a $(b,docker-compose.yml) that ties them together. Each \
             service bind-mounts $(b,./registry) onto $(b,/out) so the \
             resulting layer archives end up on the host.";
          `P
            "The per-distribution images are intentionally generic. They \
             install the required system packages and drop in the \
             statically-linked $(b,oi) binary. The real work is driven from \
             $(b,docker-compose.yml) via a $(b,command:) override. Every \
             container owns its own $(b,oi) state. On first use it clones the \
             reporepo from the built-in default URL (override with \
             $(b,OI_REPOREPO_URL) in the service environment), iterates each \
             overlay's $(b,x-root-packages) list, builds each one as \
             $(b,@HANDLE/PKG), and finishes with $(b,oi registry export /out). \
             The containers do not share state, so they run safely in \
             parallel.";
          `P
            "The resulting tree at $(b,./registry/<os_key>/) carries one \
             archive per layer along with a sqlite $(b,index.db) tagged with \
             each layer's overlay handle and version. Clients can scope to a \
             specific overlay by querying the index directly. Build the whole \
             project with:";
          `Pre "  docker compose up --build";
          `P
            "$(b,compose up) returns once every distribution has finished. The \
             output is ready to serve over static HTTP, or to $(b,rsync) onto \
             a registry server. No further $(b,oi) commands are needed on the \
             server.";
          `S Manpage.s_examples;
          `P "Generate the compose project in the current directory:";
          `Pre "  oi registry docker -o ./registry-build";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ output_dir
      $ src_context $ refresh_term)

(* -- registry mirror ------------------------------------------------------ *)

(* Human-readable byte size ("1.2GB", "47MB", …). Defined here rather
   than reusing Cache.pp_size because we want to print directly into a
   string for simple output, not via an Fmt formatter. *)
let human_bytes b =
  if Int64.compare b 1_000_000_000L > 0 then
    Fmt.str "%.1fGB" (Int64.to_float b /. 1e9)
  else if Int64.compare b 1_000_000L > 0 then
    Fmt.str "%.1fMB" (Int64.to_float b /. 1e6)
  else if Int64.compare b 1_000L > 0 then
    Fmt.str "%.1fKB" (Int64.to_float b /. 1e3)
  else Fmt.str "%LdB" b

let registry_mirror_stats_cmd =
  let run () cache_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, _fs, _clock, _sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let s = Oi.Source.Mirror.stats ~cache in
    Fmt.pr "Mirror: %s@." (Oi.Source.Mirror.dir ~cache);
    Fmt.pr "  blobs:      %d@." s.count;
    Fmt.pr "  total size: %s@." (human_bytes s.total_size)
  in
  let info =
    Cmd.info "stats"
      ~doc:"Show how many source tarballs are mirrored and their total size"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry mirror stats) prints a one-line summary of the \
             source mirror: the number of distinct tarballs it contains and \
             the total disk they occupy. Use it before an $(b,oi registry \
             export) to estimate how much data the export will ship, or to \
             track the size of the mirror over time.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

let registry_mirror_gc_cmd =
  let run () cache_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, _fs, _clock, _sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let n = Oi.Source.Mirror.gc ~cache in
    Fmt.pr "Removed %d orphaned blob(s)@." n
  in
  let info =
    Cmd.info "gc"
      ~doc:"Delete mirrored tarballs that no package still references"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry mirror gc) removes source tarballs from the \
             mirror when no package in the index still points at them. This \
             happens after you have built a newer version of a package but \
             kept the mirror around; the old tarball lingers on disk even \
             though nothing uses it any more.";
          `P
            "Safe to run at any time. If a later rebuild needs a tarball that \
             has been collected, the mirror re-fetches it from upstream on \
             demand.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

let registry_mirror_verify_cmd =
  let run () cache_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, _fs, _clock, sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    match Oi.Source.Mirror.verify ~sys ~cache with
    | [] -> Fmt.pr "All blobs verified OK@."
    | errs ->
        List.iter
          (fun (sha, msg) ->
            Fmt.epr "%a %s: %s@." Fmt.(styled `Red string) "BAD" sha msg)
          errs;
        Fmt.epr "%d blob(s) failed verification@." (List.length errs);
        exit 1
  in
  let info =
    Cmd.info "verify"
      ~doc:"Detect corrupted tarballs in the mirror by re-hashing them"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry mirror verify) walks every tarball in the mirror, \
             recomputes its sha256 checksum, and reports any file whose bytes \
             no longer match what the index records. The command exits with a \
             non-zero status if any tarball fails to verify, which makes it \
             useful in a scheduled integrity check.";
          `P
            "Run it before a large $(b,oi registry export) when the mirror has \
             been sitting around for a long time, or after any hardware event \
             that could have corrupted data on disk.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

let registry_mirror_list_cmd =
  let run () cache_dir package =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, _fs, _clock, _sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let entries = Oi.Source.Mirror.list ~cache ?package () in
    (* One line per (source, package) reference. Columns:
         <pkg.version>  <kind>  <size>  <sha256 (first 12)>  <url>
       sha256 is shortened for readability; pipe the raw column to
       sqlite3 if you need full hashes. *)
    List.iter
      (fun (e : Oi.Source.Mirror.entry) ->
        let pkg = e.package_name ^ "." ^ e.package_version in
        let kind =
          match e.kind with `Main -> "main" | `Extra n -> "extra:" ^ n
        in
        let short_sha =
          if String.length e.sha256 >= 12 then String.sub e.sha256 0 12
          else e.sha256
        in
        Fmt.pr "%-40s  %-16s  %-12s  %10s  %s@." pkg kind short_sha
          (Fmt.str "%a" Oi.Cache.pp_size e.size)
          e.url)
      entries;
    if entries = [] then
      match package with
      | Some p -> Fmt.pr "No sources in mirror for package %s@." p
      | None -> Fmt.pr "Mirror is empty@."
  in
  let package =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"PKG"
          ~doc:
            "Restrict the listing to tarballs referenced by the named package."
          [ "p"; "package" ])
  in
  let info =
    Cmd.info "list"
      ~doc:
        "Show every source tarball in the mirror, one row per package that \
         uses it"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi registry mirror list) prints one row for each tarball \
             reference in the mirror. Each row gives the package and version \
             that pulled the tarball in, the kind of source (main tarball or \
             extra patch), a short hash of the tarball, its on-disk size, and \
             the upstream URL it was fetched from.";
          `P
            "The same tarball can appear more than once when several packages \
             share a source. Those duplicate rows all point at the same short \
             hash, so the physical tarball is only counted once in the \
             mirror's size.";
          `P
            "Pass $(b,-p NAME) to restrict the output to a single package. \
             This is the fastest way to find out which sources a specific \
             package has contributed to the mirror.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term $ package)

let registry_mirror_cmd =
  let info =
    Cmd.info "mirror" ~doc:"Manage the local copy of upstream source tarballs"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Whenever $(b,oi) builds a package it keeps a copy of the source \
             tarball it fetched from upstream. Over time these copies form a \
             mirror of the opam ecosystem for the packages you actually use. \
             The mirror is shipped alongside the binary cache when you run \
             $(b,oi registry export), so downstream clients and offline \
             rebuilds do not have to reach the upstream servers.";
          `P
            "The subcommands in this group let you inspect and maintain the \
             mirror: $(b,stats) for a size summary, $(b,list) for a \
             per-tarball listing, $(b,verify) to re-hash every tarball, and \
             $(b,gc) to drop tarballs that no package still references.";
        ]
  in
  Cmd.group info
    [
      registry_mirror_stats_cmd;
      registry_mirror_list_cmd;
      registry_mirror_gc_cmd;
      registry_mirror_verify_cmd;
    ]

let registry_cmd =
  let info =
    Cmd.info "registry"
      ~doc:"Manage the cache of pre-built packages and the remote registry"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi) keeps a local cache of pre-built OCaml packages so it can \
             avoid repeated work, and it can pull pre-built packages from a \
             remote registry instead of compiling them. This group of commands \
             inspects and manages both sides of that arrangement.";
          `P
            "Most users only need $(b,oi registry list) to inspect the local \
             cache. The $(b,build) and $(b,export) subcommands come in when \
             you are running your own registry for a team or a set of \
             machines. The $(b,mirror) subgroup handles the companion mirror \
             of upstream source tarballs.";
        ]
  in
  Cmd.group info
    [
      registry_list_cmd;
      registry_index_cmd;
      registry_export_cmd;
      registry_build_cmd;
      registry_docker_cmd;
      registry_mirror_cmd;
    ]

(* -- repo (reporepo of overlay pins) ------------------------------------ *)

let reporepo_term =
  Arg.(
    value
    & opt string (reporepo_path ())
    & info ~docv:"DIR"
        ~doc:
          "Local directory that contains the reporepo clone to operate on. \
           Falls back to $(b,\\$OI_REPOREPO), and then to \
           $(b,\\$OI_DATA_DIR/reporepo) under the XDG data hierarchy."
        [ "reporepo" ])

let reporepo_url_term =
  Arg.(
    value
    & opt string (reporepo_url ())
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

let repo_list_cmd =
  let run () reporepo reporepo_url no_check =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
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
    Term.(const run $ log_term $ reporepo_term $ reporepo_url_term $ no_check)

let repo_show_cmd =
  let run () reporepo reporepo_url handle =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
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
    Term.(const run $ log_term $ reporepo_term $ reporepo_url_term $ handle)

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

let repo_add_cmd =
  let run () reporepo reporepo_url handle url ref_ toolchain depend_specs force
      =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
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
      const run $ log_term $ reporepo_term $ reporepo_url_term $ handle $ url
      $ ref_term $ toolchain_repo_term $ depend_term $ force)

let repo_bump_cmd =
  let run () reporepo reporepo_url handle url ref_ toolchain depend_specs =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
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
      const run $ log_term $ reporepo_term $ reporepo_url_term $ handle $ url
      $ ref_term $ toolchain_repo_term $ depend_term)

let repo_set_roots_cmd =
  (* Parse a PKG token: a comma-separated list becomes a multi-package
     solve group; a bare name becomes a singleton group. Empty tokens
     between commas are dropped. *)
  let parse_group token =
    String.split_on_char ',' token
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let run () reporepo reporepo_url handle pkgs =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
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
      const run $ log_term $ reporepo_term $ reporepo_url_term $ handle $ pkgs)

let repo_remove_cmd =
  let run () reporepo reporepo_url handle_spec =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
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
      const run $ log_term $ reporepo_term $ reporepo_url_term $ handle_spec)

let repo_push_cmd =
  let run () reporepo reporepo_url push_url =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
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
    Term.(const run $ log_term $ reporepo_term $ reporepo_url_term $ push_url)

let repo_cmd =
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
      repo_list_cmd;
      repo_show_cmd;
      repo_add_cmd;
      repo_bump_cmd;
      repo_set_roots_cmd;
      repo_remove_cmd;
      repo_push_cmd;
    ]

(* -- main ---------------------------------------------------------------- *)

let () =
  let info =
    Cmd.info "oi" ~version:"0.5.1"
      ~doc:"A fast, stateless OCaml package manager"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi) is a fast, stateless OCaml package manager. It reads the \
             $(b,*.opam) manifests that OCaml projects ship, consults the \
             community's opam repositories, resolves what is needed, and then \
             builds, installs, or runs the result on demand. Every build it \
             performs is cached, so repeated invocations reuse the work done \
             before.";
          `P
            "$(b,oi) is designed to stay out of your way. It does not require \
             a persistent switch, does not pollute your home directory outside \
             of clearly-named cache and data directories, and does not leave \
             any long-lived state.";
          `S "QUICK START";
          `P
            "Run any tool from the opam ecosystem. The first invocation builds \
             what it needs; every later invocation hits the cache and starts \
             instantly.";
          `Pre "  oi run utop\n  oi run ocamlformat -- --help";
          `P
            "Pin a dependency to a specific version with $(b,pkg.VERSION), \
             $(b,pkg=VERSION), or any of the opam relational operators:";
          `Pre
            "  oi run --with=dune.3.20.0 -- dune --version\n\
            \  oi run --with=fmt>=0.9 my_script.ml";
          `P
            "Run a package straight from a git repository. $(b,oi) clones the \
             URL and treats every $(b,*.opam) file at its root as a pin:";
          `Pre
            "  oi run --with=https://github.com/owner/project.git target\n\
            \  oi run --with=git+https://example.org/foo.git#branch foo";
          `P
            "Run a standalone OCaml script. Declare its dependencies on the \
             first line of the file:";
          `Pre
            "  [@@@opam fmt cmdliner lwt>=5.0 ppx_deriving.show]\n\
            \  let () = ...\n\n\
            \  oi run my_script.ml\n\
            \  oi run https://example.com/hello.ml";
          `P
            "Inside a project, $(b,oi sync) installs the project's \
             dependencies into $(b,_oi/prefix/) and writes a $(b,.envrc) for \
             $(b,direnv). The sync also installs dev tools ($(b,odoc), \
             $(b,merlin), $(b,ocaml-lsp-server), plus $(b,mdx) and \
             $(b,ocamlformat) when the project uses them) into \
             $(b,_oi/tools/).";
          `Pre
            "  oi sync\n\
            \  direnv allow      # or: eval \"\\$(oi env)\"\n\
            \  oi exec dune build";
          `P
            "Add a new dependency. $(b,oi) edits $(b,dune-project), \
             regenerates the $(b,*.opam) files, and re-syncs:";
          `Pre "  oi add logs\n  oi add \"fmt>=0.9\"";
          `P
            "An $(i,overlay) is somebody's curated opam repository, pinned to \
             a specific git commit and referred to by a short $(i,handle). The \
             $(i,reporepo) is the directory of overlays that $(b,oi) knows \
             about. See $(b,oi repo) for how to manage it. Prefix any target \
             or $(b,--with) value with $(i,@HANDLE/) to take a package from a \
             specific overlay:";
          `Pre
            "  oi run @avsm/owntracks\n  oi run --with=@avsm/crockford roguedoi";
          `P "Find a binary or package across every source $(b,oi) knows about:";
          `Pre "  oi search dune\n  oi search 'ocaml*'\n  oi search @avsm/irmin";
          `P "Preview without actually doing anything:";
          `Pre "  oi show utop\n  oi show --tree utop\n  oi run -n utop";
          `S "COMMAND CATEGORIES";
          `I
            ( "$(b,Getting started)",
              "$(b,run) executes a binary or an OCaml script, fetching any \
               missing dependencies and caching them for next time." );
          `I
            ( "$(b,Working in a project)",
              "$(b,sync) installs project dependencies and dev tools. \
               $(b,exec) runs a command in the project environment. $(b,env) \
               prints that environment for use with $(b,eval). $(b,add) \
               records a new dependency in $(b,dune-project)." );
          `I
            ( "$(b,Checking what's going on)",
              "$(b,show) summarises the build plan and lists any missing \
               system packages; pass $(b,--tree) for the full per-package plan \
               or $(b,--only-depexts) for a list suitable for piping into a \
               package manager. $(b,search) finds a binary or package across \
               caches and overlays. $(b,config) reports the platform, cache \
               directories, project state, and dev-tool probes." );
          `I
            ( "$(b,Sharing builds and managing disk)",
              "$(b,registry) manages the pre-built package cache and source \
               mirror. $(b,clean) frees disk space." );
          `I
            ( "$(b,Picking package sources)",
              "$(b,repo) manages the reporepo (see QUICK START): register \
               overlays, inspect their pinned commits, and bump them forward."
            );
          `S "SCRIPT FORMAT";
          `P
            "The first line of a $(b,.ml) script declares its dependencies \
             using an attribute:";
          `Pre "  [@@@opam fmt cmdliner>=1.2.0 lwt]";
          `P
            "Each token names an opam package. An optional version constraint \
             uses the usual relational operators ($(b,>=), $(b,>), $(b,<=), \
             $(b,<), $(b,=)). A dotted suffix selects a findlib sub-library, \
             for example $(b,ppx_deriving.show).";
          `P
            "Any package whose name starts with $(b,ppx_) is wired in as a PPX \
             preprocessor. Run $(b,oi run -vv SCRIPT.ml) to see the generated \
             build file.";
          `S Manpage.s_environment;
          `P
            "$(b,oi) works out of two directories. The data directory holds \
             long-lived state: cloned opam repositories and the relocatable \
             compiler toolchains. The cache directory holds rebuildable data: \
             pre-built packages, assembled prefixes, and the source mirror. \
             Each directory can be pointed elsewhere by setting one \
             environment variable, or by passing a command-line flag that \
             takes precedence.";
          `I
            ( "$(b,OI_DATA_DIR)",
              "Override the data directory. Falls back to \
               $(b,XDG_DATA_HOME/oi), then to $(b,~/.local/share/oi)." );
          `I
            ( "$(b,OI_CACHE_DIR)",
              "Override the cache directory. Falls back to \
               $(b,XDG_CACHE_HOME/oi), then to $(b,~/.cache/oi)." );
          `I
            ( "$(b,OI_REPOREPO)",
              "Override the location of the reporepo clone. Defaults to \
               $(b,\\$OI_DATA_DIR/reporepo)." );
          `I
            ( "$(b,OI_REPOREPO_URL)",
              "Override the upstream URL used to clone the reporepo on first \
               use. Defaults to the built-in upstream. Once the local clone \
               exists, $(b,oi) never pulls from this URL again. The clone is \
               yours to edit, commit, and push." );
        ]
  in
  let cmd =
    Cmd.group info
      [
        run_cmd;
        add_cmd;
        exec_cmd;
        search_cmd;
        show_cmd;
        sync_cmd;
        env_cmd;
        config_cmd;
        registry_cmd;
        repo_cmd;
        clean_cmd;
      ]
  in
  exit (Cmd.eval cmd)
