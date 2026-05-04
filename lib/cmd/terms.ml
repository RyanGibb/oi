open Cmdliner

let ( / ) = Filename.concat

let setup_log style_renderer level verbose_http =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Tty.Progress.logs_reporter (Logs_fmt.reporter ()));
  Requests.Cmd.setup_log_sources ~verbose_http level;
  (* Quiet the HTTP-stack sources unless the user explicitly asks for
     protocol-level chatter. [Requests.Cmd.setup_log_sources] only sets a
     handful of [requests.*] sources; the rest (the HTTP/2 client, the
     connection pool handler, the TLS layer) inherit the global Info level
     and emit per-request "ALPN / handshake / Frame reader" lines that
     drown out our own narration. Iterate the registered sources and
     pin every HTTP-related one to [Warning] when [--verbose-http] is
     off; [--verbose-http] (or [OI_VERBOSE_HTTP=1]) restores the library's
     own level mapping. *)
  if not verbose_http then begin
    let prefixes = [ "requests"; "h2."; "tls."; "h1." ] in
    List.iter
      (fun src ->
        let name = Logs.Src.name src in
        if List.exists (fun p -> String.starts_with ~prefix:p name) prefixes
        then Logs.Src.set_level src (Some Logs.Warning))
      (Logs.Src.list ())
  end

let verbose_http_term = Requests.Cmd.verbose_http_term Workspace.app_name

let log =
  Term.(
    const (fun s l v -> setup_log s l v.Requests.Cmd.value)
    $ Fmt_cli.style_renderer () $ Logs_cli.level () $ verbose_http_term)

let data_dir =
  let app_upper = String.uppercase_ascii Workspace.app_name in
  let app_env = app_upper ^ "_DATA_DIR" in
  let xdg_var = "XDG_DATA_HOME" in
  let home = Sys.getenv "HOME" in
  let default_path = home / ".local" / "share" / Workspace.app_name in
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
              | Some v when v <> "" -> v / Workspace.app_name
              | _ -> default_path))
    $ arg)

let cache_dir = Xdg_eio.Cmd.cache_term Workspace.app_name

(* -- Shared "global" options ------------------------------------------- *)

(* Cmdliner subcommand groups don't have real top-level flags: every flag
   is per-command. [common] is the workaround — one bundle every harness-
   using command consumes, so [--cache-dir], [--data-dir], [--verbosity],
   [--color], and [--verbose-http] always appear together with the same
   semantics. Adding a new "should be global" flag means adding it here
   once; the type-checker fans the change out across every command. *)

type common = { cache_dir : string; data_dir : string }

let common =
  Term.(
    const (fun () cache_dir data_dir -> { cache_dir; data_dir })
    $ log $ cache_dir $ data_dir)

type format = Text | Json

let format_conv =
  let parser = function
    | "text" -> Ok Text
    | "json" -> Ok Json
    | s -> Error (`Msg (Fmt.str "expected 'text' or 'json', got %S" s))
  in
  let printer fmt = function
    | Text -> Fmt.string fmt "text"
    | Json -> Fmt.string fmt "json"
  in
  Arg.conv ~docv:"FORMAT" (parser, printer)

let format =
  Arg.(
    value & opt format_conv Text
    & info ~docv:"FORMAT" ~doc:"Output format ($(b,text) or $(b,json))."
        [ "format" ])

let refresh =
  Arg.(
    value & flag
    & info
        ~doc:
          "Re-fetch repos, pinned sources, and git URLs even if fresh. Caches \
           refresh on their own after 24h."
        [ "refresh" ])

let skip_local =
  Arg.(
    value & flag
    & info
        ~doc:
          "Don't probe the current directory for project files ($(b,*.opam), \
           $(b,pin-depends:), $(b,x-repos:), $(b,packages/) overlay, dev \
           tools). Use to run a command against global state only."
        [ "skip-local" ])

let with_repos =
  Arg.(
    value & opt_all string []
    & info ~docv:"URL"
        ~doc:
          "Stack an opam repository (git URL or reporepo handle) on the solve. \
           Repeatable."
        [ "with-repo" ])

let jobs =
  Arg.(
    value
    & opt (some int) None
    & info ~docv:"N" ~doc:"Cap parallel builds and fetches. Default 4."
        [ "j"; "jobs" ])

let with_deps =
  Arg.(
    value & opt_all string []
    & info ~docv:"PKG"
        ~doc:
          "Add a solver root: a package name, opam atom ($(b,fmt>=0.9), \
           $(b,dune.3.20.0)), or git URL (every $(b,*.opam) at its root \
           becomes a pin). Repeatable."
        [ "with" ])

let toolchain =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"HANDLE"
        ~doc:
          "Pin the compiler set from the named reporepo toolchain. $(b,oi \
           config) lists handles. Non-relocatable toolchains (oxcaml) install \
           into $(b,\\$XDG_CACHE_HOME/oi/toolchains/) on first use."
        [ "toolchain" ])

let default_registry = "https://oi.ci.dev"

let registry =
  let doc =
    Fmt.str
      "Remote layer registry URL (default: %s). Layers fetched from \
       $(docv)/$(b,<os>/<hash>.tar.zst) before building from source."
      default_registry
  in
  Arg.(
    value & opt string default_registry & info ~docv:"URL" ~doc [ "registry" ])

let getenv_or ~default name =
  match Sys.getenv_opt name with Some v when v <> "" -> v | _ -> default

let reporepo_path () =
  getenv_or ~default:Oi.Source.Reporepo.default_path "OI_REPOREPO"

let reporepo_url () =
  getenv_or ~default:Oi.Source.Reporepo.default_url "OI_REPOREPO_URL"

let use_registry_conv =
  let parser s =
    match Oi.Use_registry.of_string s with
    | Ok v -> Ok v
    | Error msg -> Error (`Msg msg)
  in
  Arg.conv ~docv:"MODE" (parser, Oi.Use_registry.pp)

let use_registry =
  let doc =
    "What to consult the remote registry for. $(b,all) (default): binary \
     layers and source archives. $(b,archives): source archives only — every \
     layer is built from scratch (CI registry-build mode). $(b,never): offline \
     w.r.t. the registry; upstream source fetches still happen."
  in
  Arg.(
    value
    & opt use_registry_conv Oi.Use_registry.All
    & info ~docv:"MODE" ~doc [ "use-registry" ])

type remotes = {
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
}

let remotes_of ~url ~(mode : Oi.Use_registry.t) =
  match mode with
  | Never -> { layer_remote = None; source_remote = None }
  | All | Archives ->
      if url = "" then
        Oi.Error.config_error
          "--use-registry=%s requires a non-empty --registry URL (use \
           --use-registry=never for fully offline)"
          (Oi.Use_registry.to_string mode);
      let r : D10.Layer.remote = `Http_remote url in
      {
        layer_remote = (if mode = All then Some r else None);
        source_remote = Some r;
      }
