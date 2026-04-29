open Cmdliner

let ( / ) = Filename.concat

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Tty.Progress.logs_reporter (Logs_fmt.reporter ()))

let log = Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

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

let cache_dir = Xdge.Cmd.cache_term Workspace.app_name

let refresh =
  Arg.(
    value & flag
    & info
        ~doc:
          "Re-fetch repos, pinned sources, and git URLs even if fresh. Caches \
           refresh on their own after 24h."
        [ "refresh" ])

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

let remote_of_registry = function
  | "" -> None
  | url -> Some (`Http_remote url : D10.Layer.remote)
