[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** [oi] entry point.

    Each top-level command lives in its own module under {!Oi_cmd}; the program
    info and shared man-page text are the only things this file still owns. *)

open Cmdliner

(* Populated by dune-build-info from the [oi] package's opam file (or
   [git describe] for a dev build). [n/a] when the binary was built
   outside a dune package context. *)
let version =
  match Build_info.V1.version () with
  | Some v -> Build_info.V1.Version.to_string v
  | None -> "n/a"

let info =
  Cmd.info "oi" ~version ~doc:"A fast, stateless OCaml package manager"
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
          "$(b,oi) is designed to stay out of your way. It does not require a \
           persistent switch, does not pollute your home directory outside of \
           clearly-named cache and data directories, and does not leave any \
           long-lived state.";
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
          "Inside a project, $(b,oi sync) installs the project's dependencies \
           into $(b,_oi/prefix/) and writes a $(b,.envrc) for $(b,direnv). The \
           sync also installs dev tools ($(b,odoc), $(b,merlin), \
           $(b,ocaml-lsp-server), plus $(b,mdx) and $(b,ocamlformat) when the \
           project uses them) into $(b,_oi/tools/).";
        `Pre
          "  oi sync\n\
          \  direnv allow      # or: eval \"\\$(oi env)\"\n\
          \  oi exec dune build";
        `P
          "Add a new dependency. $(b,oi) edits $(b,dune-project), regenerates \
           the $(b,*.opam) files, and re-syncs:";
        `Pre "  oi add logs\n  oi add \"fmt>=0.9\"";
        `P
          "An $(i,overlay) is somebody's curated opam repository, pinned to a \
           specific git commit and referred to by a short $(i,handle). The \
           $(i,reporepo) is the directory of overlays that $(b,oi) knows \
           about. See $(b,oi repo) for how to manage it. Prefix any target or \
           $(b,--with) value with $(i,@HANDLE/) to take a package from a \
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
            "$(b,sync) installs project dependencies and dev tools. $(b,exec) \
             runs a command in the project environment. $(b,env) prints that \
             environment for use with $(b,eval). $(b,add) records a new \
             dependency in $(b,dune-project)." );
        `I
          ( "$(b,Checking what's going on)",
            "$(b,show) summarises the build plan and lists any missing system \
             packages; pass $(b,--tree) for the full per-package plan or \
             $(b,--only-depexts) for a list suitable for piping into a package \
             manager. $(b,search) finds a binary or package across caches and \
             overlays. $(b,config) reports the platform, cache directories, \
             project state, and dev-tool probes." );
        `I
          ( "$(b,Sharing builds and managing disk)",
            "$(b,registry) manages the pre-built package cache and source \
             mirror. $(b,clean) frees disk space." );
        `I
          ( "$(b,Picking package sources)",
            "$(b,repo) manages the reporepo (see QUICK START): register \
             overlays, inspect their pinned commits, and bump them forward." );
        `S "SCRIPT FORMAT";
        `P
          "The first line of a $(b,.ml) script declares its dependencies using \
           an attribute:";
        `Pre "  [@@@opam fmt cmdliner>=1.2.0 lwt]";
        `P
          "Each token names an opam package. An optional version constraint \
           uses the usual relational operators ($(b,>=), $(b,>), $(b,<=), \
           $(b,<), $(b,=)). A dotted suffix selects a findlib sub-library, for \
           example $(b,ppx_deriving.show).";
        `P
          "Any package whose name starts with $(b,ppx_) is wired in as a PPX \
           preprocessor. Run $(b,oi run -vv SCRIPT.ml) to see the generated \
           build file.";
        `S Manpage.s_environment;
        `P
          "$(b,oi) works out of two directories. The data directory holds \
           long-lived state: cloned opam repositories and the relocatable \
           compiler toolchains. The cache directory holds rebuildable data: \
           pre-built packages, assembled prefixes, and the source mirror. Each \
           directory can be pointed elsewhere by setting one environment \
           variable, or by passing a command-line flag that takes precedence.";
        `I
          ( "$(b,OI_DATA_DIR)",
            "Override the data directory. Falls back to $(b,XDG_DATA_HOME/oi), \
             then to $(b,~/.local/share/oi)." );
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

let () =
  let group =
    Cmd.group info
      [
        Oi_cmd.Run.cmd;
        Oi_cmd.Add.cmd;
        Oi_cmd.Exec.cmd;
        Oi_cmd.Search.cmd;
        Oi_cmd.Show.cmd;
        Oi_cmd.Sync.cmd;
        Oi_cmd.Env.cmd;
        Oi_cmd.Config.cmd;
        Oi_cmd.Registry.cmd;
        Oi_cmd.Repo.cmd;
        Oi_cmd.Clean.cmd;
      ]
  in
  exit (Cmd.eval group)
