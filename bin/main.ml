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
          "$(b,oi) reads the $(b,*.opam) manifests OCaml projects ship, solves \
           against opam-repository, and builds, runs, or publishes the result. \
           Every build is content-addressed and cached, so repeat invocations \
           are quick, and no global state is required to reproduce most \
           commands.";
        `S "QUICK START";
        `P "$(b,1.) Run any tool published on opam:";
        `Pre
          "  oi run utop\n\
          \  oi run ocamlformat -- --help\n\
          \  oi run --with=dune.3.20.0 -- dune --version";
        `P "$(b,2.) Run an $(b,.ml) script with deps on the first line:";
        `Pre "  echo '[@@@opam fmt cmdliner]' > hello.ml\n  oi run hello.ml";
        `P
          "$(b,3.) Build, test, and develop in a project. From the project \
           root:";
        `Pre
          "  oi build               # sync deps + dev tools, run dune build\n\
          \  oi test                # dune runtest\n\
          \  oi build --deps-only   # sync only (after editing a *.opam)\n\
          \  oi exec -- dune utop   # any command, project env applied\n\
          \  oi add logs            # edit dune-project + re-solve";
        `P
          "If $(b,direnv) is installed, $(b,oi build) writes $(b,.envrc) so \
           your shell auto-activates the prefix. Otherwise:";
        `Pre "  eval \"\\$(oi env)\"";
        `P
          "$(b,4.) Pull packages from somebody's curated overlay (a git-pinned \
           opam-repository). The $(i,reporepo) lists known overlay handles; \
           $(b,oi repo) manages it.";
        `Pre
          "  oi run @avsm/owntracks\n  oi run --with=@avsm/crockford roguedoi";
        `P "$(b,5.) Inspect, search, dry-run before doing anything:";
        `Pre
          "  oi show utop                 # build plan + depexts\n\
          \  oi show --all                # every cached layer\n\
          \  oi search dune               # find a binary or package\n\
          \  oi run -n utop               # show the plan, run nothing";
        `P
          "$(b,6.) Publish a registry of pre-built layers + source mirror to a \
           static HTTP server:";
        `Pre
          "  oi build --all --export ./registry\n\
          \  rsync -a ./registry/ user@server:/srv/oi-registry/\n\
          \  oi run --registry=https://server/oi-registry @avsm/owntracks";
        `P "$(b,7.) Generate Dockerfiles for CI:";
        `Pre
          "  oi docker                                    # project build\n\
          \  oi docker --test --distro=alpine-3.23        # project tests\n\
          \  oi docker --all -o ./registry-build          # multi-distro";
        `S "SCRIPT FORMAT";
        `P "The first line of a $(b,.ml) script declares its dependencies:";
        `Pre "  [@@@opam fmt cmdliner>=1.2.0 lwt]";
        `P
          "Each token is an opam package, with optional version constraint \
           ($(b,>=), $(b,>), $(b,<=), $(b,<), $(b,=)) and optional findlib \
           sub-library ($(b,ppx_deriving.show)). $(b,ppx_*) packages are wired \
           in as preprocessors automatically. $(b,oi run -vv SCRIPT.ml) prints \
           the generated dune project.";
        `S Manpage.s_environment;
        `P
          "$(b,oi) uses two directories. The data directory holds long-lived \
           state (opam-repository clones, toolchains). The cache directory \
           holds rebuildable data (layers, prefixes, source mirror). Each is \
           overridable.";
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
        `S Manpage.s_see_also;
        `P "$(b,oix)(1) — one-shot runner for opam-packaged binaries.";
      ]

let () =
  let group =
    Cmd.group info
      [
        Oi_cmd.Run.cmd;
        Oi_cmd.Build.cmd;
        Oi_cmd.Build.test_cmd;
        Oi_cmd.Docker.cmd;
        Oi_cmd.Add.cmd;
        Oi_cmd.Exec.cmd;
        Oi_cmd.Search.cmd;
        Oi_cmd.Show.cmd;
        Oi_cmd.Env.cmd;
        Oi_cmd.Config.cmd;
        Oi_cmd.Repo.cmd;
        Oi_cmd.Clean.cmd;
      ]
  in
  exit (Cmd.eval group)
