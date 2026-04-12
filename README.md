# oi -- an OCaml installer

oi is a fast, stateless OCaml package runner that can execute binaries
directly from the package registry without requiring any global state.

## Quick start

Run any binary from the opam repository:

    oi run utop
    oi run ocamlformat -- --help
    oi run --with=ocluster ocluster-admin

The first invocation solves, builds, caches, and runs. The second is instant.

Run a local OCaml script with dependencies:

    oi run my_script.ml

Show what would be built without building:

    oi run -n utop

## Script format

OCaml source files declare opam dependencies on the first line:

```ocaml
[@@@opam fmt cmdliner lwt]

let () =
  Fmt.pr "Hello from oi!@."
```

Version constraints use standard opam syntax:

```ocaml
[@@@opam fmt>=0.9.0 cmdliner>=1.2.0 lwt]
```

The compiled binary is cached by a hash of the script contents and its
dependencies. Edits trigger a rebuild and unchanged scripts run instantly.

## Specifying packages

When the binary name doesn't match the package name, use `--with`:

    oi run --with=crockford roguedoi
    oi run --with=tls-mirage mirage

Multiple `--with` flags are supported. All named packages are solved
together and assembled into a single prefix.

Additional opam repositories can be included with `--with-repo`:

    oi run --with-repo=https://github.com/user/repo.git my-tool

## Build plan

To see the fully resolved execution plan with commands, environment,
and layer hashes:

    oi plan utop

The `--dry-run` flag on `oi run` shows a compact summary of what would
be built (source) vs restored from cache (binary):

    oi run -n utop

## Project environments

In a project directory with `.opam` files, `oi sync` reads all dependency
declarations, solves, builds, and assembles a prefix into `_oi/prefix/`:

    cd my-project/
    oi sync

This writes a `.envrc` using direnv stdlib functions. Activate with:

    direnv allow

After that, entering the directory automatically sets all the env variables
needed by the OCaml toolchain. Running `oi sync` again after changing `.opam`
files updates the prefix (TODO: figure out if direnv lets us automate this). 

## Examining the cache

    oi show              # list all cached binary layers
    oi show fmt          # details for a specific package
    oi index             # rebuild SQLite index for fast lookups
    oi tools             # relocatable compiler and repo status
    oi clean             # show what can be cleaned

## How it works

`oi` uses a relocatable OCaml compiler (from @dra27's fork) cached as a
binary layer. Packages are solved with opam-0install and built in parallel
stages: packages within a stage have no inter-dependencies and build
concurrently, while install is serialised at stage boundaries.
After each successful install, the newly installed files are captured as a
content-addressed layer.

Layers are keyed by the MD5 of the concatenated SHA512 hashes of the opam
`effective_part` of the package and all its dependencies, so any change to
a package or its deps produces a new layer.

A prefix is a directory tree assembled by hardlinking files from all
package layers. Prefixes are cached by a hash of their constituent layers,
so identical solves reuse the same assembled prefix. Different `oi run`
invocations with different dependency trees get independent prefixes.
