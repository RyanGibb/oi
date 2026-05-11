[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

module DF = Dockerfile
module Distro = Dockerfile_opam.Distro

(* Shadow stdlib [@@] with Dockerfile's concat in this module. *)
let ( @@ ) = Dockerfile.( @@ )

(* -- Per-package-manager install command helpers ------------------------- *)

(* Bootstrap toolchain needed before any opam-driven build can run.
   Library [-dev] depexts and package-specific bits (libssl-dev,
   libsqlite3-dev, libblosc-dev, Apache Arrow, …) are intentionally NOT
   listed here — they belong in the [depexts:] field of the opam package
   that needs them, and flow into the Dockerfile via
   [compute_overlay_depexts]. This list is just "what does the host need
   to invoke a C compiler and fetch sources before opam takes over". *)
let build_depexts = function
  | `Apk ->
      (* coreutils: oxcaml-compiler's Makefile uses [cp -l -R] to set up
         [_build/runtime_stdlib_install]; busybox cp drops the file
         contents on directory hardlinks, leaving Makefile.config missing. *)
      "build-base m4 perl pkgconf autoconf git curl bash patch tar xz zstd \
       rsync sudo coreutils ca-certificates linux-headers"
  | `Apt ->
      "build-essential m4 perl pkg-config autoconf git curl bash patch tar \
       xz-utils zstd rsync sudo ca-certificates"
  | `Yum ->
      "gcc gcc-c++ make m4 perl pkgconf-pkg-config autoconf git curl bash \
       patch tar xz zstd rsync sudo ca-certificates findutils which diffutils"
  | _ -> failwith "unsupported package manager"

(* -- Distro → opam platform variables ----------------------------------- *)

type opam_vars = {
  os_distribution : string;
  os_family : string;
  os_version : string;
}

(* Parse [tag_of_distro] output (e.g. "alpine-3.23", "ubuntu-25.10") into
   a [distribution-name, version] pair. Distros whose tags contain a
   dash in the distribution segment (e.g. "opensuse-leap-15.6") are
   handled by matching on the variant itself below. *)
let parse_tag tag =
  match String.index_opt tag '-' with
  | Some i ->
      (String.sub tag 0 i, String.sub tag (i + 1) (String.length tag - i - 1))
  | None -> (tag, "")

let opam_vars_of_distro (d : Distro.t) =
  let resolved = Distro.resolve_alias d in
  let tag = Distro.tag_of_distro (resolved :> Distro.t) in
  let name, version = parse_tag tag in
  (* opam's own [os-family] lookup uses /etc/os-release's [ID_LIKE]
     on Linux. The mapping below mirrors what opam would see for each
     canonical distro family. *)
  let os_family =
    match name with
    | "ubuntu" -> "debian"
    | "debian" -> "debian"
    | "centos" | "oraclelinux" -> "rhel"
    | "fedora" -> "fedora"
    | "rhel" -> "rhel"
    | "alpine" -> "alpine"
    | "opensuse" -> "suse"
    | "archlinux" | "arch" -> "arch"
    | other -> other
  in
  let os_distribution =
    match name with "archlinux" -> "arch" | other -> other
  in
  { os_distribution; os_family; os_version = version }

let install_cmd mgr pkgs =
  match mgr with
  | `Apk -> Printf.sprintf "apk add --no-cache %s" pkgs
  | `Apt ->
      Printf.sprintf
        "apt-get update -qq && DEBIAN_FRONTEND=noninteractive apt-get install \
         -y --no-install-recommends %s && rm -rf /var/lib/apt/lists/*"
        pkgs
  | `Yum -> Printf.sprintf "dnf install -y %s && dnf clean all" pkgs
  | _ -> failwith "unsupported package manager"

(* -- Stage 0: static-linked oi binary built on alpine/musl --------------- *)

(* [OI_STATIC=1] is picked up by the root [dune]'s discover rule and
   appended to the release-profile link flags as [-cclib -static].
   Building inside Alpine/musl with that flag yields a fully static
   binary that runs on any Linux distro. *)
let oi_builder_stage ~src_context =
  let apk_build_pkgs =
    "build-base m4 perl pkgconf git curl bash patch gmp-dev sqlite-dev \
     sqlite-static openssl-dev openssl-libs-static zlib-dev zlib-static"
  in
  DF.comment "=== Stage: oi-builder (static musl binary on alpine) ==="
  @@ DF.from ~alias:"oi-builder" ~tag:"alpine-3.22-ocaml-5.4" "ocaml/opam"
  @@ DF.user "root"
  @@ DF.run "apk add --no-cache %s" apk_build_pkgs
  @@ DF.user "opam"
  @@ DF.workdir "/home/opam/src"
  @@ DF.copy ~chown:"opam:opam" ~src:[ src_context ] ~dst:"/home/opam/src" ()
  @@ DF.env
       [
         ("OPAMYES", "true");
         ("OPAMCONFIRMLEVEL", "unsafe-yes");
         ("OI_STATIC", "1");
         ("CI", "true");
       ]
  (* The ocaml/opam image's [default] repo points at a baked-in local
     clone (git+file:///home/opam/opam-repository), so `opam update
     default` alone just re-reads that frozen snapshot. Pull the clone
     from upstream first so recent packages (e.g. dune >= 3.21 required
     by tomlt) are visible to the solver. *)
  @@ DF.run
       "cd ~/opam-repository && git pull --quiet origin master && opam-2.5 \
        update default"
  @@ DF.run "opam-2.5 install ."
  @@ DF.run "opam-2.5 exec -- dune build --profile=release bin/main.exe"
  @@ DF.user "root"
  @@ DF.run
       "mkdir -p /out && cp _build/default/bin/main.exe /out/oi && chmod 755 \
        /out/oi && strip /out/oi"

(* -- Per-distro build stage --------------------------------------------- *)

(* GitHub repo whose releases page hosts the static [oi] binaries.
   Change this if you maintain a fork; every per-distro image pulls
   [oi-linux-<arch>] from [<release_repo>/releases/latest/download/]. *)
let release_repo = "avsm/oi"

(* A short, filesystem-safe stage alias for a resolved distro. *)
let stage_alias (d : Distro.distro) =
  let tag = Distro.tag_of_distro (d :> Distro.t) in
  "build-" ^ tag

(* Each distro stage installs the generic build toolchain and the union
   of overlay depexts, then fetches the latest statically linked
   [oi-linux-<arch>] and [oix-linux-<arch>] from the [oi] GitHub
   releases page rather than building from source. This keeps image
   builds fast and makes the release workflow the single source of
   truth for both binaries. The stage doesn't run the build/export
   itself: the compose file supplies a [command:] that invokes
   [oi build --all --export /out]. *)
let distro_stage ?(overlay_depexts = []) d =
  let resolved = Distro.resolve_alias d in
  let img, tag = Distro.base_distro_tag (resolved :> Distro.t) in
  let mgr = Distro.package_manager (resolved :> Distro.t) in
  let alias = stage_alias resolved in
  let human = Distro.human_readable_string_of_distro (resolved :> Distro.t) in
  let fetch_oi =
    Printf.sprintf
      "arch=$(uname -m) && curl -fsSL -o /usr/local/bin/oi \
       https://github.com/%s/releases/latest/download/oi-linux-$arch && curl \
       -fsSL -o /usr/local/bin/oix \
       https://github.com/%s/releases/latest/download/oix-linux-$arch && chmod \
       0755 /usr/local/bin/oi /usr/local/bin/oix"
      release_repo release_repo
  in
  let base = build_depexts mgr in
  (* Dedup overlay depexts against the bootstrap toolchain so the
     install command doesn't list e.g. [git] twice. *)
  let base_words =
    String.split_on_char ' ' base |> List.filter (fun s -> s <> "")
  in
  let overlay_extras =
    overlay_depexts
    |> List.filter (fun p -> not (List.mem p base_words))
    |> List.sort_uniq String.compare
  in
  let combined =
    match overlay_extras with
    | [] -> base
    | xs -> base ^ " " ^ String.concat " " xs
  in
  DF.comment "=== Stage: %s ===" human
  @@ DF.from ~alias ~tag img
  @@ DF.env [ ("CI", "true") ]
  @@ DF.run "%s" (install_cmd mgr combined)
  @@ DF.run "%s" fetch_oi @@ DF.workdir "/work"

(* -- Top-level Dockerfiles ---------------------------------------------- *)

let dockerfile_oi ~src_context =
  DF.comment
    "Generated by `oi docker --all` -- static musl build of oi on alpine."
  @@ DF.comment
       "Build: docker buildx build -f Dockerfile.oi --output \
        type=local,dest=./oi-bin ."
  @@ oi_builder_stage ~src_context
  @@ DF.comment "=== Stage: final (just the static binary) ==="
  @@ DF.from "scratch"
  @@ DF.copy ~from:"oi-builder" ~src:[ "/out/oi" ] ~dst:"/oi" ()

(* Project Dockerfile: bake oi + build depexts into a distro stage,
   then [COPY . /src] and run [cmd]. Used by [oi build --docker]
   ([cmd = "oi build"]) and [oi test --docker] ([cmd = "oi test"]). *)
let dockerfile_project ?(overlay_depexts = []) ?(cmd = "oi build")
    ?(generator = "oi build --docker") d =
  let resolved = Distro.resolve_alias d in
  let tag = Distro.tag_of_distro (resolved :> Distro.t) in
  DF.comment "Generated by `%s` -- %s project image (cmd: %s)." generator tag
    cmd
  @@ DF.comment "Build: docker build -t my-project ."
  @@ distro_stage ~overlay_depexts d
  @@ DF.workdir "/src"
  @@ DF.copy ~src:[ "." ] ~dst:"/src" ()
  @@ DF.run "%s" cmd

(* Single-distro Dockerfile: one distro stage that curls the [oi] binary
   from the latest GitHub release. The [docker-compose.yml] supplies a
   bind mount at /out and a [command:] override that runs the
   build+export. *)
let dockerfile_one_distro ?(overlay_depexts = []) d =
  let resolved = Distro.resolve_alias d in
  let tag = Distro.tag_of_distro (resolved :> Distro.t) in
  DF.comment "Generated by `oi docker --all` -- %s registry build image." tag
  @@ DF.comment
       "Usage: docker compose up  (mounts ./registry on /out, then runs oi \
        build --all --export /out)."
  @@ distro_stage ~overlay_depexts d

(* Filename (without directory) for the per-distro Dockerfile. *)
let one_distro_filename d =
  let resolved = Distro.resolve_alias d in
  "Dockerfile." ^ Distro.tag_of_distro (resolved :> Distro.t)

(* Short name used for the docker-compose service and for the image tag. *)
let service_name d =
  let resolved = Distro.resolve_alias d in
  Distro.tag_of_distro (resolved :> Distro.t)

(* -- docker-compose.yml emitter ----------------------------------------- *)

(* Compose v2 is the current syntax; the top-level [version] key is obsolete
   but we keep a comment so old tooling doesn't choke. *)

(* Shell one-liner that a distro service runs. Each container owns its
   own oi state: [oi] auto-clones the reporepo from its configured
   default URL (or [$OI_REPOREPO_URL]) into the container's own
   XDG data dir on first use, and [--refresh] keeps it current. The
   single-shot [oi build --all --export /out] iterates every overlay's
   [x-root-packages] and publishes the cache to the bind-mounted
   registry tree.

   [--use-registry=archives] disables the layer fetch so every package
   is built from source inside this container — otherwise the prior
   registry's cached binaries would short-circuit most of the work and
   the produced manifest would be all "cached" outcomes. The point of
   this flow is to re-validate every layer end-to-end. The d10ir source
   archives still flow from the configured [--registry] URL (default
   [https://oi.thicket.dev]) since the container starts with an empty
   cache; without that the very first build would have nothing to
   unpack and would [Error.config_error] on missing-archive.

   [OI_BUILD_PARALLELISM=$(nproc)] overrides the [min cpu_count 8] cap
   that {!D10ir.Config.default} applies for macOS fd-limit safety. Inside
   a Linux container with a high [nofile] ulimit (set on the service
   below) we want to use every core on the host. *)
let build_export_cmd () =
  "OI_BUILD_PARALLELISM=$(nproc) oi build --refresh --all \
   --use-registry=archives --export /out"

let docker_compose_yaml ~distros ~registry_host_path () =
  let buf = Buffer.create 1024 in
  let out s = Buffer.add_string buf s in
  out "# Generated by `oi docker --all`.\n";
  out "# Run with: docker compose up --build\n";
  out
    "# Each per-distro service runs `oi build --refresh --all --export /out`\n\
     # which clones the reporepo into the container's own state dir,\n\
     # iterates every overlay's x-root-packages, builds each package, and\n\
     # publishes per-os_key archives + the sqlite index + sources to the\n\
     # shared registry bind mount. Each os_key has only one producer\n\
     # container, so no cross-service merge is needed. Compose exits once\n\
     # every service has finished.\n\
     #\n\
     # Override the reporepo URL via `OI_REPOREPO_URL` in the service\n\
     # environment if you need something other than oi's built-in default.\n";
  out "services:\n";
  let cmd = build_export_cmd () in
  List.iter
    (fun d ->
      let name = service_name d in
      let file = one_distro_filename d in
      Printf.ksprintf out "  %s:\n" name;
      Printf.ksprintf out "    build:\n";
      Printf.ksprintf out "      context: .\n";
      Printf.ksprintf out "      dockerfile: %s\n" file;
      Printf.ksprintf out "    image: oi-registry-%s\n" name;
      Printf.ksprintf out "    command: [\"sh\", \"-c\", %s]\n"
        (Printf.sprintf "%S" cmd);
      (* Raise the per-container fd limit so high parallelism doesn't
         hit the kernel's default 1024 nofile cap. Each in-flight oi
         build holds capture pipes, compiler subprocess pipes, and
         transient fetch / patch handles — at [nproc] concurrent
         builds on a many-core host that easily climbs into the
         thousands. 65536 leaves plenty of headroom. *)
      Printf.ksprintf out "    ulimits:\n";
      Printf.ksprintf out "      nofile:\n";
      Printf.ksprintf out "        soft: 65536\n";
      Printf.ksprintf out "        hard: 65536\n";
      Printf.ksprintf out "    volumes:\n";
      Printf.ksprintf out "      - %s:/out\n" registry_host_path)
    distros;
  Buffer.contents buf

(* -- Writers ------------------------------------------------------------ *)

let write_dockerfile path df =
  let oc = open_out path in
  let buf = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buf in
  DF.pp fmt df;
  Format.pp_print_flush fmt ();
  output_string oc (Buffer.contents buf);
  close_out oc

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc
