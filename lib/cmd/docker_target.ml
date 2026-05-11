let ( / ) = Filename.concat

module Distro = Dockerfile_opam.Distro

(* Slug a target token for filename use: strips '@' and replaces '/' with '-'.
   "dune" -> "dune"; "@avsm/karakeep" -> "avsm-karakeep"; "@oxcaml" -> "oxcaml". *)
let slug_of_target s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '@' -> ()
      | '/' -> Buffer.add_char buf '-'
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let bp_target_of_token (token : string) : Oi.Build_pipeline.target =
  match Target.parse_build_target token with
  | Plain_target s -> Oi.Build_pipeline.Plain s
  | Overlay_pkg (h, spec) -> Oi.Build_pipeline.Overlay_pkg { handle = h; spec }
  | Overlay_all h -> Oi.Build_pipeline.Overlay_all h

let handles_of_targets tokens =
  List.filter_map
    (fun t ->
      match Target.parse_build_target t with
      | Plain_target _ -> None
      | Overlay_pkg (h, _) | Overlay_all h -> Some h)
    tokens
  |> List.sort_uniq String.compare

(* Project the merged d10ir plan's unique archive shas, in topological-ish
   order (preserves the [merged.nodes] ordering, deduped first-wins). *)
let unique_archive_shas (plan : D10ir.Plan.t) =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun (n : D10ir.Plan.node) ->
      let s = n.archive.sha256 in
      if Hashtbl.mem seen s then None
      else begin
        Hashtbl.replace seen s ();
        Some s
      end)
    plan.nodes

(* Render the full Dockerfile. One stage for the distro base + oi binary,
   one stage that fetches every archive in one heredoc'd RUN under a BuildKit
   cache mount, then [oi build TARGET] which solves against the reporepo
   inside the container and consumes the prefetched archives.

   The earlier shape baked a [recipe.json] sidecar and ran [oi ir run /work]
   to replay it; that's gone because the prefetched archives + the reporepo
   give [oi build] everything it needs to solve and build directly. The
   image is no longer pinned to a frozen plan, so reporepo changes between
   archive bake and [docker build] are picked up — at the cost of paying
   the solve time on every build.

   [no_cache_mount=true] strips the [--mount=type=cache] directives and falls
   back to plain [mkdir -p] for the same target paths. Useful when the
   BuildKit cache mount lives on a different filesystem from the image
   overlay and downstream [cp -Rfl] hardlink passes (e.g. [Sysops.link_tree])
   fail with [EXDEV]. The trade-off is no cross-build cache reuse: every
   docker build re-downloads archives and rebuilds layers. *)
let render_dockerfile ~distro ~oi_version_default ~registry_default ~depexts
    ~shas ~target_label ~recipe_node_count ~no_cache_mount =
  let resolved = Distro.resolve_alias distro in
  let img, image_tag = Distro.base_distro_tag (resolved :> Distro.t) in
  let distro_label = Distro.tag_of_distro (resolved :> Distro.t) in
  let mgr = Distro.package_manager (resolved :> Distro.t) in
  let mgr =
    match mgr with
    | (`Apk | `Apt | `Yum) as m -> m
    | _ ->
        Oi.Error.config_error
          "oi docker: distro %s uses an unsupported package manager"
          (Distro.tag_of_distro (resolved :> Distro.t))
  in
  let base = Registry_docker.build_depexts mgr in
  let extras = Registry_docker.extra_depexts mgr in
  let combined =
    let s = if extras = "" then base else base ^ " " ^ extras in
    let words = String.split_on_char ' ' s |> List.filter (( <> ) "") in
    let extra_words =
      List.filter (fun p -> not (List.mem p words)) depexts
      |> List.sort_uniq String.compare
    in
    String.concat " " (words @ extra_words)
  in
  let install = Registry_docker.install_cmd mgr combined in
  let sha_block = String.concat "\n" shas in
  (* Pick the [RUN] prefix for the fetch and replay stages. With cache
     mounts we let BuildKit create the target dirs implicitly; without,
     we prepend a [mkdir -p] so the [cd]/oi steps don't crash on a
     missing path. The image-overlay write goes through anyway, so
     fetched archives + built layers persist across the two RUNs within
     the same build (just not across builds). *)
  let fetch_prefix =
    if no_cache_mount then "RUN mkdir -p /cache/d10ir/archives\nRUN "
    else
      "RUN \
       --mount=type=cache,target=/cache/d10ir/archives,id=oi-d10ir-archives,sharing=locked \
       \\\n"
  in
  let replay_prefix =
    if no_cache_mount then
      "RUN mkdir -p /cache/d10ir/archives /cache/layers && \\\n    "
    else
      "RUN \
       --mount=type=cache,target=/cache/d10ir/archives,id=oi-d10ir-archives \\\n\
      \    --mount=type=cache,target=/cache/layers,id=oi-layers \\\n\
      \    "
  in
  let cache_doc =
    if no_cache_mount then
      "#   RUN fetch-archives      -> downloaded into the image layer (no\n\
       #                              cache mount); rebuilds re-download.\n\
       #   RUN oi build            -> solves + builds into the image layer."
    else
      "#   RUN fetch-archives      -> ONE layer; busts only when the sha list \
       inside\n\
       #                              the heredoc changes. Archive bytes live \
       in a\n\
       #                              BuildKit cache mount, so unchanged shas \
       aren't\n\
       #                              re-downloaded.\n\
       #   RUN oi build            -> reuses the cache mount + d10 layer cache"
  in
  Fmt.str
    {|# syntax=docker/dockerfile:1.7
#
# Generated by `oi docker %s --distro=%s`.
# Target:    %s
# Distro:    %s
# Nodes:     %d (solver plan at generation time; container re-solves)
# Archives:  %d unique
#
# Cache layout (top to bottom = most-stable to most-volatile):
#   base + depexts          -> invalidates on distro/depext change
#   oi binary               -> invalidates on OI_VERSION change
%s

ARG OI_VERSION=%s
ARG OI_REGISTRY=%s
ARG OI_REPOREPO_URL=

# ---- 1. base distro + build depexts -----------------------------------------
FROM %s:%s AS base
ARG OI_VERSION
RUN %s

# ---- 2. install oi from the GitHub releases page ----------------------------
RUN set -eux; \
    arch=$(uname -m); \
    tag="${OI_VERSION}"; \
    if [ "$tag" = "latest" ]; then \
        tag=$(curl -fsSL https://api.github.com/repos/avsm/oi/releases/latest \
              | sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p' | head -n1); \
    fi; \
    curl -fsSL -o /usr/local/bin/oi \
        "https://github.com/avsm/oi/releases/download/${tag}/oi-linux-${arch}"; \
    chmod 0755 /usr/local/bin/oi; \
    /usr/local/bin/oi --version
ENV OI_CACHE_DIR=/cache OI_DATA_DIR=/state

# Pre-stamp the cache + data roots with the schema [oi --version] writes
# itself. [Stamp.ensure] (called at the start of every oi invocation)
# treats "stamp present + matching schema" as a no-op; without this, the
# cache mounts below would be populated by step 3 before [oi build]
# fires, and [Stamp.check] would mistake "items present + no stamp" for a
# pre-stamp install that needs sweeping.
RUN mkdir -p /cache /state && \
    printf 'schema %%d\nwritten_at 0\n' %d > /cache/.oi-stamp && \
    printf 'schema %%d\nwritten_at 0\n' %d > /state/.oi-stamp

# ---- 3. bulk archive fetch (one layer) --------------------------------------
FROM base AS build
ARG OI_REGISTRY
ARG OI_REPOREPO_URL
# oi's [Source.Reporepo.env_url] treats an empty [OI_REPOREPO_URL] as "use
# the built-in default", so leaving the ARG empty doesn't break the build.
ENV OI_REGISTRY=${OI_REGISTRY} OI_REPOREPO_URL=${OI_REPOREPO_URL}

%s<<'BASH'
set -eu
cd /cache/d10ir/archives
cat > /tmp/shas.txt <<'SHAS'
%s
SHAS
xargs -a /tmp/shas.txt -P8 -I{} sh -c '
    sha="$1"; f="${sha}.tar.zst"
    if [ -f "$f" ] && [ "$(sha256sum "$f" | cut -d" " -f1)" = "$sha" ]; then
        exit 0
    fi
    curl -fsSL --retry 3 -o "$f.tmp" "${OI_REGISTRY}/d10ir-archives/$f"
    echo "$sha  $f.tmp" | sha256sum -c -
    mv "$f.tmp" "$f"
' _ {}
BASH

# ---- 4. solve + build against the prefetched archives -----------------------
# [--use-registry=archives] disables the layer fetch so every package is built
# from source against the prefetched archive set. The solve consults the
# reporepo (cloned on first use) for opam metadata; the archives are
# content-addressed in [/cache/d10ir/archives/<sha>.tar.zst].
# [--dist=/dist] surfaces the [bin/], [sbin/], [share/] sub-trees of every
# root layer into [/dist] so the runtime stage below can [COPY --from=build].
%soi build --registry=${OI_REGISTRY} --use-registry=archives --dist=/dist %s

# ---- 5. runtime image ------------------------------------------------------
# Same distro + depexts as the build stage so any libs the binaries dlopen at
# runtime are present, then copy the gathered binaries and shared data into
# the FHS prefix. One [COPY] of [/dist/] -> [/usr/local/] preserves the
# [bin/], [sbin/], [share/] sub-trees and tolerates any of them being absent
# (a target that ships only binaries doesn't produce [/dist/share/]). Build
# artefacts under [/cache], [/state] are left behind in the [build] stage.
FROM base AS runtime
COPY --from=build /dist/ /usr/local/
|}
    target_label distro_label target_label distro_label recipe_node_count
    (List.length shas) cache_doc oi_version_default registry_default img
    image_tag install Oi.Stamp.cache_schema Oi.Stamp.data_schema fetch_prefix
    sha_block replay_prefix target_label

let solve_targets ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir ~session
    ~platform ~refresh targets =
  let conf =
    Oi.Pipeline.make_conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let bp_targets = List.map bp_target_of_token targets in
  let global_handles = handles_of_targets targets in
  let pipeline_env : Oi.Build_pipeline.env =
    {
      proc_mgr;
      fs;
      clock;
      sys;
      os_key;
      cache;
      data_dir;
      http_session = session;
    }
  in
  let req : Oi.Build_pipeline.request =
    {
      targets = bp_targets;
      with_repos = global_handles;
      pins = [];
      extra_repos = [];
      constraints = OpamPackage.Name.Map.empty;
      toolchain_override = None;
      toolchain = None;
      conf;
      local_packages_dir = None;
      project_root = None;
      (* The container starts empty, so the recipe must include every node
         regardless of whether the host has a cached layer for it. Without
         this, [Plan.of_solution] marks cached packages as [Binary] and
         [Recipe_emitter] prunes them from the d10ir plan — the generated
         Dockerfile would [oi ir run] a recipe with 0 nodes. *)
      force_source = true;
      refresh;
    }
  in
  Oi.Build_pipeline.solve pipeline_env req

(* Render a slim, source-independent Dockerfile that doesn't bake a recipe.
   At [docker build] time, [oi] itself solves the target against the registry
   and reporepo, fetches archives, and builds. The image is reproducible
   only insofar as [OI_VERSION], the reporepo URL, and the registry index
   are stable — every other input is resolved at build time. *)
let render_no_recipe_dockerfile ~distro ~oi_version_default ~registry_default
    ~target_label ~no_cache_mount =
  let resolved = Distro.resolve_alias distro in
  let img, image_tag = Distro.base_distro_tag (resolved :> Distro.t) in
  let distro_label = Distro.tag_of_distro (resolved :> Distro.t) in
  let mgr = Distro.package_manager (resolved :> Distro.t) in
  let mgr =
    match mgr with
    | (`Apk | `Apt | `Yum) as m -> m
    | _ ->
        Oi.Error.config_error
          "oi docker: distro %s uses an unsupported package manager"
          distro_label
  in
  let base = Registry_docker.build_depexts mgr in
  let extras = Registry_docker.extra_depexts mgr in
  let combined = if extras = "" then base else base ^ " " ^ extras in
  let install = Registry_docker.install_cmd mgr combined in
  let build_prefix =
    if no_cache_mount then "RUN "
    else
      "RUN --mount=type=cache,target=/cache,id=oi-cache-build,sharing=locked \\\n\
      \    "
  in
  let cache_doc =
    if no_cache_mount then
      "#   oi build TARGET         -> writes /cache into the image layer (no \
       cache\n\
       #                              mount); rebuilds re-fetch and re-solve."
    else
      "#   oi build TARGET         -> one cached layer per target. The \
       BuildKit cache\n\
       #                              mount keeps /cache across rebuilds, so a \
       second\n\
       #                              build with the same OI_VERSION and \
       TARGET hits\n\
       #                              local d10 layers and is mostly free."
  in
  Fmt.str
    {|# syntax=docker/dockerfile:1.7
#
# Generated by `oi docker --no-recipe %s --distro=%s`.
# Target:    %s
# Distro:    %s
# Mode:      source-independent — oi solves at docker-build time, fetches
#            archives from the registry, and builds. No recipe sidecar.
#
# Caching:
#   base + depexts          -> apt/dnf/apk layer; invalidates on distro change.
#   oi/oix install          -> invalidates on OI_VERSION change.
%s

ARG OI_VERSION=%s
ARG OI_REGISTRY=%s
ARG OI_REPOREPO_URL=

FROM %s:%s AS base
ARG OI_VERSION
RUN %s
RUN set -eux; \
    arch=$(uname -m); \
    tag="${OI_VERSION}"; \
    if [ "$tag" = "latest" ]; then \
        tag=$(curl -fsSL https://api.github.com/repos/avsm/oi/releases/latest \
              | sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p' | head -n1); \
    fi; \
    for bin in oi oix; do \
        curl -fsSL -o /usr/local/bin/$bin \
            "https://github.com/avsm/oi/releases/download/${tag}/$bin-linux-${arch}"; \
        chmod 0755 /usr/local/bin/$bin; \
    done; \
    /usr/local/bin/oi --version
ENV OI_CACHE_DIR=/cache OI_DATA_DIR=/state

# Pre-stamp cache + data roots so [Stamp.ensure] sees [Up_to_date] and
# doesn't try to "upgrade" (sweep) the BuildKit cache mount on first use.
RUN mkdir -p /cache /state && \
    printf 'schema %%d\nwritten_at 0\n' %d > /cache/.oi-stamp && \
    printf 'schema %%d\nwritten_at 0\n' %d > /state/.oi-stamp

# ---- solve + fetch + build ---------------------------------------------------
FROM base AS build
ARG OI_REGISTRY
ARG OI_REPOREPO_URL
# oi's [Source.Reporepo.env_url] treats an empty [OI_REPOREPO_URL] as "use
# the built-in default", so leaving the ARG empty doesn't break the build.
ENV OI_REGISTRY=${OI_REGISTRY} OI_REPOREPO_URL=${OI_REPOREPO_URL}
%soi build --registry=${OI_REGISTRY} %s

# Usage:
#   docker build -t %s -f %s .
#   docker run --rm %s oix %s --help
|}
    target_label distro_label target_label distro_label cache_doc
    oi_version_default registry_default img image_tag install
    Oi.Stamp.cache_schema Oi.Stamp.data_schema build_prefix target_label
    (Fmt.str "oi-%s"
       (String.concat "-" (List.map slug_of_target [ target_label ])))
    (Fmt.str "Dockerfile.oi-%s.%s"
       (String.concat "-" (List.map slug_of_target [ target_label ]))
       distro_label)
    (Fmt.str "oi-%s"
       (String.concat "-" (List.map slug_of_target [ target_label ])))
    target_label

let emit_no_recipe ~distro ~oi_version ~registry ~no_cache_mount ~output
    ~targets =
  let target_label = String.concat " " targets in
  let body =
    render_no_recipe_dockerfile ~distro ~oi_version_default:oi_version
      ~registry_default:registry ~target_label ~no_cache_mount
  in
  let slug = targets |> List.map slug_of_target |> String.concat "-" in
  let distro_tag =
    Distro.tag_of_distro (Distro.resolve_alias distro :> Distro.t)
  in
  let dockerfile_name = Fmt.str "Dockerfile.oi-%s.%s" slug distro_tag in
  let cwd = Sys.getcwd () in
  let dockerfile_path =
    match output with
    | None -> cwd / dockerfile_name
    | Some p ->
        if Sys.file_exists p && Sys.is_directory p then p / dockerfile_name
        else p
  in
  Registry_docker.write_file dockerfile_path body;
  Oi.Say.step "Wrote";
  Oi.Say.info "%s" dockerfile_path;
  Oi.Say.newline ();
  Oi.Say.step "Build with";
  Oi.Say.info "docker build -t oi-%s -f %s %s" slug dockerfile_path
    (Filename.dirname dockerfile_path)

let emit ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir ~session ~platform
    ~refresh ~registry ~distro ~oi_version ~no_cache_mount ~output ~targets =
  if targets = [] then
    Oi.Error.config_error
      "oi docker: no target. Pass one or more PKG / @HANDLE / @HANDLE/PKG \
       tokens, or run without arguments for project mode.";
  let solved =
    solve_targets ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir ~session
      ~platform ~refresh targets
  in
  let merged =
    match solved.merged with
    | Some m -> m
    | None ->
        let msgs =
          List.filter_map
            (fun (gr : Oi.Build_pipeline.group_result) ->
              match gr.error with
              | Ok () -> None
              | Error e ->
                  let kind =
                    match e with
                    | Solve_failed { msg; _ } -> Fmt.str "solve: %s" msg
                    | Cycle _ -> "cycle"
                    | Empty_after_strip -> "empty"
                    | Elaborate_failed { msg } -> Fmt.str "elaborate: %s" msg
                    | Emit_failed { msg } -> Fmt.str "emit: %s" msg
                  in
                  Some (Fmt.str "%s — %s" gr.group.label kind))
            solved.groups
        in
        Oi.Error.config_error
          "oi docker: target produced no executable plan:@\n  %s"
          (String.concat "\n  " msgs)
  in
  let shas = unique_archive_shas merged in
  let target_label = String.concat " " targets in
  let dockerfile_body =
    render_dockerfile ~distro ~oi_version_default:oi_version
      ~registry_default:registry
      ~depexts:[] (* TODO target-scoped depexts; for now base set only *)
      ~shas ~target_label ~recipe_node_count:(List.length merged.nodes)
      ~no_cache_mount
  in
  let slug = targets |> List.map slug_of_target |> String.concat "-" in
  let distro_tag =
    Distro.tag_of_distro (Distro.resolve_alias distro :> Distro.t)
  in
  let dockerfile_name = Fmt.str "Dockerfile.oi-%s.%s" slug distro_tag in
  let cwd_s, _ = Workspace.resolved_cwd fs in
  let dst_dir, dockerfile_path =
    match output with
    | None -> (cwd_s, cwd_s / dockerfile_name)
    | Some p ->
        if Sys.file_exists p && Sys.is_directory p then (p, p / dockerfile_name)
        else (Filename.dirname p, p)
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dst_dir);
  Registry_docker.write_file dockerfile_path dockerfile_body;
  Oi.Say.step "Wrote";
  Oi.Say.info "%s  %a" dockerfile_path Oi.Style.dim_string
    (Fmt.str "(plan: %d nodes, %d unique archives)" (List.length merged.nodes)
       (List.length shas));
  Oi.Say.newline ();
  Oi.Say.step "Build with";
  Oi.Say.info "docker build -t oi-%s -f %s %s" slug dockerfile_path dst_dir
