[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** DAG-driven parallel build executor. *)

let log_src = Logs.Src.create "oi.execute"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat
let fetch_log_path_of ~cache_root ~(p : Plan.package_plan) =
  Cache.Logs.path ~cache_root ~kind:"fetch" ~name:p.pkg ~hash:p.layer_hash

(* Render a [Source.Mirror.origin] for the "Fetching X from Y" log line.
   We can only tell for sure when the local mirror has the blob (a free
   filesystem check); for anything else opam's [pull_tree] resolves the
   source via [cache_urls] and we fall back to logging the package's
   canonical URL. *)
let describe_origin ~src_url : Source.Mirror.origin -> string = function
  | Local_mirror path -> Fmt.str "local mirror (%s)" path
  | Other -> src_url

let fetch_source ?(cache_urls = []) ~fs ~cache_root (p : Plan.package_plan) =
  match p.source with
  | None -> ()
  | Some src ->
      if not (Sys.file_exists p.build_dir) then begin
        let url = OpamUrl.parse ~handle_suffix:true src.url in
        let checksums = List.map OpamHash.of_string src.checksums in
        let dst_dir = OpamFilename.Dir.of_string p.build_dir in
        let cache_dir =
          OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
        in
        let origin = Source.Mirror.source_origin ~cache_urls ~checksums in
        Log.info (fun m ->
            m "Fetching %s from %s" p.pkg
              (describe_origin ~src_url:src.url origin));
        let error_log_path = fetch_log_path_of ~cache_root ~p in
        Cache.Logs.ensure ~fs ~cache_root;
        try
          Retry.with_attempts ~label:(Fmt.str "fetch %s (%s)" p.pkg src.url)
            ~error_log_path (fun () ->
              let result =
                OpamRepository.pull_tree p.pkg ~cache_dir ~cache_urls dst_dir
                  checksums [ url ]
                |> OpamProcess.Job.run
              in
              match result with
              | OpamTypes.Result _ | OpamTypes.Up_to_date _ ->
                  (* Promote the just-fetched blob into our content-
                     addressed mirror so [oi build --export] picks it up
                     and registry consumers can fetch it offline. *)
                  ignore
                    (Source.Mirror.import_from_opam_cache ~fs ~cache_root
                       checksums)
              | OpamTypes.Not_available (_, msg) ->
                  Fmt.failwith "Failed to fetch %s: %s" p.pkg msg)
        with Failure msg ->
          (* Re-raise as a typed Fetch_failed so the build loop can
             classify the outcome distinctly from a Build_failed. *)
          Error.raise (Fetch_failed { url = src.url; msg })
      end

let fetch_extra_sources ?(cache_urls = []) ~fs ~cache_root
    (p : Plan.package_plan) =
  List.iter
    (fun (name, (src : Plan.source_info)) ->
      let dst = p.build_dir / name in
      if not (Sys.file_exists dst) then begin
        let url = OpamUrl.parse ~handle_suffix:true src.url in
        let checksums = List.map OpamHash.of_string src.checksums in
        let dst_file = OpamFilename.of_string dst in
        let cache_dir =
          OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
        in
        let origin = Source.Mirror.source_origin ~cache_urls ~checksums in
        Log.info (fun m ->
            m "Fetching extra source %s for %s from %s" name p.pkg
              (describe_origin ~src_url:src.url origin));
        let error_log_path = fetch_log_path_of ~cache_root ~p in
        Cache.Logs.ensure ~fs ~cache_root;
        try
          Retry.with_attempts
            ~label:(Fmt.str "fetch extra source %s (%s)" name src.url)
            ~error_log_path (fun () ->
              let result =
                OpamRepository.pull_file name ~cache_dir ~cache_urls
                  ~silent_hits:true dst_file checksums [ url ]
                |> OpamProcess.Job.run
              in
              match result with
              | OpamTypes.Result () | OpamTypes.Up_to_date () ->
                  ignore
                    (Source.Mirror.import_from_opam_cache ~fs ~cache_root
                       checksums)
              | OpamTypes.Not_available (_, msg) -> Fmt.failwith "%s" msg)
        with Failure msg ->
          (* Match the previous semantics: extra sources are
             best-effort, so a hard failure (after retries) downgrades
             to a warning rather than aborting the whole build. *)
          Log.warn (fun m -> m "Failed to fetch extra source %s: %s" name msg)
      end)
    p.extra_sources

(* -- Patches and substitutions -------------------------------------------- *)

(* Copy opam [extra-files:] from their location next to the opam file
   into the package's build dir, where patches and other build steps
   expect to find them. opam itself does this implicitly when the
   recipe has [extra-files:]; oi has to wire it up because we don't
   stage sources through opam. *)
let copy_extra_files (p : Plan.package_plan) =
  List.iter
    (fun (basename, src) ->
      let dst = p.build_dir / basename in
      if Sys.file_exists src && not (Sys.file_exists dst) then begin
        (* Use a portable read+write rather than [cp] so we don't
           depend on shell tools mid-build. Patches are small text
           files; [Eio.Path.load] / [Eio.Path.save] handle them. *)
        let ic = open_in_bin src in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let len = in_channel_length ic in
            let bytes = really_input_string ic len in
            let oc = open_out_bin dst in
            Fun.protect
              ~finally:(fun () -> close_out_noerr oc)
              (fun () -> output_string oc bytes))
      end)
    p.extra_files

(* Apply patches via [OpamFilename.patch] — opam's pure-OCaml patch
   implementation. Avoids the GNU vs BSD [patch(1)] divergence on
   different hosts (BSD patch on macOS produces subtly different
   conflict-marker output than GNU on Linux) so consolidated source
   archives are byte-identical regardless of where they're baked.

   [allow_unclean:true] mirrors GNU patch's default behaviour: hunks
   that match with offset/fuzz still apply. Many opam patches in the
   wild rely on this (e.g. relocatable.patch carries small offsets
   between OCaml versions). Switching to [false] would reject every
   such patch and break re-bumps on packages that were applying fine
   with [patch(1)]. *)
let apply_patches (p : Plan.package_plan) =
  let dir = OpamFilename.Dir.of_string p.build_dir in
  List.iter
    (fun (patch : Plan.patch) ->
      let patch_file = p.build_dir / patch.file in
      if Sys.file_exists patch_file then
        let pf = OpamFilename.of_string patch_file in
        match
          OpamFilename.patch ~allow_unclean:true (`Patch_file pf) dir
        with
        | Ok _ -> ()
        | Error exn ->
            Error.build_failed ~pkg:p.pkg
              ~cmd:(Fmt.str "patch -p1 -i %s" patch.file)
              ~output:(Printexc.to_string exn))
    p.patches

let fetch_phase ?(cache_urls = []) ~fs ~cache_root (p : Plan.package_plan) =
  fetch_source ~cache_urls ~fs ~cache_root p;
  (* Ensure build_dir exists before fetching extra-sources: pull_tree
     (in fetch_source) creates the directory, but packages with no main
     source (e.g. seq.base) still need the directory to exist so that
     extra-source files can be written into it. *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / p.build_dir);
  fetch_extra_sources ~cache_urls ~fs ~cache_root p
