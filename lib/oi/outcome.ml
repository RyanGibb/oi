type fetch_kind =
  | Http_status of int
  | Checksum_mismatch
  | Network_timeout
  | Git_failed
  | Other of string

type t =
  | Ok
  | Cached
  | Restored
  | Build_failed of { command : string; exit_code : int option }
  | Install_failed of { command : string; exit_code : int option }
  | Dep_failed of { upstream : Identity.dep }
  | Fetch_failed of { url : string; kind : fetch_kind }
  | Depext_missing of { missing : string list; not_found : string list }
  | Solve_failed of { reason : string }
  | Skipped of { reason : string }

type kind =
  | K_ok
  | K_cached
  | K_restored
  | K_build_failed
  | K_install_failed
  | K_dep_failed
  | K_fetch_failed
  | K_depext_missing
  | K_solve_failed
  | K_skipped

let kind_of = function
  | Ok -> K_ok
  | Cached -> K_cached
  | Restored -> K_restored
  | Build_failed _ -> K_build_failed
  | Install_failed _ -> K_install_failed
  | Dep_failed _ -> K_dep_failed
  | Fetch_failed _ -> K_fetch_failed
  | Depext_missing _ -> K_depext_missing
  | Solve_failed _ -> K_solve_failed
  | Skipped _ -> K_skipped

let string_of_kind = function
  | K_ok -> "ok"
  | K_cached -> "cached"
  | K_restored -> "restored"
  | K_build_failed -> "build_failed"
  | K_install_failed -> "install_failed"
  | K_dep_failed -> "dep_failed"
  | K_fetch_failed -> "fetch_failed"
  | K_depext_missing -> "depext_missing"
  | K_solve_failed -> "solve_failed"
  | K_skipped -> "skipped"

let pp ppf t = Fmt.string ppf (string_of_kind (kind_of t))

(* -- Histogram ----------------------------------------------------------- *)

let bump k = function
  | [] -> [ (k, 1) ]
  | xs ->
      let rec walk = function
        | [] -> [ (k, 1) ]
        | (k', c) :: rest when k' = k -> (k', c + 1) :: rest
        | head :: rest -> head :: walk rest
      in
      walk xs

let sort_histogram xs =
  List.sort
    (fun (k1, c1) (k2, c2) ->
      if c1 <> c2 then compare c2 c1
      else compare (string_of_kind k1) (string_of_kind k2))
    xs

(* -- Codecs -------------------------------------------------------------- *)

let kind_codec : kind Jsont.t =
  Jsont.enum ~kind:"outcome_kind"
    [
      ("ok", K_ok);
      ("cached", K_cached);
      ("restored", K_restored);
      ("build_failed", K_build_failed);
      ("install_failed", K_install_failed);
      ("dep_failed", K_dep_failed);
      ("fetch_failed", K_fetch_failed);
      ("depext_missing", K_depext_missing);
      ("solve_failed", K_solve_failed);
      ("skipped", K_skipped);
    ]

let fetch_kind_codec =
  let open Jsont in
  let case_http =
    Object.Case.map "http_status"
      (Object.map ~kind:"http_status" (fun code -> Http_status code)
      |> Object.mem "code" int ~enc:(function Http_status n -> n | _ -> 0)
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_checksum =
    Object.Case.map "checksum_mismatch"
      (Object.map ~kind:"checksum_mismatch" Checksum_mismatch |> Object.finish)
      ~dec:Fun.id
  in
  let case_timeout =
    Object.Case.map "network_timeout"
      (Object.map ~kind:"network_timeout" Network_timeout |> Object.finish)
      ~dec:Fun.id
  in
  let case_git =
    Object.Case.map "git_failed"
      (Object.map ~kind:"git_failed" Git_failed |> Object.finish)
      ~dec:Fun.id
  in
  let case_other =
    Object.Case.map "other"
      (Object.map ~kind:"other" (fun message -> Other message)
      |> Object.mem "message" string ~enc:(function Other s -> s | _ -> "")
      |> Object.finish)
      ~dec:Fun.id
  in
  let cases =
    [
      Object.Case.make case_http;
      Object.Case.make case_checksum;
      Object.Case.make case_timeout;
      Object.Case.make case_git;
      Object.Case.make case_other;
    ]
  in
  Object.map ~kind:"fetch_kind" Fun.id
  |> Object.case_mem "type" string cases ~enc:Fun.id ~enc_case:(fun k ->
      match k with
      | Http_status _ -> Object.Case.value case_http k
      | Checksum_mismatch -> Object.Case.value case_checksum k
      | Network_timeout -> Object.Case.value case_timeout k
      | Git_failed -> Object.Case.value case_git k
      | Other _ -> Object.Case.value case_other k)
  |> Object.finish

let codec =
  let open Jsont in
  let case_ok =
    Object.Case.map "ok" (Object.map ~kind:"ok" Ok |> Object.finish) ~dec:Fun.id
  in
  let case_cached =
    Object.Case.map "cached"
      (Object.map ~kind:"cached" Cached |> Object.finish)
      ~dec:Fun.id
  in
  let case_restored =
    Object.Case.map "restored"
      (Object.map ~kind:"restored" Restored |> Object.finish)
      ~dec:Fun.id
  in
  let case_skipped =
    Object.Case.map "skipped"
      (Object.map ~kind:"skipped" (fun reason -> Skipped { reason })
      |> Object.mem "reason" string ~enc:(function
        | Skipped { reason } -> reason
        | _ -> "")
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_build_failed =
    Object.Case.map "build_failed"
      (Object.map ~kind:"build_failed" (fun command exit_code ->
           Build_failed { command; exit_code })
      |> Object.mem "command" string ~enc:(function
        | Build_failed { command; _ } -> command
        | _ -> "")
      |> Object.opt_mem "exit_code" int ~enc:(function
        | Build_failed { exit_code; _ } -> exit_code
        | _ -> None)
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_install_failed =
    Object.Case.map "install_failed"
      (Object.map ~kind:"install_failed" (fun command exit_code ->
           Install_failed { command; exit_code })
      |> Object.mem "command" string ~enc:(function
        | Install_failed { command; _ } -> command
        | _ -> "")
      |> Object.opt_mem "exit_code" int ~enc:(function
        | Install_failed { exit_code; _ } -> exit_code
        | _ -> None)
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_dep_failed =
    Object.Case.map "dep_failed"
      (Object.map ~kind:"dep_failed" (fun upstream -> Dep_failed { upstream })
      |> Object.mem "upstream" Identity.dep_codec ~enc:(function
        | Dep_failed { upstream } -> upstream
        | _ -> { id = { name = ""; version = "" }; hash = "" })
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_fetch_failed =
    Object.Case.map "fetch_failed"
      (Object.map ~kind:"fetch_failed" (fun url kind ->
           Fetch_failed { url; kind })
      |> Object.mem "url" string ~enc:(function
        | Fetch_failed { url; _ } -> url
        | _ -> "")
      |> Object.mem "fetch_kind" fetch_kind_codec ~enc:(function
        | Fetch_failed { kind; _ } -> kind
        | _ -> Other "")
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_depext_missing =
    Object.Case.map "depext_missing"
      (Object.map ~kind:"depext_missing" (fun missing not_found ->
           Depext_missing { missing; not_found })
      |> Object.mem "missing" (list string) ~enc:(function
        | Depext_missing { missing; _ } -> missing
        | _ -> [])
      |> Object.mem "not_found" (list string) ~enc:(function
        | Depext_missing { not_found; _ } -> not_found
        | _ -> [])
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_solve_failed =
    Object.Case.map "solve_failed"
      (Object.map ~kind:"solve_failed" (fun reason -> Solve_failed { reason })
      |> Object.mem "reason" string ~enc:(function
        | Solve_failed { reason } -> reason
        | _ -> "")
      |> Object.finish)
      ~dec:Fun.id
  in
  let cases =
    [
      Object.Case.make case_ok;
      Object.Case.make case_cached;
      Object.Case.make case_restored;
      Object.Case.make case_skipped;
      Object.Case.make case_build_failed;
      Object.Case.make case_install_failed;
      Object.Case.make case_dep_failed;
      Object.Case.make case_fetch_failed;
      Object.Case.make case_depext_missing;
      Object.Case.make case_solve_failed;
    ]
  in
  Object.map ~kind:"outcome" Fun.id
  |> Object.case_mem "kind" string cases ~enc:Fun.id ~enc_case:(fun o ->
      match o with
      | Ok -> Object.Case.value case_ok o
      | Cached -> Object.Case.value case_cached o
      | Restored -> Object.Case.value case_restored o
      | Skipped _ -> Object.Case.value case_skipped o
      | Build_failed _ -> Object.Case.value case_build_failed o
      | Install_failed _ -> Object.Case.value case_install_failed o
      | Dep_failed _ -> Object.Case.value case_dep_failed o
      | Fetch_failed _ -> Object.Case.value case_fetch_failed o
      | Depext_missing _ -> Object.Case.value case_depext_missing o
      | Solve_failed _ -> Object.Case.value case_solve_failed o)
  |> Object.finish
