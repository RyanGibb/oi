(* Thin cmdliner-side re-export of [Oi.Build_request].

   The real logic lives in [lib/oi/build_request.ml] so library
   callers (the future [Build_pipeline.solve]) and the [oi]
   cmdliner layer share one target-classification surface. This
   wrapper just fills in the reporepo path/url from [Terms] (which
   reads them from environment overrides) so existing callers don't
   need to thread those values explicitly. *)

let log_overlay = Oi.Build_request.log_overlay

type build_target = Oi.Build_request.build_target =
  | Plain_target of string
  | Overlay_pkg of string * string
  | Overlay_all of string

let parse_build_target = Oi.Build_request.parse_build_target
let bare_handle = Oi.Build_request.bare_handle

type handle_pin = Oi.Build_request.handle_pin = {
  handle : string;
  pkg : OpamPackage.Name.t;
  user_constr : OpamFormula.version_constraint option;
}

let split_handle_prefix = Oi.Build_request.split_handle_prefix
let extract_handle_pins = Oi.Build_request.extract_handle_pins
let pin_handles = Oi.Build_request.pin_handles
let handle_pin_constraints = Oi.Build_request.handle_pin_constraints
let is_url_like = Oi.Build_request.is_url_like
let handles_of_tokens = Oi.Build_request.handles_of_tokens
let cli_extra_repo_of_url = Oi.Build_request.cli_extra_repo_of_url

let cli_extra_repos ~fs ~sys ?toolchain tokens =
  Oi.Build_request.cli_extra_repos ~fs ~sys
    ~reporepo_path:(Terms.reporepo_path ())
    ~reporepo_url:(Terms.reporepo_url ()) ?toolchain tokens

let merge_extras = Oi.Build_request.merge_extras
let parse_pkg_target = Oi.Build_request.parse_pkg_target
let latest_version_in_dirs = Oi.Build_request.latest_version_in_dirs
