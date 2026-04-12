[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** OCaml-specific prefix environment.

    Environment variables for OCaml toolchain prefixes. Layer assembly is
    handled by {!D10.Prefix}. *)

val env_vars : prefix:string -> dune_cache_root:string -> (string * string) list
val envrc_content : prefix:string -> dune_cache_root:string -> string
val make_env : prefix:string -> dune_cache_root:string -> string array
