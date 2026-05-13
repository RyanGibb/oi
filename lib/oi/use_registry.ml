type t = All | Archives | Never

let err_str fmt = Fmt.kstr (fun s -> Error s) fmt

let to_string = function
  | All -> "all"
  | Archives -> "archives"
  | Never -> "never"

let of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "all" -> Ok All
  | "archives" -> Ok Archives
  | "never" -> Ok Never
  | other ->
      err_str "unknown --use-registry value %S (expected: all|archives|never)"
        other

let pp ppf t = Format.pp_print_string ppf (to_string t)
