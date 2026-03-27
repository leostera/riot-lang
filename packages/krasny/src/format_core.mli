open Std

type format_error =
  | Cannot_build_cst of Syn.build_cst_error

val format_error_to_string : format_error -> string

val format : Syn.Parser.parse_result -> (string, format_error) result
