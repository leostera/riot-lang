open Std
open Std.Collections

type format_error =
  | Cannot_build_cst of Syn.build_cst_error
  | Cannot_parse of Syn.Diagnostic.t Vector.t
  | Cannot_lower of string
val format_error_to_string: format_error -> string

val parse_source: filename:Path.t -> string -> Syn.Parser2.parse_result

val format: Syn.Parser2.parse_result -> (string, format_error) result

val format_source: filename:Path.t -> string -> (string, format_error) result

val format2: Syn.Parser2.parse_result -> (string, format_error) result
