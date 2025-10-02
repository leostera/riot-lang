type value =
  | String of string
  | Array of value list
  | Table of (string * value) list
  | Bool of bool

type error =
  | Invalid_path of { path : string }
  | File_read_error of { path : string; reason : string }
  | Parse_error of { line_number : int; line : string; reason : string }
  | Empty_file of { path : string }

val error_to_string : error -> string
val parse_file : string -> (value, error) result
val find_value : string -> value -> value option
val get_string : value -> string option
val get_array : value -> value list option
val get_table : value -> (string * value) list option
