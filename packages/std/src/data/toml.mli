type value =
  | String of string
  | Array of value list
  | Table of (string * value) list
  | Bool of bool

type error =
  | Invalid_path of { path : string }
  | File_read_error of { path : string; reason : string }
  | Parse_error of { position : int; context : string; reason : string }
  | Unterminated_string of { position : int }
  | Unterminated_array of { position : int }
  | Unexpected_char of { position : int; found : char; expected : string }

val error_to_string : error -> string
val parse_file : string -> (value, error) result
val get_string : value -> string option
val get_array : value -> value list option
val get_table : value -> (string * value) list option
