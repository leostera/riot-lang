type value =
  | String of string
  | Array of value list
  | Table of (string * value) list
  | Bool of bool

val parse_file : string -> value
val find_value : string -> value -> value option
val get_string : value -> string option
val get_array : value -> value list option
val get_table : value -> (string * value) list option
