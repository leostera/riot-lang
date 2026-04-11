type t =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | Text of string
  | Array of t list
  | Map of (string * t) list
