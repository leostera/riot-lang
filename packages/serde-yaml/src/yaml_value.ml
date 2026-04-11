type t =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string
  | Seq of t list
  | Map of (string * t) list
  | Tagged of string * t

let rec is_scalar = function
  | Null
  | Bool _
  | Int _
  | Float _
  | String _ -> true
  | Seq []
  | Map [] -> true
  | Tagged (_, value) -> is_scalar value
  | Seq _
  | Map _ -> false
