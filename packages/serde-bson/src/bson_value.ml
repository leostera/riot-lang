type t =
  | Null
  | Bool of bool
  | Int32 of int32
  | Int64 of int64
  | Double of float
  | String of string
  | Array of t list
  | Document of (string * t) list

let rec is_document = function
  | Document _ -> true
  | _ -> false
