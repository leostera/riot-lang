open Std

type t =
  | Var of string
  | Const of Value.t
  | Wildcard

let compare t1 t2 =
  match (t1, t2) with
  | (Var a, Var b) -> String.compare a b
  | (Const a, Const b) -> Value.compare a b
  | (Wildcard, Wildcard) -> 0
  | (Var _, _) -> -1
  | (_, Var _) -> 1
  | (Const _, Wildcard) -> -1
  | (Wildcard, Const _) -> 1

let equal t1 t2 = compare t1 t2 = 0

let is_var = function Var _ -> true | _ -> false
let is_const = function Const _ -> true | _ -> false
let is_wildcard = function Wildcard -> true | _ -> false

let var_name = function Var name -> Some name | _ -> None
let const_value = function Const v -> Some v | _ -> None

let to_string = function
  | Var name -> name
  | Const v -> Value.to_string v
  | Wildcard -> "_"

let vars = function Var name -> [ name ] | _ -> []
