open Std

type t =
  | Int of int
  | String of string
  | Uri of string

let compare v1 v2 =
  match (v1, v2) with
  | (Int a, Int b) -> Int.compare a b
  | (String a, String b) -> String.compare a b
  | (Uri a, Uri b) -> String.compare a b
  | (Int _, _) -> -1
  | (_, Int _) -> 1
  | (String _, Uri _) -> -1
  | (Uri _, String _) -> 1

let equal v1 v2 = compare v1 v2 = 0

let to_string = function
  | Int i -> Int.to_string i
  | String s -> "\"" ^ s ^ "\""
  | Uri u -> u

let hash = function
  | Int i -> i land max_int
  | String s -> String.length s * 31
  | Uri u -> String.length u * 37
