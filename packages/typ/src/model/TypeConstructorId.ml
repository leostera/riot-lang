open Std

type t = int

let compare = Int.compare

let equal = Int.equal

let of_int = fun value -> value

let to_int = fun value -> value

let to_string = fun type_constructor_id -> "type_constructor#" ^ Int.to_string type_constructor_id
