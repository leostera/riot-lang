open Std

type 'a option =
  | None
  | Some of 'a

let id : int -> int = fun value -> value

let packed = Some 1
