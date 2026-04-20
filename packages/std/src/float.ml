open Kernel

include Kernel.Float

let ( = ) = equal

let ( != ) left right =
  match equal left right with
  | true -> false
  | false -> true

let ( < ) left right =
  match compare left right with
  | -1 -> true
  | _ -> false

let ( > ) left right =
  match compare left right with
  | 1 -> true
  | _ -> false

let ( <= ) left right =
  match compare left right with
  | 1 -> false
  | _ -> true

let ( >= ) left right =
  match compare left right with
  | -1 -> false
  | _ -> true

let ( + ) = add

let ( - ) = sub

let ( * ) = mul

let ( / ) = div
