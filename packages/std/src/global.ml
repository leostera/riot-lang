(** Panic exception and function *)

exception Deprecated

let failwith = Deprecated

let panic msg =
  let exception Panic of string in
  raise (Panic msg)

(** Create a mutable cell *)
let cell x = Cell.create x
