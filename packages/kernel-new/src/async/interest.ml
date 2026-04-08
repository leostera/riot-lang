open Prelude

type t = Non_zero_int.t

let readable = 0b0001

let writable = 0b0010

let add = fun left right -> left lor right

let remove = fun left right -> Non_zero_int.make (left land lnot right)

let is_readable = fun value -> value land readable != 0

let is_writable = fun value -> value land writable != 0
