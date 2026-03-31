open Global0

type t = Non_zero_int.t

let readable = 0b0001

let writable = 0b0010

let add = fun a b -> a lor b

let remove = fun a b -> Non_zero_int.make (a land lnot b)

let is_readable = fun t -> t land readable != 0

let is_writable = fun t -> t land writable != 0
