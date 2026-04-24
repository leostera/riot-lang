include Kernel.Int64

let ( = ) = equal

let ( != ) left right =
  match equal left right with
  | true -> false
  | false -> true

let ( < ) left right =
  match compare left right with
  | Order.LT -> true
  | Order.EQ
  | Order.GT -> false

let ( > ) left right =
  match compare left right with
  | Order.GT -> true
  | Order.LT
  | Order.EQ -> false

let ( <= ) left right =
  match compare left right with
  | Order.GT -> false
  | Order.LT
  | Order.EQ -> true

let ( >= ) left right =
  match compare left right with
  | Order.LT -> false
  | Order.EQ
  | Order.GT -> true

let ( + ) = add

let ( - ) = sub

let ( * ) = mul

let ( / ) = div

let ( mod ) = rem
