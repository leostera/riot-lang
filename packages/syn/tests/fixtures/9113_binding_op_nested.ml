(* TEST_BELOW *)

let ( let* ) = Result.and_then

let ( and* ) = Result.both

let ( let+ ) = Result.map

let complex x y z =
  let* a = Ok x in
  let* b = Ok y
  and* c = Ok z
  in
  let+ result = Ok (a + b + c) in
  result * 2
