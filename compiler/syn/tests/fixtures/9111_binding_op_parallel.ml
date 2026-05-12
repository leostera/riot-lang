(* TEST_BELOW *)

let ( let* ) = Result.and_then

let ( and* ) = Result.both

let test x y =
  let* a = Ok x
  and* b = Ok y
  in
  Ok (a + b)
