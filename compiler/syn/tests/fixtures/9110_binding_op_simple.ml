(* TEST_BELOW *)

let ( let* ) = Result.and_then

let test x =
  let* y = Ok x in
  Ok (y + 1)
