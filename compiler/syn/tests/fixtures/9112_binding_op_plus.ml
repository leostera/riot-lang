(* TEST_BELOW *)

let ( let+ ) = Result.map

let test x =
  let+ y = Ok x in
  y * 2
