(* Define binding operators *)

let ( let* ) x f = Result.and_then f x

let ( and* ) x y = Result.both x y

let ( let+ ) x f = Result.map f x

let ( and+ ) x y = Result.both x y

(* Use binding operators *)

let test1 x =
  let* a = Ok x in
  let* b = Ok (a + 1) in
  Ok (a + b)

let test2 x y =
  let* a = Ok x
  and* b = Ok y
  in
  Ok (a + b)

let test3 x =
  let+ a = Ok x in
  a * 2
