(* Multiple mutually recursive functions *)

let rec f x = g x + 1

and g y = h y * 2

and h z =
  if z > 0 then
    f (z - 1)
  else
    0
