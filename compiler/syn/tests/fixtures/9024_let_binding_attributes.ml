(* Test: let binding attributes like [@inline], [@tailcall] *)

let f x = x + 1

let rec loop n =
  if n = 0 then
    ()
  else
    loop (n - 1)

let g x = x * 2
