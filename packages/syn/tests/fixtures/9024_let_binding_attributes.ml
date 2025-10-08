(* Test: let binding attributes like [@inline], [@tailcall] *)
let[@inline] f x = x + 1
let[@tailcall] rec loop n = if n = 0 then () else loop (n - 1)
let[@inline][@specialise] g x = x * 2
