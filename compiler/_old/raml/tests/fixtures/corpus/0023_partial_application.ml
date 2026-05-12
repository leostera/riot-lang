(* Partial application over curried functions. *)
let mul x y z = x * y * z

let double_then = mul 2
let six_times = double_then 3

let () = Printf.printf "%d\n" (six_times 7)
