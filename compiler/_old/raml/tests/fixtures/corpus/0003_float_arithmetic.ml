(* Floating-point arithmetic and sqrt. *)
let x = 3.0 +. 4.0 *. 0.5
let y = sqrt (x *. x +. 5.0)

let () = Printf.printf "%.4f %.4f\n" x y
