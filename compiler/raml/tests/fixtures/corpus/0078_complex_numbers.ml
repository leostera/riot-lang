(* Complex arithmetic. *)
let z1 = { Complex.re = 1.0; im = 2.0 }
let z2 = { Complex.re = 3.0; im = -4.0 }
let one = { Complex.re = 1.0; im = 0.0 }
let z = Complex.add (Complex.mul z1 z2) one

let () = Printf.printf "%.1f %.1f\n" z.re z.im
