(* Float array specific allocation path. *)
let a = Array.create_float 4

let () =
  for i = 0 to Array.length a - 1 do
    a.(i) <- float_of_int (i * i)
  done;
  let sum = Array.fold_left ( +. ) 0.0 a in
  Printf.printf "%.1f\n" sum
