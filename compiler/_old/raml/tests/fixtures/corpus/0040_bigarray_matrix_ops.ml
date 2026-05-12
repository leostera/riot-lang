(* Two-dimensional bigarray operations. *)
open Bigarray

let a = Array2.create float64 c_layout 2 3

let () =
  for i = 0 to 1 do
    for j = 0 to 2 do
      Array2.set a i j (float_of_int (i * 10 + j))
    done
  done;
  let sum = ref 0.0 in
  for i = 0 to 1 do
    for j = 0 to 2 do
      sum := !sum +. Array2.get a i j
    done
  done;
  Printf.printf "%.1f\n" !sum
