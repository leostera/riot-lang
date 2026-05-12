(* One-dimensional bigarray operations. *)
open Bigarray

let v = Array1.create int c_layout 8

let () =
  for i = 0 to Array1.dim v - 1 do
    Array1.set v i (i * i)
  done;
  let acc = ref 0 in
  for i = 0 to Array1.dim v - 1 do
    acc := !acc + Array1.get v i
  done;
  Printf.printf "%d\n" !acc
