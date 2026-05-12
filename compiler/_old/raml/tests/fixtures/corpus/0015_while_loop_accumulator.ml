(* While loop with refs. *)
let sum_to n =
  let i = ref 0 in
  let acc = ref 0 in
  while !i <= n do
    acc := !acc + !i;
    incr i
  done;
  !acc

let () = Printf.printf "%d\n" (sum_to 100)
