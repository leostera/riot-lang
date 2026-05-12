(* Closures capturing lexical environment. *)
let make_adder base =
  let add x = base + x in
  add

let () =
  let add7 = make_adder 7 in
  Printf.printf "%d\n" (add7 35)
