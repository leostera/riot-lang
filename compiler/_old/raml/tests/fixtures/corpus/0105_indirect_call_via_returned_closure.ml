(* Source-driven indirect call through a returned closure expression. *)
let make_adder base =
  let add x = base + x in
  add

let () = Printf.printf "%d\n" ((make_adder 7) 35)
