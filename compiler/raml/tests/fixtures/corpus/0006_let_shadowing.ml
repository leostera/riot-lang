(* Nested let-bindings and shadowing. *)
let x = 10

let y =
  let x = x + 5 in
  let x = x * 2 in
  x - 3

let () = Printf.printf "%d %d\n" x y
