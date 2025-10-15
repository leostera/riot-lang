(* Simple OCaml program to test bytecode loading *)

let add x y = x + y

let () =
  let result = add 40 2 in
  print_int result;
  print_newline ()
