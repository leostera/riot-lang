(* User-defined infix operators. *)
let ( ++ ) a b = a @ b
let ( ^^^ ) a b = "(" ^ a ^ "," ^ b ^ ")"

let () =
  let xs = [ 1; 2 ] ++ [ 3; 4 ] in
  List.iter (fun x -> Printf.printf "%d " x) xs;
  print_endline ("=> " ^ ("raml" ^^^ "ocaml"))
