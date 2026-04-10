(* Multi-branch conditional logic. *)
let classify n =
  if n < 0 then "neg"
  else if n = 0 then "zero"
  else if n mod 2 = 0 then "even"
  else "odd"

let () =
  List.iter
    (fun n -> Printf.printf "%d:%s " n (classify n))
    [ -2; 0; 3; 4 ];
  print_newline ()
