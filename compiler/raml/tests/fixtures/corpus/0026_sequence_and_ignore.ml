(* Sequencing and ignored results. *)
let step label x =
  print_string label;
  print_char ':';
  print_int x;
  print_char ' ';
  x + 1

let () =
  let n = step "a" 1 in
  let n = step "b" n in
  ignore (step "c" n);
  print_newline ()
