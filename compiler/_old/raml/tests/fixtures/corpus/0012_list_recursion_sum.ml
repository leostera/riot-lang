(* Structural recursion over lists. *)
let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs

let () = Printf.printf "%d\n" (sum [ 1; 2; 3; 4; 5 ])
