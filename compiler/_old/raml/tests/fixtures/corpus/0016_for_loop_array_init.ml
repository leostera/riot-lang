(* For loop and imperative array writes. *)
let a = Array.make 8 0

let () =
  for i = 0 to Array.length a - 1 do
    a.(i) <- i * i
  done;
  Array.iter (fun x -> Printf.printf "%d " x) a;
  print_newline ()
