(* Labelled and optional arguments. *)
let window ?(step = 1) ~start ~stop () =
  let rec loop acc x =
    if x > stop then List.rev acc
    else loop (x :: acc) (x + step)
  in
  loop [] start

let () =
  let xs = window ~start:2 ~stop:10 ~step:2 () in
  List.iter (fun x -> Printf.printf "%d " x) xs;
  print_newline ()
