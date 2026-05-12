(* Deterministic pseudo-random generation. *)
let state = Random.State.make [| 1; 2; 3; 4 |]

let () =
  List.init 5 (fun _ -> Random.State.int state 100)
  |> List.iter (fun x -> Printf.printf "%d " x);
  print_newline ()
