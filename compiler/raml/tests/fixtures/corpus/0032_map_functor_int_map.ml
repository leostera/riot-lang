(* Persistent maps via Map.Make. *)
module IntMap = Map.Make (Int)

let add_count k map =
  let n =
    match IntMap.find_opt k map with
    | None -> 0
    | Some x -> x
  in
  IntMap.add k (n + 1) map

let counts xs =
  List.fold_left (fun acc x -> add_count x acc) IntMap.empty xs

let () =
  counts [ 3; 1; 3; 2; 1; 3 ]
  |> IntMap.iter (fun k v -> Printf.printf "%d:%d " k v);
  print_newline ()
