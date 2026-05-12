(* Mutable hash table accumulation. *)
let frequencies words =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun w ->
      let count =
        match Hashtbl.find_opt tbl w with
        | None -> 0
        | Some n -> n
      in
      Hashtbl.replace tbl w (count + 1))
    words;
  tbl

let () =
  let tbl = frequencies [ "a"; "b"; "a"; "c"; "b"; "a" ] in
  let xs =
    Hashtbl.to_seq tbl
    |> List.from_seq
    |> List.sort compare
  in
  List.iter (fun (k, v) -> Printf.printf "%s:%d " k v) xs;
  print_newline ()
