(* Persistent sets via Set.Make. *)
module StringSet = Set.Make (String)

let () =
  let set =
    List.fold_left
      (fun acc x -> StringSet.add x acc)
      StringSet.empty
      [ "raml"; "ocaml"; "raml"; "backend" ]
  in
  StringSet.iter (fun x -> Printf.printf "%s " x) set;
  print_newline ()
