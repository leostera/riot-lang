(* Rank-1 polymorphism in records. *)
type mapper = { f : 'a. 'a list -> int }

let length_mapper = { f = List.length }

let () =
  Printf.printf "%d %d\n"
    (length_mapper.f [ 1; 2; 3 ])
    (length_mapper.f [ "a"; "b" ])
