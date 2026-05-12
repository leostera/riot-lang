(* Or-patterns and as-patterns. *)
let describe = function
  | ([] | [ _ ]) as xs -> Printf.sprintf "short:%d" (List.length xs)
  | (x :: _ as xs) -> Printf.sprintf "head=%d len=%d" x (List.length xs)

let () =
  List.iter
    (fun xs -> Printf.printf "%s\n" (describe xs))
    [ []; [ 9 ]; [ 1; 2; 3 ] ]
