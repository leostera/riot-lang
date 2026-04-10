(* Persistent binary search tree. *)
type tree =
  | Empty
  | Node of tree * int * tree

let rec insert x = function
  | Empty -> Node (Empty, x, Empty)
  | Node (l, y, r) when x < y -> Node (insert x l, y, r)
  | Node (l, y, r) when x > y -> Node (l, y, insert x r)
  | tree -> tree

let rec inorder = function
  | Empty -> []
  | Node (l, x, r) -> inorder l @ (x :: inorder r)

let () =
  let t =
    List.fold_left (fun acc x -> insert x acc) Empty [ 5; 2; 8; 1; 3; 7 ]
  in
  inorder t |> List.iter (fun x -> Printf.printf "%d " x);
  print_newline ()
