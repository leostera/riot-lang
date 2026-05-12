(* Continuation-passing style. *)
type tree =
  | Leaf of int
  | Node of tree * tree

let rec sum_cps t k =
  match t with
  | Leaf n -> k n
  | Node (l, r) ->
      sum_cps l (fun x ->
          sum_cps r (fun y ->
              k (x + y)))

let () =
  let t = Node (Leaf 1, Node (Leaf 2, Leaf 3)) in
  Printf.printf "%d\n" (sum_cps t Fun.id)
