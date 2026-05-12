(* oracle corpus fixture
   category: 05_variants
   title: tree_leaf_bool_tree
   complexity: 3
   min_ocaml: 4.08
   tags: variants, tree, recursion
*)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

let leftmost value =
  match value with
  | Leaf x -> x
  | Node (Leaf x, _) -> x
  | Node (Node (Leaf x, _), _) -> x
  | Node (_, Leaf y) -> y
  | Node (_, Node (Leaf y, _)) -> y

let answer = leftmost (Node (Leaf true, Leaf false))
