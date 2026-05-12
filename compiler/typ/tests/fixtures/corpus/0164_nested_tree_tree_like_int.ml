(* oracle corpus fixture
   category: 03_patterns
   title: nested_tree_tree_like_int
   complexity: 3
   min_ocaml: 4.08
   tags: patterns, nested, tree
*)

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let leftmost value =
  match value with
  | Node (Leaf x, _) -> x
  | Leaf x -> x
  | Node (_, Leaf y) -> y

let answer = leftmost (Node (Leaf 0, Leaf 1))
