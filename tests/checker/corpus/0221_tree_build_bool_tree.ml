(* oracle corpus fixture
   category: 05_variants
   title: tree_build_bool_tree
   complexity: 3
   min_ocaml: 4.08
   tags: variants, tree, constructor
*)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

let pair x y = Node (Leaf x, Leaf y)

let answer = pair true false
