(* oracle corpus fixture
   category: 12_gadts
   title: gadt_tree
   complexity: 7
   min_ocaml: 4.08
   tags: gadts, locally_abstract_types
*)

type _ tree =
  | Leaf : 'a -> 'a tree
  | Pair : 'a tree * 'b tree -> ('a * 'b) tree

let rec left_size : type a. a tree -> int = function
  | Leaf _ -> 1
  | Pair (left, _right) -> left_size left

let answer = left_size (Pair (Leaf 0, Pair (Leaf true, Leaf 'a')))
