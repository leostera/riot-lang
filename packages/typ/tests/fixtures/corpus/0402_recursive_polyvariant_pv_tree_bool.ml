(* oracle corpus fixture
   category: 11_polyvariants
   title: recursive_polyvariant_pv_tree_bool
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, recursive
*)

let rec map value =
  match value with
  | `Leaf x -> `Leaf (x, x)
  | `Node (left, right) -> `Node (map left, map right)

let answer = map (`Node (`Leaf true, `Leaf false))
