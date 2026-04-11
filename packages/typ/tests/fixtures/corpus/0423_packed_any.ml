(* oracle corpus fixture
   category: 12_gadts
   title: packed_any
   complexity: 7
   min_ocaml: 4.08
   tags: gadts, locally_abstract_types
*)

type any = Any : 'a -> any

let keep (Any value) = Any value

let answer = keep (Any 0)
