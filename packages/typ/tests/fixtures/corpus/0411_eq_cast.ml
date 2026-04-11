(* oracle corpus fixture
   category: 12_gadts
   title: eq_cast
   complexity: 7
   min_ocaml: 4.08
   tags: gadts, locally_abstract_types
*)

type (_, _) eq = Refl : ('a, 'a) eq

let cast : type a b. (a, b) eq -> a -> b =
  fun Refl x -> x

let answer = cast Refl 0
