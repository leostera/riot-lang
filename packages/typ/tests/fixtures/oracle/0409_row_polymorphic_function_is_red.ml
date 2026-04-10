(* oracle corpus fixture
   category: 11_polyvariants
   title: row_polymorphic_function_is_red
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, row_polymorphism, function
*)

let id = function
  | `Red -> `Red
  | other -> other

let answer = (id `Red, id `Blue)
