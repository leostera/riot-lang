(* oracle corpus fixture
   category: 11_polyvariants
   title: row_polymorphic_function_is_yes
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, row_polymorphism, function
*)

let id = function
  | `Yes -> `Yes
  | other -> other

let answer = (id `Yes, id `No)
