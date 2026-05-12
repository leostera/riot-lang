(* oracle corpus fixture
   category: 11_polyvariants
   title: row_polymorphic_function_is_dot
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, row_polymorphism, function
*)

let id = function
  | `Dot -> `Dot
  | other -> other

let answer = (id `Dot, id `Line)
