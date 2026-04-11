(* oracle corpus fixture
   category: 05_variants
   title: mutual_types_even_odd_nat
   complexity: 4
   min_ocaml: 4.08
   tags: variants, mutual_types
*)

type even = Zero | Succ of odd
and odd = Step of even

        let answer = Zero
