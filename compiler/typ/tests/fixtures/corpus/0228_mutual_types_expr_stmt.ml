(* oracle corpus fixture
   category: 05_variants
   title: mutual_types_expr_stmt
   complexity: 4
   min_ocaml: 4.08
   tags: variants, mutual_types
*)

type expr = Atom of int | Block of stmt
and stmt = Let of expr

        let answer = Atom 0
