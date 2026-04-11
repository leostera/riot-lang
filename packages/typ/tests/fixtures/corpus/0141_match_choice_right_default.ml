(* oracle corpus fixture
   category: 03_patterns
   title: match_choice_right_default
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, variant, match
*)

type ('a, 'b) choice = This of 'a | That of 'b

let unwrap value =
  match value with
  | This x -> x
  | That _ -> ""

let answer = unwrap (That 'y')
