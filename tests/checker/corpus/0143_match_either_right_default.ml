(* oracle corpus fixture
   category: 03_patterns
   title: match_either_right_default
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, variant, match
*)

type ('a, 'b) either = Left of 'a | Right of 'b

let unwrap value =
  match value with
  | Left x -> x
  | Right _ -> 0

let answer = unwrap (Right true)
