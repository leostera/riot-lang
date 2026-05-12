(* oracle corpus fixture
   category: 03_patterns
   title: match_resultish_left
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, variant, match
*)

type ('a, 'b) resultish = Ok of 'a | Error of 'b

let unwrap value =
  match value with
  | Ok x -> x
  | Error _ -> 0.0

let answer = unwrap (Ok 0.0)
