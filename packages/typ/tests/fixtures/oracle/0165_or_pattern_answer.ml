(* oracle corpus fixture
   category: 03_patterns
   title: or_pattern_answer
   complexity: 3
   min_ocaml: 4.08
   tags: patterns, or_pattern, variant
*)

type reply = Yes | No | Maybe

let classify value =
  match value with
  | Yes | No -> value
  | Maybe -> Maybe

let answer = classify (Yes)
