(* oracle corpus fixture
   category: 03_patterns
   title: or_pattern_tri
   complexity: 3
   min_ocaml: 4.08
   tags: patterns, or_pattern, variant
*)

type tri = A | B | C

let classify value =
  match value with
  | A | B -> value
  | C -> C

let answer = classify (A)
