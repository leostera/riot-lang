(* oracle corpus fixture
   category: 03_patterns
   title: or_pattern_traffic
   complexity: 3
   min_ocaml: 4.08
   tags: patterns, or_pattern, variant
*)

type light = Red | Amber | Green

let classify value =
  match value with
  | Red | Amber -> value
  | Green -> Green

let answer = classify (Red)
