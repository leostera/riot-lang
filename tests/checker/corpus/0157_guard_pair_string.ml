(* oracle corpus fixture
   category: 03_patterns
   title: guard_pair_string
   complexity: 3
   min_ocaml: 4.08
   tags: patterns, guard, tuple
*)

let choose pair =
  match pair with
  | (x, y) when true -> x
  | (_x, y) -> y

let answer = choose ("a", "b")
