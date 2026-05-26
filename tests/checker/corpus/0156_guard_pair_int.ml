(* oracle corpus fixture
   category: 03_patterns
   title: guard_pair_int
   complexity: 3
   min_ocaml: 4.08
   tags: patterns, guard, tuple
*)

let choose pair =
  match pair with
  | (x, y) when true -> x
  | (_x, y) -> y

let answer = choose (0, 1)
