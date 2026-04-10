(* oracle corpus fixture
   category: 03_patterns
   title: list_head_tail_ints
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, list, builtin_list
*)

let view xs =
  match xs with
  | head :: tail -> (head, tail)
  | [] -> (0, [])

let answer = view [0; 1; 2]
