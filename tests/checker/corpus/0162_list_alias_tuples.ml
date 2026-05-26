(* oracle corpus fixture
   category: 03_patterns
   title: list_alias_tuples
   complexity: 3
   min_ocaml: 4.08
   tags: patterns, list, alias, builtin_list
*)

let view xs =
  match xs with
  | ((head :: _tail) as whole) -> (head, whole)
  | [] -> ((0, true), [])

let answer = view [(0, true); (1, false)]
