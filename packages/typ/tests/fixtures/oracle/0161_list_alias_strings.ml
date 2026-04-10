(* oracle corpus fixture
   category: 03_patterns
   title: list_alias_strings
   complexity: 3
   min_ocaml: 4.08
   tags: patterns, list, alias, builtin_list
*)

let view xs =
  match xs with
  | ((head :: _tail) as whole) -> (head, whole)
  | [] -> ("a", [])

let answer = view ["a"; "b"]
