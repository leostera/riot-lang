(* oracle corpus fixture
   category: 01_basics
   title: fst_bool_int
   complexity: 1
   min_ocaml: 4.08
   tags: basics, tuple, pattern, function
*)

let fst (left, _right) = left

let answer = fst (false, 1)
