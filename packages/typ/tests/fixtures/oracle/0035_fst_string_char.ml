(* oracle corpus fixture
   category: 01_basics
   title: fst_string_char
   complexity: 1
   min_ocaml: 4.08
   tags: basics, tuple, pattern, function
*)

let fst (left, _right) = left

let answer = fst ("x", 'y')
