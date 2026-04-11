(* oracle corpus fixture
   category: 14_schema_expansion
   title: array_box_char_a
   complexity: 4
   min_ocaml: 4.08
   tags: schema, array, mutation
*)

let touch array =
  let before = array.(0) in
  array.(0) <- ('b');
  (before, array.(0))

let answer = touch [| ('a'); ('b') |]
