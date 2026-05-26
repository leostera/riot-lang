(* oracle corpus fixture
   category: 14_schema_expansion
   title: array_box_int1
   complexity: 4
   min_ocaml: 4.08
   tags: schema, array, mutation
*)

let touch array =
  let before = array.(0) in
  array.(0) <- (2);
  (before, array.(0))

let answer = touch [| (1); (2) |]
