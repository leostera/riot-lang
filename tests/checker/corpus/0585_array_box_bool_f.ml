(* oracle corpus fixture
   category: 14_schema_expansion
   title: array_box_bool_f
   complexity: 4
   min_ocaml: 4.08
   tags: schema, array, mutation
*)

let touch array =
  let before = array.(0) in
  array.(0) <- (true);
  (before, array.(0))

let answer = touch [| (false); (true) |]
