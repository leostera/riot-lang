(* oracle corpus fixture
   category: 14_schema_expansion
   title: array_box_string_typ
   complexity: 4
   min_ocaml: 4.08
   tags: schema, array, mutation
*)

let touch array =
  let before = array.(0) in
  array.(0) <- ("oracle");
  (before, array.(0))

let answer = touch [| ("typ"); ("oracle") |]
