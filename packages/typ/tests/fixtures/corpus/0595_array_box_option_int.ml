(* oracle corpus fixture
   category: 14_schema_expansion
   title: array_box_option_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, array, mutation
*)

type 'a option =
  | Some of 'a
  | None

let touch array =
  let before = array.(0) in
  array.(0) <- (None);
  (before, array.(0))

let answer = touch [| (Some 0); (None) |]
