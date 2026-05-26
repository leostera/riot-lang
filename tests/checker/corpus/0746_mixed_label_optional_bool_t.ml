(* oracle corpus fixture
   category: 14_schema_expansion
   title: mixed_label_optional_bool_t
   complexity: 5
   min_ocaml: 4.08
   tags: schema, labeled_args, optional_args
*)

let build ~left ?(right = (true)) () = (left, right)

let answer = build ~left:(false) ()
