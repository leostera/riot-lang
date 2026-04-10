(* oracle corpus fixture
   category: 14_schema_expansion
   title: mixed_label_optional_float1
   complexity: 5
   min_ocaml: 4.08
   tags: schema, labeled_args, optional_args
*)

let build ~left ?(right = (1.5)) () = (left, right)

let answer = build ~left:(2.5) ()
