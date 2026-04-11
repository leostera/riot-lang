(* oracle corpus fixture
   category: 14_schema_expansion
   title: mixed_label_optional_string_typ
   complexity: 5
   min_ocaml: 4.08
   tags: schema, labeled_args, optional_args
*)

let build ~left ?(right = ("typ")) () = (left, right)

let answer = build ~left:("oracle") ()
