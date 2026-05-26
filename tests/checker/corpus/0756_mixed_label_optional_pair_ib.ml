(* oracle corpus fixture
   category: 14_schema_expansion
   title: mixed_label_optional_pair_ib
   complexity: 5
   min_ocaml: 4.08
   tags: schema, labeled_args, optional_args
*)

let build ~left ?(right = ((0, true))) () = (left, right)

let answer = build ~left:((1, false)) ()
