(* oracle corpus fixture
   category: 14_schema_expansion
   title: mixed_label_optional_option_int
   complexity: 5
   min_ocaml: 4.08
   tags: schema, labeled_args, optional_args
*)

type 'a option =
  | Some of 'a
  | None

let build ~left ?(right = (Some 0)) () = (left, right)

let answer = build ~left:(None) ()
