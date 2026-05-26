(* oracle corpus fixture
   category: 14_schema_expansion
   title: mixed_label_optional_option_char
   complexity: 5
   min_ocaml: 4.08
   tags: schema, labeled_args, optional_args
*)

type 'a option =
  | Some of 'a
  | None

let build ~left ?(right = (Some 'x')) () = (left, right)

let answer = build ~left:(None) ()
