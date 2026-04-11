(* oracle corpus fixture
   category: 07_labeled_optional
   title: mixed_labeled_optional_int
   complexity: 4
   min_ocaml: 4.08
   tags: labeled_args, optional_args, mixed
*)

let build ~left ?(right = 0) () = (left, right)

let answer = build ~left:1 ()
