(* oracle corpus fixture
   category: 07_labeled_optional
   title: mixed_override_char
   complexity: 4
   min_ocaml: 4.08
   tags: labeled_args, optional_args, mixed
*)

let build ~left ?(right = 'a') () = (left, right)

let answer = build ~left:'a' ~right:'b' ()
