(* oracle corpus fixture
   category: 07_labeled_optional
   title: optional_override_char
   complexity: 3
   min_ocaml: 4.08
   tags: optional_args, default, override
*)

let build ?(value = 'a') () = value

let answer = build ~value:'b' ()
