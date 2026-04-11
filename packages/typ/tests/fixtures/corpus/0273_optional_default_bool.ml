(* oracle corpus fixture
   category: 07_labeled_optional
   title: optional_default_bool
   complexity: 3
   min_ocaml: 4.08
   tags: optional_args, default
*)

let build ?(value = true) () = value

let answer = build ()
