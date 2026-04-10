(* oracle corpus fixture
   category: 14_schema_expansion
   title: optional_default_int1
   complexity: 4
   min_ocaml: 4.08
   tags: schema, optional_args
*)

let build ?(value = (1)) () = value

let answer = build ()
