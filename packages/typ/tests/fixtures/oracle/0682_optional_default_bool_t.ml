(* oracle corpus fixture
   category: 14_schema_expansion
   title: optional_default_bool_t
   complexity: 4
   min_ocaml: 4.08
   tags: schema, optional_args
*)

let build ?(value = (true)) () = value

let answer = build ()
