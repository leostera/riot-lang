(* oracle corpus fixture
   category: 14_schema_expansion
   title: optional_default_string_a
   complexity: 4
   min_ocaml: 4.08
   tags: schema, optional_args
*)

let build ?(value = ("a")) () = value

let answer = build ()
