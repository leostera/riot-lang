(* oracle corpus fixture
   category: 14_schema_expansion
   title: optional_default_list_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, optional_args
*)

let build ?(value = ([0; 1])) () = value

let answer = build ()
