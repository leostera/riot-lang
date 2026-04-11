(* oracle corpus fixture
   category: 14_schema_expansion
   title: optional_default_pair_sc
   complexity: 4
   min_ocaml: 4.08
   tags: schema, optional_args
*)

let build ?(value = (("x", 'y'))) () = value

let answer = build ()
