(* oracle corpus fixture
   category: 14_schema_expansion
   title: gadt_pack_string_a
   complexity: 7
   min_ocaml: 4.08
   tags: schema, gadt, existential
*)

type packed = Pack : 'a * ('a -> 'a) -> packed

let run (Pack (x, f)) = Pack (f x, f)

let answer = run (Pack (("a"), fun x -> x))
