(* oracle corpus fixture
   category: 14_schema_expansion
   title: gadt_pack_option_char
   complexity: 7
   min_ocaml: 4.08
   tags: schema, gadt, existential
*)

type 'a option =
  | Some of 'a
  | None

type packed = Pack : 'a * ('a -> 'a) -> packed

let run (Pack (x, f)) = Pack (f x, f)

let answer = run (Pack ((Some 'x'), fun x -> x))
