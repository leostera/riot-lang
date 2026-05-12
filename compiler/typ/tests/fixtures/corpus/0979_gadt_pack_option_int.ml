(* oracle corpus fixture
   category: 14_schema_expansion
   title: gadt_pack_option_int
   complexity: 7
   min_ocaml: 4.08
   tags: schema, gadt, existential
*)

type 'a option =
  | Some of 'a
  | None

type packed = Pack : 'a * ('a -> 'a) -> packed

let run (Pack (x, f)) = Pack (f x, f)

let answer = run (Pack ((Some 0), fun x -> x))
