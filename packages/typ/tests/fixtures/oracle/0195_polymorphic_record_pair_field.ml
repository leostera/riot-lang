(* oracle corpus fixture
   category: 04_records
   title: polymorphic_record_pair_field
   complexity: 5
   min_ocaml: 4.08
   tags: records, polymorphism, polymorphic_field
*)

type poly = { f : 'a. 'a -> 'a * 'a }
let value = { f = fun x -> (x, x) }
let answer = (value.f 0, value.f false)
