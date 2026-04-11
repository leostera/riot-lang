(* oracle corpus fixture
   category: 04_records
   title: polymorphic_record_const_field
   complexity: 5
   min_ocaml: 4.08
   tags: records, polymorphism, polymorphic_field
*)

type poly = { f : 'a. 'a -> unit }
let value = { f = fun _ -> () }
let answer = (value.f 0, value.f 'a')
