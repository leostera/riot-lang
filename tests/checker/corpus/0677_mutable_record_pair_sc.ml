(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_pair_sc
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : string * char }

let touch record =
  let before = record.value in
  record.value <- (("z", 'w'));
  (before, record.value)

let answer = touch { value = (("x", 'y')) }
