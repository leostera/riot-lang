(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_unit
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : unit }

let touch record =
  let before = record.value in
  record.value <- (());
  (before, record.value)

let answer = touch { value = (()) }
