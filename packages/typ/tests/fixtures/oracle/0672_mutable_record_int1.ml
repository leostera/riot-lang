(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_int1
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : int }

let touch record =
  let before = record.value in
  record.value <- (2);
  (before, record.value)

let answer = touch { value = (1) }
