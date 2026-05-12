(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_float1
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : float }

let touch record =
  let before = record.value in
  record.value <- (2.5);
  (before, record.value)

let answer = touch { value = (1.5) }
