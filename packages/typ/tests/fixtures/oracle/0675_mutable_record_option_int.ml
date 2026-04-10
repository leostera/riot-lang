(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_option_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : int option }

let touch record =
  let before = record.value in
  record.value <- (None);
  (before, record.value)

let answer = touch { value = (Some 0) }
