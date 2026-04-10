(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_string_a
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : string }

let touch record =
  let before = record.value in
  record.value <- ("b");
  (before, record.value)

let answer = touch { value = ("a") }
