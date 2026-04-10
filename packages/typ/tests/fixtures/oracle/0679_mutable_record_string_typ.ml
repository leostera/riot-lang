(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_string_typ
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : string }

let touch record =
  let before = record.value in
  record.value <- ("oracle");
  (before, record.value)

let answer = touch { value = ("typ") }
