(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_bool_t
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : bool }

let touch record =
  let before = record.value in
  record.value <- (false);
  (before, record.value)

let answer = touch { value = (true) }
