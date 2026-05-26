(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_char_z
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : char }

let touch record =
  let before = record.value in
  record.value <- ('y');
  (before, record.value)

let answer = touch { value = ('z') }
