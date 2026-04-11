(* oracle corpus fixture
   category: 14_schema_expansion
   title: mutable_record_list_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, record, mutable
*)

type t = { mutable value : int list }

let touch record =
  let before = record.value in
  record.value <- ([]);
  (before, record.value)

let answer = touch { value = ([0; 1]) }
