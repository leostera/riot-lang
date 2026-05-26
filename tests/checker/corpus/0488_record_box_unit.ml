(* oracle corpus fixture
   category: 14_schema_expansion
   title: record_box_unit
   complexity: 2
   min_ocaml: 4.08
   tags: schema, record, field
*)

type t = { value : unit }

let get record = record.value

let answer = get { value = (()) }
