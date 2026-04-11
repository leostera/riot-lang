(* oracle corpus fixture
   category: 14_schema_expansion
   title: record_box_option_char
   complexity: 2
   min_ocaml: 4.08
   tags: schema, record, field
*)

type t = { value : char option }

let get record = record.value

let answer = get { value = (Some 'x') }
