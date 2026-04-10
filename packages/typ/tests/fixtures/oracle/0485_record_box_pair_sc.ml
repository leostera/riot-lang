(* oracle corpus fixture
   category: 14_schema_expansion
   title: record_box_pair_sc
   complexity: 2
   min_ocaml: 4.08
   tags: schema, record, field
*)

type t = { value : string * char }

let get record = record.value

let answer = get { value = (("x", 'y')) }
