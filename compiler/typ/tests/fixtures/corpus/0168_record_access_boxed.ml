(* oracle corpus fixture
   category: 04_records
   title: record_access_boxed
   complexity: 2
   min_ocaml: 4.08
   tags: records, access, field
*)

type 'a box = { value : 'a }

let get value = value.value

let answer = get { value = 0 }
