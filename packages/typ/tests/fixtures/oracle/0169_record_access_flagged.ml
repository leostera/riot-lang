(* oracle corpus fixture
   category: 04_records
   title: record_access_flagged
   complexity: 2
   min_ocaml: 4.08
   tags: records, access, field
*)

type flagged = { flag : bool; code : int }

let get value = value.flag

let answer = get { flag = true; code = 0 }
