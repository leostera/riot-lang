(* oracle corpus fixture
   category: 04_records
   title: record_pattern_flagged
   complexity: 2
   min_ocaml: 4.08
   tags: records, pattern, field
*)

type flagged = { flag : bool; code : int }

let get value =
  let { flag; _ } = value in
  flag

let answer = get { flag = true; code = 0 }
