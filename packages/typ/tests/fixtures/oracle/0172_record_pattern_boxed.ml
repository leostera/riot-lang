(* oracle corpus fixture
   category: 04_records
   title: record_pattern_boxed
   complexity: 2
   min_ocaml: 4.08
   tags: records, pattern, field
*)

type 'a box = { value : 'a }

let get value =
  let { value; _ } = value in
  value

let answer = get { value = 0 }
