(* oracle corpus fixture
   category: 04_records
   title: record_pattern_named
   complexity: 2
   min_ocaml: 4.08
   tags: records, pattern, field
*)

type named = { name : string; mark : char }

let get value =
  let { name; _ } = value in
  name

let answer = get { name = "a"; mark = 'b' }
