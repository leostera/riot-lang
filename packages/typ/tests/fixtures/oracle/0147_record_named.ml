(* oracle corpus fixture
   category: 03_patterns
   title: record_named
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, record, field
*)

type named = { name : string; mark : char }

let project value =
  match value with
  | { name; _ } -> name

let answer = project { name = "a"; mark = 'b' }
