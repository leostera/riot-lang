(* oracle corpus fixture
   category: 04_records
   title: record_access_named
   complexity: 2
   min_ocaml: 4.08
   tags: records, access, field
*)

type named = { name : string; mark : char }

let get value = value.name

let answer = get { name = "a"; mark = 'b' }
