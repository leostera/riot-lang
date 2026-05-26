(* oracle corpus fixture
   category: 04_records
   title: nested_record_outer_named
   complexity: 3
   min_ocaml: 4.08
   tags: records, nested, fields
*)

type inner = { name : string; mark : char }
type wrapper = { inner : inner; keep : bool }

        let answer = { inner = { name = "a"; mark = 'b' }; keep = false }
