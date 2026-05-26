(* oracle corpus fixture
   category: 04_records
   title: functional_update_named
   complexity: 3
   min_ocaml: 4.08
   tags: records, functional_update
*)

type named = { name : string; mark : char }

let bump value =
  { value with name = "z" }

let answer = bump { name = "a"; mark = 'b' }
