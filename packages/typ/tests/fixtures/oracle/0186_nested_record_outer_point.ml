(* oracle corpus fixture
   category: 04_records
   title: nested_record_outer_point
   complexity: 3
   min_ocaml: 4.08
   tags: records, nested, fields
*)

type point = { x : int; y : int }
type wrapper = { point : point; flag : bool }

        let answer = { point = { x = 0; y = 1 }; flag = true }
