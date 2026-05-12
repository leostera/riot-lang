(* oracle corpus fixture
   category: 04_records
   title: nested_record_pattern_outer_point
   complexity: 3
   min_ocaml: 4.08
   tags: records, nested, pattern
*)

type point = { x : int; y : int }
type wrapper = { point : point; flag : bool }

        let view value =
          match value with
          | { flag = keep; point = { x; y } } -> (x, y, keep)

        let answer = view { point = { x = 0; y = 1 }; flag = true }
