(* oracle corpus fixture
   category: 04_records
   title: inline_record_point_inline
   complexity: 4
   min_ocaml: 4.03
   tags: records, inline_record, variant
*)

type t =
  | Point of { x: int; y: int }

let make () = Point { x = 0; y = 1 }

let view (Point { x; y }) = (x, y)

let answer = view (make ())
