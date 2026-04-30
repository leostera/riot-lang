(* oracle corpus fixture
   category: 04_records
   title: inline_record_payload_inline
   complexity: 4
   min_ocaml: 4.03
   tags: records, inline_record, variant
*)

type t =
  | Payload of { flag: bool; code: int }

let make () = Payload { flag = true; code = 0 }

let view (Payload { flag; code }) = (flag, code)

let answer = view (make ())
