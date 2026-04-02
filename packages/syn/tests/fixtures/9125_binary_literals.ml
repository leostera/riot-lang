(* Binary integer literals *)

let flags = 0b1010

let mask = 0b1111_0000

let zero = 0b0000

(* Binary in patterns *)

let parse_bits b =
  match b with
  | 0b0000 -> "none"
  | 0b0001 -> "read"
  | 0b0010 -> "write"
  | 0b0100 -> "execute"
  | _ -> "multiple"
