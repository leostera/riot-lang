(* Hexadecimal integer literals *)
let x = 0x000A
let y = 0xFF
let z = 0xDEADBEEF
let w = 0x1_2_3_4

(* Hex in patterns *)
let classify c =
  match c with
  | 0x000A -> "LF"
  | 0x000D -> "CR"
  | 0x0020 -> "Space"
  | _ -> "Other"
