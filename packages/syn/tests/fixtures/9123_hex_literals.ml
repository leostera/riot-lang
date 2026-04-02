(* Hexadecimal integer literals *)

let x = 0x000a

let y = 0xff

let z = 0xdead_beef

let w = 0x1234

(* Hex in patterns *)

let classify c =
  match c with
  | 0x000a -> "LF"
  | 0x000d -> "CR"
  | 0x0020 -> "Space"
  | _ -> "Other"
