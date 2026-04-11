open Std

type color = [
  `ansi of int
  | `rgb of int * int * int
]

let to_string = fun t ->
  match t with
  | `ansi i -> "ANSI(" ^ Int.to_string i ^ ")"
  | `rgb (r, g, b) -> "RGB(" ^ Int.to_string r ^ "," ^ Int.to_string g ^ "," ^ Int.to_string b ^ ")"

let blue = `rgb (0, 0, 255)
