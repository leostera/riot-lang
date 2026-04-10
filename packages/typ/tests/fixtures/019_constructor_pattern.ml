type color =
  RGB of int * int * int
  | No_color

type 'a option =
  | None
  | Some of 'a

let channel_sum = function
  | RGB (r, g, b) -> r + g + b
  | No_color -> 0

let unwrap = function
  | Some value -> value
  | None -> 0
