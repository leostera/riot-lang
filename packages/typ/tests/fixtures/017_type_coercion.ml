type ansi = [
  `ansi of int
]

type rgb = [
  `rgb of int * int * int
]

type color = [
  ansi
  | rgb
]

let midpoint = `rgb (1, 2, 3)
let as_color = (midpoint :> color)
