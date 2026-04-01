let f = function
  | `Plain c -> `Plain c
  | `Gradient Style.(No_color, No_color) -> `Plain no_color
  | `Gradient Style.(No_color, c) -> `Plain c
  | `Gradient Style.(c, No_color) -> `Plain c
  | `Gradient (start, finish) -> `Gradient (start, finish)
