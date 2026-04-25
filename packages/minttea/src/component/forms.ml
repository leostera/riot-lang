open Std

let checkbox = fun ?(checked = false) label ->
  "[" ^ (
    if checked then
      "x"
    else " "
  ) ^ "] " ^ label
