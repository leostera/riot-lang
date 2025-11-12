open Std

let checkbox ?(checked = false) label =
  "[" ^ (if checked then "x" else " ") ^ "] " ^ label
