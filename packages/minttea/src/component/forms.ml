open Std

let checkbox ?(checked = false) label =
  format "[%s] %s" (if checked then "x" else " ") label
