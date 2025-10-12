type color = Red | Green | Blue

let color_to_string c =
  match c with Red -> "red" | Green -> "green" | Blue -> "blue"
