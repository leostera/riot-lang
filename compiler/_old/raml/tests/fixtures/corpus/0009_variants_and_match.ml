(* Closed variants plus one exhaustive match. *)
type color =
  | Red
  | Green
  | Blue
  | Rgb of int * int * int

let luminance color =
  match color with
  | Red -> 76
  | Green -> 150
  | Blue -> 29
  | Rgb (r, g, b) -> (3 * r + 6 * g + b) / 10

let sample = Rgb (10, 20, 30)

let () = Printf.printf "%d\n" (luminance sample)
