(* Algebraic variants with payloads. *)
type color =
  | Red
  | Green
  | Blue
  | Rgb of int * int * int

let luminance = function
  | Red -> 76
  | Green -> 150
  | Blue -> 29
  | Rgb (r, g, b) -> (3 * r + 6 * g + b) / 10

let () =
  let samples = [ Red; Green; Blue; Rgb (10, 20, 30) ] in
  List.iter (fun c -> Printf.printf "%d " (luminance c)) samples;
  print_newline ()
