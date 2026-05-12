(* Polymorphic variants. *)
type shape =
  [ `Circle of float
  | `Rect of float * float
  | `Point ]

let area = function
  | `Point -> 0.0
  | `Circle r -> Float.pi *. r *. r
  | `Rect (w, h) -> w *. h

let () =
  let shapes = [ `Point; `Circle 2.0; `Rect (3.0, 4.0) ] in
  List.iter (fun s -> Printf.printf "%.2f " (area s)) shapes;
  print_newline ()
