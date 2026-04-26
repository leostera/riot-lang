open Std
open Colors

let main ~args:_ =
  let blue = `rgb (0, 0, 255) in
  let yellow = `rgb (255, 255, 0) in
  let midpoint = RGB.blend blue yellow ~mix:0.5 in
  println ("blue = " ^ to_string (blue :> color));
  println ("yellow = " ^ to_string (yellow :> color));
  println ("midpoint = " ^ to_string (midpoint :> color));
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
