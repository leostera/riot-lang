open Std
open Std.Collections

type color = Tty.Color.t

exception Invalid_gradient_color of color

let to_rgb = fun c ->
  match c with
  | Tty.Color.RGB (r, g, b) -> `rgb (r, g, b)
  | ANSI i
  | ANSI256 i -> Colors.ANSI.to_rgb (`ansi i)
  | No_color -> raise (Invalid_gradient_color c)

let make ~start ~finish ~steps : color array =
  let colors = Array.make ~count:steps ~value:start in
  let start = to_rgb start in
  let finish = to_rgb finish in
  for i = 0 to steps - 1 do
    let p =
      if steps = 1 then
        0.5
      else
        Float.from_int i /. Float.from_int (steps - 1)
    in
    let (`rgb (r, g, b)) = Colors.RGB.blend start finish ~mix:p in
    Array.set colors ~at:i ~value:(Tty.Color.from_rgb (r, g, b))
  done;
  colors
