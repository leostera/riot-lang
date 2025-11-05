type render_mode = Clear | Persist

type t = {
  render_mode : render_mode;
  fps : int;
  initial_width : int;
  initial_height : int;
}

let make ?(render_mode = Clear) ?(fps = 30) ?(initial_width = 80) ?(initial_height = 24) () =
  { render_mode; fps; initial_width; initial_height }
