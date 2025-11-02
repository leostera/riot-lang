type render_mode = Clear | Persist

type t = {
  render_mode : render_mode;
  fps : int;
}

let make ?(render_mode = Clear) ?(fps = 30) () =
  { render_mode; fps }
