type t = {
  render_mode : [ `clear | `persist ];
  fps : int;
}

let make ?(render_mode = `clear) ?(fps = 60) () =
  { render_mode; fps }
