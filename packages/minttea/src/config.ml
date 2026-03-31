type render_mode =
  Clear
  | Persist

type output_target =
  Stdout
  | Stderr

type t = {
  render_mode : render_mode;
  fps : int;
  output : output_target;
}

let make = fun ?(render_mode = Clear) ?(fps = 60) ?(output = Stdout) () -> {render_mode; fps; output}
