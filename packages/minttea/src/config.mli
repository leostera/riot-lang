(** Application configuration *)

type render_mode = Clear | Persist

type output_target = Stdout | Stderr

type t = {
  render_mode : render_mode;
  fps : int;
  output : output_target;
}
(** Configuration for the terminal application *)

val make :
  ?render_mode:render_mode ->
  ?fps:int ->
  ?output:output_target ->
  unit ->
  t
(** Create a configuration with optional parameters.
    
    Defaults:
    - render_mode: [`clear]
    - fps: 60
    - output: Stdout *)
