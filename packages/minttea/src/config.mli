(** Application configuration *)

type render_mode = Clear | Persist

type t = {
  render_mode : render_mode;
  fps : int;
  initial_width : int;
  initial_height : int;
}
(** Configuration for the terminal application *)

val make :
  ?render_mode:render_mode ->
  ?fps:int ->
  ?initial_width:int ->
  ?initial_height:int ->
  unit ->
  t
(** Create a configuration with optional parameters.
    
    Defaults:
    - render_mode: [`clear]
    - fps: 60
    - initial_width: 80
    - initial_height: 24 *)
