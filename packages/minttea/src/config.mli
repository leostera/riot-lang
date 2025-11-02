(** Application configuration *)

type render_mode = Clear | Persist

type t = {
  render_mode : render_mode;
  fps : int;
}
(** Configuration for the terminal application *)

val make :
  ?render_mode:render_mode ->
  ?fps:int ->
  unit ->
  t
(** Create a configuration with optional parameters.
    
    Defaults:
    - render_mode: [`clear]
    - fps: 60 *)
