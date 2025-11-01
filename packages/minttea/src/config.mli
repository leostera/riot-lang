(** Application configuration *)

type t = {
  render_mode : [ `clear | `persist ];
  fps : int;
}
(** Configuration for the terminal application *)

val make :
  ?render_mode:[ `clear | `persist ] ->
  ?fps:int ->
  unit ->
  t
(** Create a configuration with optional parameters.
    
    Defaults:
    - render_mode: [`clear]
    - fps: 60 *)
