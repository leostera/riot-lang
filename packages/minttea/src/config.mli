(** Application configuration *)
type render_mode =
  | Clear
  | Persist
(** Configuration for the terminal application *)
type output_target =
  | Stdout
  | Stderr
(** Create a configuration with optional parameters.
    
    Defaults:
    - render_mode: [`clear]
    - fps: 60
    - output: Stdout *)
type t = {
  render_mode : render_mode;
  fps : int;
  output : output_target;
}
val make : ?render_mode:render_mode -> ?fps:int -> ?output:output_target -> unit -> t
