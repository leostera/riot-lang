(** Render commands generated from layout 
    
    After computing layout, Gooey outputs a list of render commands.
    These are renderer-agnostic primitives that describe what to draw.
    
    Your application processes these commands to actually draw to the screen
    (using ANSI escape codes, ncurses, a GUI framework, etc.)
*)
open Std

(** Border width specification *)
type border_width = {
  left: int;
  right: int;
  top: int;
  bottom: int;
}
(** Rectangle render data *)
type rectangle_data = {
  color: Colors.rgb;
  corner_radius: Style.corner_radius;
}
(** Text render data *)
type text_data = {
  content: string;
  color: Colors.rgb;
  size: int;
  weight: Style.font_weight;
}
(** Border render data *)
type border_data = {
  width: border_width;
  color: Colors.rgb;
  corner_radius: Style.corner_radius;
}
(** Render command type *)
type command_type =
  | Rectangle of rectangle_data
  | Text of text_data
  | Border of border_data
  | ScissorStart of Geometry.Rect.t
  (** Start clipping region *)
  | ScissorEnd
  (** End clipping region *)
  | Custom of { data: string }
(** Custom render data *)
(** A single render command *)
type command = {
  bounding_box: Geometry.Rect.t;
  command_type: command_type;
  z_index: int;
}
(** List of render commands *)
type command_list = command list
