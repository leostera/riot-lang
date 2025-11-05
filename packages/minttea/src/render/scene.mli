(** Scene Graph - Positioned UI elements ready for rendering *)

open Std

(** Rectangle representing position and size *)
type rect = {
  x : int;
  y : int;
  width : int;
  height : int;
}

(** Visual styling attributes *)
type style_attrs = {
  fg : Tty.Color.t option;
  bg : Tty.Color.t option;
  bold : bool;
  italic : bool;
  underline : bool;
  strikethrough : bool;
  reverse : bool;
}

val default_style : style_attrs

(** Scene node content *)
type scene_content =
  | TextNode of {
      text : string;
      style : style_attrs;
    }
  | Container of {
      children : scene_node list;
      style : style_attrs option;  (** Optional background/styling for container *)
    }

(** A node in the scene graph with position, z-index, and content *)
and scene_node = {
  rect : rect;
  z_index : int;
  clip : rect option;  (** Clipping bounds for overflow *)
  content : scene_content;
}

(** Create a text node *)
val text_node : rect:rect -> z_index:int -> style:style_attrs -> string -> scene_node

(** Create a container node *)
val container : rect:rect -> z_index:int -> ?style:style_attrs -> scene_node list -> scene_node

(** Sort nodes by z-index (lower first, so higher z-index paints on top) *)
val sort_by_z : scene_node list -> scene_node list

(** Get all leaf text nodes from a scene graph (flattened, z-sorted) *)
val flatten : scene_node -> scene_node list
