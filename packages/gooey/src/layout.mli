(** Internal layout algorithm 
    
    This module implements the core layout computation based on Clay-TUI's algorithm.
    It's an internal module not exposed in the public API.
*)

(** Internal layout node with computed values *)
(** Main layout computation function *)
type layout_node = {
  element : Element.t;
  style : Style.t;
  children : layout_node list;
  mutable computed_size : Viewport.t;
  mutable computed_position : Geometry.Point.t;
  mutable final_box : Geometry.Rect.t;
}
val compute : config:Config.t -> Element.t -> Render.command_list
