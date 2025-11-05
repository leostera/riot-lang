(** Layout - Convert Element tree to positioned Scene graph *)

(** Layout context - tracks position and available space *)
type ctx = {
  x : int;
  y : int;
  available_width : int;
  available_height : int;
}

(** Convert an Element tree into a positioned Scene graph.
    This performs layout calculations and produces a scene ready for painting. *)
val to_scene : Element.t -> ctx -> Scene.scene_node
