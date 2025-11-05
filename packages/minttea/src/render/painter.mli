(** Painter - paints a Scene graph onto a Matrix *)

open Std

(** Paint a scene graph onto a matrix.
    
    This walks the flattened scene graph (sorted by z-index) and paints each
    node into the matrix at its specified position. Text nodes are painted
    character-by-character, respecting clipping boundaries if specified.
    
    @param matrix The target matrix to paint into
    @param scene The scene graph to paint
*)
val paint : matrix:Matrix.t -> scene:Scene.scene_node list -> unit
