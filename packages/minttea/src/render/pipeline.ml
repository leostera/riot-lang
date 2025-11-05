(** Render - Pure rendering pipeline from Element to ANSI string *)

open Std

(** Pure function: Element + dimensions → ANSI string *)
let to_string element ~width ~height =
  (* Phase 1: Layout - convert Element tree to Scene graph *)
  let ctx = Layout.{ x = 0; y = 0; available_width = width; available_height = height } in
  let scene = Layout.to_scene element ctx in
  
  (* Phase 2: Flatten *)
  let flattened = Scene.flatten scene in
  
  (* Phase 3: Paint *)
  let matrix = Matrix.create ~width ~height in
  Painter.paint ~matrix ~scene:flattened;
  
  (* Phase 4: Emit *)
  Ansi_emitter.emit matrix
