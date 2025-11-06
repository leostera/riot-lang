open Std

(* Debug: Test if box with flex layout fills the screen *)

let () =
  let module E = Minttea.Element in
  let module S = Minttea.Style in
  let module Layout = Minttea.Render.Layout in
  let module Scene = Minttea.Render.Scene in
  
  (* Create the same element as the blue box example *)
  let blue = S.color "#0000FF" in
  let elem = E.box 
    ~style:(S.default 
      |> S.width_flex 1.0 
      |> S.height_flex 1.0
      |> S.bg blue)
    (E.text "")
  in
  
  (* Simulate a 40x50 terminal *)
  let ctx = Layout.{x = 0; y = 0; available_width = 40; available_height = 50} in
  
  (* Convert to scene *)
  let scene = Layout.to_scene elem ctx in
  
  (* Print the scene rect *)
  eprintln "Scene rect: x=%d, y=%d, width=%d, height=%d"
    scene.rect.x scene.rect.y scene.rect.width scene.rect.height;
  
  (* Check the style *)
  match scene.content with
  | Scene.Container { style; children } ->
      eprintln "Container has style: %b" (Option.is_some style);
      (match style with
      | Some s ->
          eprintln "Background color: %s" 
            (match s.bg with Some _ -> "set" | None -> "none")
      | None -> ());
      eprintln "Container has %d children" (List.length children);
      
      (* Check child if exists *)
      if List.length children > 0 then (
        let child = List.hd children in
        eprintln "Child rect: x=%d, y=%d, width=%d, height=%d"
          child.rect.x child.rect.y child.rect.width child.rect.height
      );
      
      (* Now flatten and paint *)
      eprintln "\n--- Flattening scene ---";
      let flattened = Scene.flatten scene in
      eprintln "Flattened scene has %d nodes" (List.length flattened);
      
      (* Print each node *)
      List.iteri (fun i node ->
        eprintln "Node %d: rect=(%d,%d,%dx%d) z=%d"
          i node.Scene.rect.x node.rect.y node.rect.width node.rect.height node.z_index
      ) flattened;
      
      (* Create matrix and paint *)
      eprintln "\n--- Painting to matrix ---";
      let module Matrix = Minttea.Render.Matrix in
      let module Painter = Minttea.Render.Painter in
      let matrix = Matrix.create ~width:40 ~height:50 in
      Painter.paint ~matrix ~scene:flattened;
      
      (* Check how many cells have blue background *)
      let blue_count = ref 0 in
      for y = 0 to 49 do
        for x = 0 to 39 do
          match Matrix.get matrix ~x ~y with
          | Some cell ->
              if cell.Matrix.bg <> None then
                blue_count := !blue_count + 1
          | None -> ()
        done
      done;
      eprintln "Matrix has %d cells with background (expected 2000 for 40x50)" !blue_count;
      
      (* Test ANSI emission *)
      eprintln "\n--- Testing ANSI emission ---";
      let module Ansi = Minttea.Render.Ansi_emitter in
      
      (* Test with ContentFit *)
      let output_fit = Ansi.emit matrix ~mode:Ansi.ContentFit in
      let fit_lines = String.split_on_char '\n' output_fit in
      eprintln "ContentFit mode: %d lines" (List.length fit_lines);
      
      (* Test with Fullscreen *)
      let output_full = Ansi.emit matrix ~mode:Ansi.Fullscreen in
      let full_lines = String.split_on_char '\n' output_full in
      eprintln "Fullscreen mode: %d lines" (List.length full_lines)
      
  | _ -> eprintln "Not a container!"
