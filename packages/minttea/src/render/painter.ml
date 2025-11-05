(** Painter - paints a Scene graph onto a Matrix *)

open Std

(** Paint a single text node onto the matrix *)
let paint_text_node matrix node =
  let Scene.{rect; content; clip; _} = node in
  
  match content with
  | Scene.TextNode {text; style} ->
      Log.debug "[PAINTER] Painting text node at rect %d,%d size %dx%d, text len=%d" 
        rect.Scene.x rect.y rect.width rect.height (String.length text);
      (* Determine actual paint bounds (apply clipping if specified) *)
      let paint_x, paint_y, paint_w, paint_h =
        match clip with
        | Some clip_rect ->
            (* Calculate intersection of node rect and clip rect *)
            let x1 = Int.max rect.Scene.x clip_rect.Scene.x in
            let y1 = Int.max rect.y clip_rect.y in
            let x2 = Int.min (rect.x + rect.width) (clip_rect.x + clip_rect.width) in
            let y2 = Int.min (rect.y + rect.height) (clip_rect.y + clip_rect.height) in
            let w = Int.max 0 (x2 - x1) in
            let h = Int.max 0 (y2 - y1) in
            (x1, y1, w, h)
        | None ->
            (rect.x, rect.y, rect.width, rect.height)
      in
      
      (* Skip if nothing to paint *)
      if paint_w <= 0 || paint_h <= 0 then ()
      else begin
        (* Create a cell with the style *)
        let cell = {
          Matrix.char = " ";  (* Will be replaced per character *)
          fg = style.Scene.fg;
          bg = style.bg;
          bold = style.bold;
          italic = style.italic;
          underline = style.underline;
          strikethrough = style.strikethrough;
          reverse = style.reverse;
        } in
        
        (* Paint each character of the text *)
        let text_len = String.length text in
        let row = ref 0 in
        let col = ref 0 in
        let i = ref 0 in
        
        while !i < text_len && !row < rect.height do
          let ch = String.get text !i in
          
          (* Handle newline *)
          if ch = '\n' then begin
            row := !row + 1;
            col := 0;
          end else begin
            (* Calculate absolute position *)
            let abs_x = rect.x + !col in
            let abs_y = rect.y + !row in
            
            (* Check if this position is within paint bounds *)
            if abs_x >= paint_x && abs_x < paint_x + paint_w &&
               abs_y >= paint_y && abs_y < paint_y + paint_h then begin
              (* Paint the character, preserving existing background if text has no bg *)
              let final_bg = match cell.bg with
                | None ->
                    (* Preserve existing background from matrix *)
                    (match Matrix.get matrix ~x:abs_x ~y:abs_y with
                    | Some existing -> existing.Matrix.bg
                    | None -> None)
                | Some _ as bg -> bg  (* Use text's background *)
              in
              let char_cell = { cell with char = String.make 1 ch; bg = final_bg } in
              Matrix.set matrix ~x:abs_x ~y:abs_y char_cell;
            end;
            
            col := !col + 1;
            
            (* Wrap if we exceed rect width *)
            if !col >= rect.width then begin
              row := !row + 1;
              col := 0;
            end;
          end;
          
          i := !i + 1;
        done;
      end
  | Scene.Container { style = Some style; _ } ->
      (* Paint container background if it has a style *)
      Log.debug "[PAINTER] Painting container at rect %d,%d size %dx%d" 
        rect.Scene.x rect.y rect.width rect.height;
      let paint_x, paint_y, paint_w, paint_h =
        match clip with
        | Some clip_rect ->
            let x1 = Int.max rect.Scene.x clip_rect.Scene.x in
            let y1 = Int.max rect.y clip_rect.y in
            let x2 = Int.min (rect.x + rect.width) (clip_rect.x + clip_rect.width) in
            let y2 = Int.min (rect.y + rect.height) (clip_rect.y + clip_rect.height) in
            let w = Int.max 0 (x2 - x1) in
            let h = Int.max 0 (y2 - y1) in
            (x1, y1, w, h)
        | None ->
            (rect.x, rect.y, rect.width, rect.height)
      in
      
      (* Paint background for entire container area *)
      if paint_w > 0 && paint_h > 0 then begin
        let cell = {
          Matrix.char = " ";
          fg = style.Scene.fg;
          bg = style.bg;
          bold = style.bold;
          italic = style.italic;
          underline = style.underline;
          strikethrough = style.strikethrough;
          reverse = style.reverse;
        } in
        
        for y = paint_y to paint_y + paint_h - 1 do
          for x = paint_x to paint_x + paint_w - 1 do
            Matrix.set matrix ~x ~y cell;
          done;
        done;
      end
  | Scene.Container { style = None; _ } ->
      (* Containers without style don't paint anything *)
      ()

(** Paint a scene graph onto a matrix *)
let paint ~matrix ~scene =
  (* Scene should already be flattened and sorted by z-index *)
  List.iter (fun node -> paint_text_node matrix node) scene
