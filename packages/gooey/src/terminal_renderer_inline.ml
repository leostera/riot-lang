open Std
open Std.Collections
open Std.IO
open Sync
open Sync.Cell

(* Inline renderer with line-by-line output *)

let coords_to_cell x y =
  let col = int_of_float (x +. 0.5) in
  let row = int_of_float (y +. 0.5) in
  (col, row)

let rgb_to_color (`rgb (r, g, b)) = Tty.Color.of_rgb (r, g, b)

let format_with_fg color text =
  Ansi_formatter.format_string [Ansi_formatter.Foreground color] text

let format_with_bg color text =
  Ansi_formatter.format_string [Ansi_formatter.Background color] text

type cell = {
  mutable char: string;
  mutable fg_color: Tty.Color.t option;
  mutable bg_color: Tty.Color.t option;
}

let make_cell () = { char = " "; fg_color = None; bg_color = None }

let render_to_string commands =
  if commands = [] then "" else
  
  (* Find grid dimensions *)
  let max_row = Cell.create 0 in
  let max_col = Cell.create 0 in
  List.iter (fun cmd ->
    let box = cmd.Render.bounding_box in
    let col_end, row_end = coords_to_cell 
      (box.Geometry.Rect.x +. box.Geometry.Rect.width) 
      (box.Geometry.Rect.y +. box.Geometry.Rect.height) in
    Cell.set max_row (Int.max (Cell.get max_row) row_end);
    Cell.set max_col (Int.max (Cell.get max_col) col_end);
  ) commands;
  
  let height = Cell.get max_row in
  let width = Cell.get max_col in
  
  if height = 0 || width = 0 then "" else
  
  (* Create grid *)
  let grid = Array.init height (fun _ -> Array.init width (fun _ -> make_cell ())) in
  
  (* Fill grid from commands *)
  List.iter (fun cmd ->
    match cmd.Render.command_type with
    | Render.Rectangle { color; _ } ->
        let col_start, row_start = coords_to_cell cmd.bounding_box.x cmd.bounding_box.y in
        let col_end, row_end = coords_to_cell 
          (cmd.bounding_box.x +. cmd.bounding_box.width) 
          (cmd.bounding_box.y +. cmd.bounding_box.height) in
        let tty_color = rgb_to_color color in
        for row = row_start to Int.min (row_end - 1) (height - 1) do
          for col = col_start to Int.min (col_end - 1) (width - 1) do
            grid.(row).(col).char <- " ";
            grid.(row).(col).bg_color <- Some tty_color;
          done
        done
    
    | Render.Text { content; color; _ } ->
        let col, row = coords_to_cell cmd.bounding_box.x cmd.bounding_box.y in
        let tty_color = rgb_to_color color in
        let lines = String.split_on_char '\n' content in
        List.iteri (fun line_idx line ->
          let current_row = row + line_idx in
          if current_row < height then begin
            (* Iterate over graphemes (user-perceived characters), not bytes *)
            let graphemes = String.into_grapheme_iter line |> Std.Iter.Iterator.to_list in
            List.iteri (fun char_idx grapheme ->
              let current_col = col + char_idx in
              if current_col < width then begin
                grid.(current_row).(current_col).char <- Std.Unicode.Grapheme.to_string grapheme;
                grid.(current_row).(current_col).fg_color <- Some tty_color;
              end
            ) graphemes
          end
        ) lines
    
    | Render.Border { width = border_width; color; _ } ->
        let col_start, row_start = coords_to_cell cmd.bounding_box.x cmd.bounding_box.y in
        let col_end, row_end = coords_to_cell 
          (cmd.bounding_box.x +. cmd.bounding_box.width) 
          (cmd.bounding_box.y +. cmd.bounding_box.height) in
        let tty_color = rgb_to_color color in
        
        (* Top border *)
        if border_width.top > 0 && row_start < height then begin
          for col = col_start to Int.min (col_end - 1) (width - 1) do
            let ch = if col = col_start && border_width.left > 0 then "┌"
                    else if col = col_end - 1 && border_width.right > 0 then "┐"
                    else "─" in
            grid.(row_start).(col).char <- ch;
            grid.(row_start).(col).fg_color <- Some tty_color;
          done
        end;
        
        (* Bottom border *)
        if border_width.bottom > 0 && row_end - 1 < height then begin
          for col = col_start to Int.min (col_end - 1) (width - 1) do
            let ch = if col = col_start && border_width.left > 0 then "└"
                    else if col = col_end - 1 && border_width.right > 0 then "┘"
                    else "─" in
            grid.(row_end - 1).(col).char <- ch;
            grid.(row_end - 1).(col).fg_color <- Some tty_color;
          done
        end;
        
        (* Left and right borders *)
        for row = row_start + 1 to Int.min (row_end - 2) (height - 1) do
          if border_width.left > 0 && col_start < width then begin
            grid.(row).(col_start).char <- "│";
            grid.(row).(col_start).fg_color <- Some tty_color;
          end;
          if border_width.right > 0 && col_end - 1 < width then begin
            grid.(row).(col_end - 1).char <- "│";
            grid.(row).(col_end - 1).fg_color <- Some tty_color;
          end;
        done
    
    | Render.Custom _ -> 
        (* Custom commands are handled separately after grid rendering *)
        ()
    
    | _ -> ()
  ) commands;
  
  (* Collect Custom commands for post-processing *)
  let custom_commands = List.filter_map (fun cmd ->
    match cmd.Render.command_type with
    | Render.Custom { data } -> 
        let col, row = coords_to_cell cmd.bounding_box.x cmd.bounding_box.y in
        Some (row, col, data)
    | _ -> None
  ) commands in
  
  (* Convert grid to string line-by-line *)
  let buf = Buffer.create (height * width * 2) in
  for row = 0 to height - 1 do
    if row > 0 then Buffer.add_char buf '\n';
    
    (* Check if this row has a custom command *)
    let custom_at_row = List.find_opt (fun (r, _, _) -> r = row) custom_commands in
    
    match custom_at_row with
    | Some (_row, col, data) ->
        (* Render cells up to the custom position *)
        for c = 0 to col - 1 do
          if c < width then begin
            let cell = grid.(row).(c) in
            let styled_char = 
              match cell.fg_color, cell.bg_color with
              | Some fg, Some bg -> 
                  Ansi_formatter.format_string [Ansi_formatter.Foreground fg; Ansi_formatter.Background bg] cell.char
              | Some fg, None -> 
                  format_with_fg fg cell.char
              | None, Some bg ->
                  format_with_bg bg cell.char
              | None, None ->
                  cell.char
            in
            Buffer.add_string buf styled_char;
          end
        done;
        (* Insert the raw custom data *)
        Buffer.add_string buf data;
        (* Add erase to end of line *)
        Buffer.add_string buf (Tty.Escape_seq.erase_line_seq 0);
    | None ->
        (* Normal row - output cells in this row *)
        for col = 0 to width - 1 do
          let cell = grid.(row).(col) in
          let styled_char = 
            match cell.fg_color, cell.bg_color with
            | Some fg, Some bg -> 
                Ansi_formatter.format_string [Ansi_formatter.Foreground fg; Ansi_formatter.Background bg] cell.char
            | Some fg, None -> 
                format_with_fg fg cell.char
            | None, Some bg ->
                format_with_bg bg cell.char
            | None, None ->
                cell.char
          in
          Buffer.add_string buf styled_char;
        done;
        
        (* Add erase to end of line *)
        Buffer.add_string buf (Tty.Escape_seq.erase_line_seq 0);
  done;
  
  Buffer.contents buf

let render commands =
  print (render_to_string commands)
