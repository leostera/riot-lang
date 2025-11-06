open Std

(** Differential Rendering - Only render what changed
    
    This module compares the previous and current matrix states
    and generates optimal ANSI operations to update only the
    changed regions.
*)

type change_region = {
  x : int;
  y : int;
  width : int;
  height : int;
}

(** Find all regions that differ between two matrices *)
let find_changed_regions prev_matrix curr_matrix =
  let open Matrix in
  if prev_matrix.width <> curr_matrix.width || prev_matrix.height <> curr_matrix.height then
    (* Size changed - everything needs redraw *)
    [{ x = 0; y = 0; width = curr_matrix.width; height = curr_matrix.height }]
  else
    (* Find individual changed cells and group into regions *)
    let changes = ref [] in
    let visited = Array.make_matrix curr_matrix.height curr_matrix.width false in
    
    for y = 0 to curr_matrix.height - 1 do
      for x = 0 to curr_matrix.width - 1 do
        if not visited.(y).(x) then
          let prev_cell = prev_matrix.cells.(y).(x) in
          let curr_cell = curr_matrix.cells.(y).(x) in
          
          (* Check if cells differ *)
          if prev_cell <> curr_cell then begin
            (* Find extent of change region *)
            let min_x = ref x in
            let max_x = ref x in
            let min_y = ref y in
            let max_y = ref y in
            
            (* Expand region to include adjacent changes *)
            let rec expand () =
              let expanded = ref false in
              
              (* Try expanding right *)
              if !max_x < curr_matrix.width - 1 then
                for check_y = !min_y to !max_y do
                  let check_x = !max_x + 1 in
                  if not visited.(check_y).(check_x) &&
                     prev_matrix.cells.(check_y).(check_x) <> curr_matrix.cells.(check_y).(check_x) then begin
                    max_x := check_x;
                    expanded := true
                  end
                done;
              
              (* Try expanding down *)
              if !max_y < curr_matrix.height - 1 then
                for check_x = !min_x to !max_x do
                  let check_y = !max_y + 1 in
                  if not visited.(check_y).(check_x) &&
                     prev_matrix.cells.(check_y).(check_x) <> curr_matrix.cells.(check_y).(check_x) then begin
                    max_y := check_y;
                    expanded := true
                  end
                done;
              
              if !expanded then expand ()
            in
            expand ();
            
            (* Mark region as visited *)
            for mark_y = !min_y to !max_y do
              for mark_x = !min_x to !max_x do
                visited.(mark_y).(mark_x) <- true
              done
            done;
            
            (* Add region to changes *)
            changes := { 
              x = !min_x; 
              y = !min_y; 
              width = !max_x - !min_x + 1; 
              height = !max_y - !min_y + 1 
            } :: !changes
          end else
            visited.(y).(x) <- true
      done
    done;
    
    List.rev !changes

(** Render only the changed regions *)
let render_changes ~prev_matrix ~curr_matrix =
  let regions = find_changed_regions prev_matrix curr_matrix in
  let ops = ref [] in
  
  List.iter (fun region ->
    (* Move to region start *)
    ops := Ansi_ast.MoveCursor (region.x, region.y) :: !ops;
    
    (* Render each line in the region *)
    for y = region.y to region.y + region.height - 1 do
      if y > region.y then
        (* Move to next line *)
        ops := Ansi_ast.MoveCursor (region.x, y) :: !ops;
      
      (* Render cells in this line *)
      for x = region.x to region.x + region.width - 1 do
        match Matrix.get curr_matrix ~x ~y with
        | Some cell ->
            (* Build nested style operations for this cell *)
            let text_op = Ansi_ast.Text cell.Matrix.char in
            
            (* Wrap with style modifiers as needed *)
            let styled_op = 
              let op = ref text_op in
              if cell.strikethrough then op := Ansi_ast.Strikethrough [!op];
              if cell.reverse then op := Ansi_ast.Reverse [!op];
              if cell.underline then op := Ansi_ast.Underline [!op];
              if cell.italic then op := Ansi_ast.Italic [!op];
              if cell.bold then op := Ansi_ast.Bold [!op];
              (match cell.bg with
              | Some bg -> op := Ansi_ast.Bg (bg, [!op])
              | None -> ());
              (match cell.fg with
              | Some fg -> op := Ansi_ast.Fg (fg, [!op])
              | None -> ());
              !op
            in
            
            ops := styled_op :: !ops
        | None -> ()
      done
    done
  ) regions;
  
  (* Optimize and render *)
  Ansi_ast.Seq (List.rev !ops) |> Ansi_ast.render

(** Compute statistics about the changes *)
let change_stats ~prev_matrix ~curr_matrix =
  let regions = find_changed_regions prev_matrix curr_matrix in
  let total_cells = curr_matrix.Matrix.width * curr_matrix.Matrix.height in
  let changed_cells = List.fold_left (fun acc r -> 
    acc + (r.width * r.height)
  ) 0 regions in
  let percentage = if total_cells > 0 then
    float_of_int changed_cells *. 100.0 /. float_of_int total_cells
  else 0.0 in
  format {|
    Total cells: %d
    Changed cells: %d
    Change percentage: %.1f%%
    Regions: %d
  |} total_cells changed_cells percentage (List.length regions)