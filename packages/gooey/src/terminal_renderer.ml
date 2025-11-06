open Std

(* Configuration *)
type config = {
  clear_screen : bool;
  use_alternate_buffer : bool;
}

let default_config = {
  clear_screen = true;
  use_alternate_buffer = false;
}

(* Convert coordinates to character cells *)
(* For terminal rendering, we treat layout coordinates directly as character cells *)
let coords_to_cell x y =
  let col = int_of_float (x +. 0.5) in
  let row = int_of_float (y +. 0.5) in
  (col, row)

(* Convert Colors.rgb to Tty.Color.t *)
let rgb_to_color (`rgb (r, g, b)) = Tty.Color.of_rgb (r, g, b)

(* Format string with color using ANSI formatter *)
let format_with_fg color text =
  Ansi_formatter.format_string [Ansi_formatter.Foreground color] text

let format_with_bg color text =
  Ansi_formatter.format_string [Ansi_formatter.Background color] text

(* Check if a point is inside the scissor box *)
let is_inside_scissor x y scissor =
  match scissor with
  | None -> true
  | Some rect ->
      let open Geometry.Rect in
      let fx = float_of_int x in
      let fy = float_of_int y in
      fx >= rect.x && fx < rect.x +. rect.width &&
      fy >= rect.y && fy < rect.y +. rect.height

(* Move cursor to position (1-based for terminal) *)
let move_cursor col row =
  print "%s" (Tty.Escape_seq.cursor_position_seq (row + 1) (col + 1))

(* Render a filled rectangle using colored spaces *)
let render_rectangle bbox color scissor =
  let open Geometry.Rect in
  let col_start, row_start = coords_to_cell bbox.x bbox.y in
  let col_end, row_end = coords_to_cell (bbox.x +. bbox.width) (bbox.y +. bbox.height) in
  
  let tty_color = rgb_to_color color in
  let colored_space = format_with_bg tty_color " " in
  
  for row = row_start to row_end - 1 do
    for col = col_start to col_end - 1 do
      if is_inside_scissor col row scissor then begin
        move_cursor col row;
        print "%s" colored_space
      end
    done
  done

(* Render text with foreground color *)
let render_text bbox content color scissor =
  let open Geometry.Rect in
  let col, row = coords_to_cell bbox.x bbox.y in
  
  if is_inside_scissor col row scissor then begin
    let tty_color = rgb_to_color color in
    move_cursor col row;
    
    (* Handle multi-line text *)
    let lines = String.split_on_char '\n' content in
    List.iteri (fun i line ->
      if i > 0 then move_cursor col (row + i);
      let colored_text = format_with_fg tty_color line in
      print "%s" colored_text
    ) lines
  end

(* Unicode box-drawing characters *)
let box_chars = {|
  ┌─┐
  │ │
  └─┘
|}

(* Render border using box-drawing characters *)
let render_border bbox (width : Render.border_width) color scissor =
  let col_start, row_start = coords_to_cell bbox.Geometry.Rect.x bbox.Geometry.Rect.y in
  let col_end, row_end = coords_to_cell (bbox.Geometry.Rect.x +. bbox.Geometry.Rect.width) (bbox.Geometry.Rect.y +. bbox.Geometry.Rect.height) in
  
  let tty_color = rgb_to_color color in
  
  (* Top border *)
  if width.top > 0 then begin
    move_cursor col_start row_start;
    
    (* Build the top line *)
    let top_left = if width.left > 0 then "┌" else "─" in
    let num_middle = max 0 (col_end - col_start - 2) in
    let top_middle = String.concat "" (List.init num_middle (fun _ -> "─")) in
    let top_right = if width.right > 0 then "┐" else "─" in
    let top_line = top_left ^ top_middle ^ top_right in
    
    let colored_border = format_with_fg tty_color top_line in
    print "%s" colored_border
  end;
  
  (* Left and right borders *)
  for row = row_start + 1 to row_end - 2 do
    if width.left > 0 && is_inside_scissor col_start row scissor then begin
      move_cursor col_start row;
      let colored_border = format_with_fg tty_color "│" in
      print "%s" colored_border
    end;
    
    if width.right > 0 && is_inside_scissor (col_end - 1) row scissor then begin
      move_cursor (col_end - 1) row;
      let colored_border = format_with_fg tty_color "│" in
      print "%s" colored_border
    end
  done;
  
  (* Bottom border *)
  if width.bottom > 0 then begin
    move_cursor col_start (row_end - 1);
    
    (* Build the bottom line *)
    let bottom_left = if width.left > 0 then "└" else "─" in
    let num_middle = max 0 (col_end - col_start - 2) in
    let bottom_middle = String.concat "" (List.init num_middle (fun _ -> "─")) in
    let bottom_right = if width.right > 0 then "┘" else "─" in
    let bottom_line = bottom_left ^ bottom_middle ^ bottom_right in
    
    let colored_border = format_with_fg tty_color bottom_line in
    print "%s" colored_border
  end

(* Main render function *)
let render_with_config config commands =
  (* Clear screen if requested *)
  if config.clear_screen then begin
    print "%s" (Tty.Escape_seq.csi ^ Tty.Escape_seq.erase_display_seq 2);
    print "%s" (Tty.Escape_seq.cursor_position_seq 1 1)
  end;
  
  (* Use alternate buffer if requested *)
  if config.use_alternate_buffer then
    print "%s" Tty.Escape_seq.alt_screen_seq;
  
  (* Hide cursor *)
  print "%s" Tty.Escape_seq.hide_cursor_seq;
  
  (* Track scissor region for clipping *)
  let scissor_box = ref None in
  
  (* Process each render command *)
  List.iter (fun cmd ->
    let open Render in
    match cmd.command_type with
    | Rectangle { color; _ } ->
        render_rectangle cmd.bounding_box color !scissor_box
    
    | Text { content; color; _ } ->
        render_text cmd.bounding_box content color !scissor_box
    
    | Border { width; color; _ } ->
        render_border cmd.bounding_box width color !scissor_box
    
    | ScissorStart rect ->
        scissor_box := Some rect
    
    | ScissorEnd ->
        scissor_box := None
    
    | Custom _ ->
        (* Skip custom render commands *)
        ()
  ) commands;
  
  (* Show cursor and flush *)
  print "%s" Tty.Escape_seq.show_cursor_seq;
  ()

(* Convenience function with default config *)
let render commands =
  render_with_config default_config commands

(* Render to buffer instead of stdout *)
let render_to_buffer commands buf =
  (* Temporarily redirect stdout *)
  let old_stdout = Unix.dup Unix.stdout in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.dup2 pipe_write Unix.stdout;
  Unix.close pipe_write;
  
  (* Render *)
  render commands;
  
  (* Read from pipe into buffer *)
  let rec read_all () =
    let bytes = Bytes.create 4096 in
    try
      let n = Unix.read pipe_read bytes 0 4096 in
      if n > 0 then begin
        Buffer.add_subbytes buf bytes 0 n;
        read_all ()
      end
    with Unix.Unix_error (Unix.EAGAIN, _, _) | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
      ()
  in
  read_all ();
  
  (* Restore stdout *)
  Unix.dup2 old_stdout Unix.stdout;
  Unix.close old_stdout;
  Unix.close pipe_read

(* Utility functions *)
let clear () =
  print "%s" (Tty.Escape_seq.csi ^ Tty.Escape_seq.erase_display_seq 2);
  print "%s" (Tty.Escape_seq.cursor_position_seq 1 1);
  ()

let hide_cursor () =
  print "%s" Tty.Escape_seq.hide_cursor_seq;
  ()

let show_cursor () =
  print "%s" Tty.Escape_seq.show_cursor_seq;
  ()

let move_cursor ~x ~y =
  print "%s" (Tty.Escape_seq.cursor_position_seq (y + 1) (x + 1));
  ()
