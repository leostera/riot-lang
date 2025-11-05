open Std

(** Convert Tty.Color.t to ANSI foreground escape code *)
let color_to_fg_ansi color =
  match color with
  | Tty.Color.RGB (r, g, b) -> format "\x1b[38;2;%d;%d;%dm" r g b
  | Tty.Color.ANSI c -> format "\x1b[%dm" (30 + c)
  | Tty.Color.ANSI256 c -> format "\x1b[38;5;%dm" c
  | Tty.Color.No_color -> ""

(** Convert Tty.Color.t to ANSI background escape code *)
let color_to_bg_ansi color =
  match color with
  | Tty.Color.RGB (r, g, b) -> format "\x1b[48;2;%d;%d;%dm" r g b
  | Tty.Color.ANSI c -> format "\x1b[%dm" (40 + c)
  | Tty.Color.ANSI256 c -> format "\x1b[48;5;%dm" c
  | Tty.Color.No_color -> ""

(** Build ANSI codes for a cell's style attributes *)
let cell_to_ansi cell prev_cell =
  let buf = Buffer.create 32 in
  
  (* Only emit changes compared to previous cell *)
  let fg_changed = cell.Matrix.fg <> prev_cell.Matrix.fg in
  let bg_changed = cell.Matrix.bg <> prev_cell.Matrix.bg in
  let attrs_changed = 
    cell.bold <> prev_cell.bold ||
    cell.italic <> prev_cell.italic ||
    cell.underline <> prev_cell.underline ||
    cell.strikethrough <> prev_cell.strikethrough ||
    cell.reverse <> prev_cell.reverse
  in
  
  (* If anything changed, reset and rebuild *)
  if fg_changed || bg_changed || attrs_changed then begin
    (* Reset all attributes *)
    Buffer.add_string buf "\x1b[0m";
    
    (* Apply foreground color *)
    (match cell.fg with
    | Some c -> Buffer.add_string buf (color_to_fg_ansi c)
    | None -> ());
    
    (* Apply background color *)
    (match cell.bg with
    | Some c -> Buffer.add_string buf (color_to_bg_ansi c)
    | None -> ());
    
    (* Apply text attributes *)
    if cell.bold then Buffer.add_string buf "\x1b[1m";
    if cell.italic then Buffer.add_string buf "\x1b[3m";
    if cell.underline then Buffer.add_string buf "\x1b[4m";
    if cell.strikethrough then Buffer.add_string buf "\x1b[9m";
    if cell.reverse then Buffer.add_string buf "\x1b[7m";
  end;
  
  Buffer.contents buf

(** Emit full matrix to ANSI string *)
let emit matrix =
  let buf = Buffer.create (matrix.Matrix.width * matrix.Matrix.height * 2) in
  
  (* NOTE: Do NOT emit \x1b[H here - the caller is responsible for positioning.
     Emitting it here causes issues when multiple processes are printing because
     it moves the cursor back to home mid-render. *)
  
  let prev_cell = ref Matrix.empty_cell in
  
  for row = 0 to matrix.height - 1 do
    for col = 0 to matrix.width - 1 do
      let cell = matrix.cells.(row).(col) in
      
      (* Emit style changes *)
      let style_codes = cell_to_ansi cell !prev_cell in
      Buffer.add_string buf style_codes;
      
      (* Emit character *)
      Buffer.add_string buf cell.char;
      
      (* Update prev_cell *)
      prev_cell := cell;
    done;
    
    (* Newline at end of each row (except last) *)
    if row < matrix.height - 1 then
      Buffer.add_string buf "\r\n";
  done;
  
  (* Reset at the end *)
  Buffer.add_string buf "\x1b[0m";
  
  Buffer.contents buf

(** Emit only differences between two matrices *)
let emit_diff ~old ~new_ =
  if old.Matrix.width <> new_.Matrix.width || old.Matrix.height <> new_.Matrix.height then
    (* Sizes differ, do full re-render *)
    emit new_
  else
    let buf = Buffer.create 1024 in
    let prev_cell = ref Matrix.empty_cell in
    
    for row = 0 to new_.Matrix.height - 1 do
      let row_has_changes = ref false in
      
      (* Check if this row has any changes *)
      for col = 0 to new_.Matrix.width - 1 do
        if old.Matrix.cells.(row).(col) <> new_.Matrix.cells.(row).(col) then begin
          row_has_changes := true;
        end
      done;
      
      (* If row has changes, emit it *)
      if !row_has_changes then begin
        (* Move to start of this row *)
        Buffer.add_string buf (format "\x1b[%d;1H" (row + 1));
        
        for col = 0 to new_.Matrix.width - 1 do
          let cell = new_.Matrix.cells.(row).(col) in
          
          (* Emit style changes *)
          let style_codes = cell_to_ansi cell !prev_cell in
          Buffer.add_string buf style_codes;
          
          (* Emit character *)
          Buffer.add_string buf cell.Matrix.char;
          
          (* Update prev_cell *)
          prev_cell := cell;
        done;
      end;
    done;
    
    (* Reset at the end *)
    Buffer.add_string buf "\x1b[0m";
    
    Buffer.contents buf
