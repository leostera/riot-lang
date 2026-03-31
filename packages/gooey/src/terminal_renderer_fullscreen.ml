open Std
open Std.IO
open Sync
open Sync.Cell

(* Fullscreen renderer with absolute cursor positioning *)

let coords_to_cell = fun x y ->
  let col = int_of_float (x +. 0.5) in
  let row = int_of_float (y +. 0.5) in
  (col, row)

let rgb_to_color = fun (`rgb (r, g, b)) -> Tty.Color.of_rgb (r, g, b)

let format_with_fg = fun color text ->
  Ansi_formatter.format_string [ Ansi_formatter.Foreground color ] text

let format_with_bg = fun color text ->
  Ansi_formatter.format_string [ Ansi_formatter.Background color ] text

let is_inside_scissor = fun x y scissor ->
  match scissor with
  | None -> true
  | Some rect ->
      let open Geometry.Rect in
        let fx = float_of_int x in
        let fy = float_of_int y in
        fx >= rect.x && fx < rect.x +. rect.width && fy >= rect.y && fy < rect.y +. rect.height

let render_to_string = fun commands ->
  let buf = Buffer.create 1_024 in
  let scissor_box = Cell.create None in
  List.iter
    (fun cmd ->
      let open Render in
        match cmd.command_type with
        | Rectangle { color; _ } ->
            let col_start, row_start = coords_to_cell cmd.bounding_box.x cmd.bounding_box.y in
            let col_end, row_end = coords_to_cell
            (cmd.bounding_box.x +. cmd.bounding_box.width)
            (cmd.bounding_box.y +. cmd.bounding_box.height) in
            let tty_color = rgb_to_color color in
            let colored_space = format_with_bg tty_color " " in
            for row = row_start to row_end - 1 do
              for col = col_start to col_end - 1 do
                if is_inside_scissor col row (Cell.get scissor_box) then
                  begin
                    Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (col + 1));
                    Buffer.add_string buf colored_space
                  end
              done
            done
        | Text { content; color; _ } ->
            let col, row = coords_to_cell cmd.bounding_box.x cmd.bounding_box.y in
            if is_inside_scissor col row (Cell.get scissor_box) then
              begin
                let tty_color = rgb_to_color color in
                Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (col + 1));
                let lines = String.split_on_char '\n' content in
                List.iteri
                  (fun i line ->
                    if i > 0 then
                      Buffer.add_string
                      buf
                      (Tty.Escape_seq.cursor_position_seq (row + i + 1) (col + 1));
                    let colored_text = format_with_fg tty_color line in
                    Buffer.add_string buf colored_text)
                  lines
              end
        | Border { width; color; _ } ->
            let col_start, row_start = coords_to_cell cmd.bounding_box.x cmd.bounding_box.y in
            let col_end, row_end = coords_to_cell
            (cmd.bounding_box.x +. cmd.bounding_box.width)
            (cmd.bounding_box.y +. cmd.bounding_box.height) in
            let tty_color = rgb_to_color color in
            if width.top > 0 then
              begin
                Buffer.add_string
                buf
                (Tty.Escape_seq.cursor_position_seq (row_start + 1) (col_start + 1));
                let top_left =
                  if width.left > 0 then
                    "┌"
                  else
                    "─"
                in
                let num_middle = max 0 (col_end - col_start - 2) in
                let top_middle =
                  String.concat "" (List.init num_middle (fun _ -> "─"))
                in
                let top_right =
                  if width.right > 0 then
                    "┐"
                  else
                    "─"
                in
                let top_line = top_left ^ top_middle ^ top_right in
                Buffer.add_string buf (format_with_fg tty_color top_line)
              end;
            for row = row_start + 1 to row_end - 2 do
              if width.left > 0 && is_inside_scissor col_start row (Cell.get scissor_box) then
                begin
                  Buffer.add_string
                  buf
                  (Tty.Escape_seq.cursor_position_seq (row + 1) (col_start + 1));
                  Buffer.add_string buf (format_with_fg tty_color "│")
                end;
              if width.right > 0 && is_inside_scissor (col_end - 1) row (Cell.get scissor_box) then
                begin
                  Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) col_end);
                  Buffer.add_string buf (format_with_fg tty_color "│")
                end
            done;
            if width.bottom > 0 then
              begin
                Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq row_end (col_start + 1));
                let bottom_left =
                  if width.left > 0 then
                    "└"
                  else
                    "─"
                in
                let num_middle = max 0 (col_end - col_start - 2) in
                let bottom_middle =
                  String.concat "" (List.init num_middle (fun _ -> "─"))
                in
                let bottom_right =
                  if width.right > 0 then
                    "┘"
                  else
                    "─"
                in
                let bottom_line = bottom_left ^ bottom_middle ^ bottom_right in
                Buffer.add_string buf (format_with_fg tty_color bottom_line)
              end
        | ScissorStart rect ->
            Cell.set scissor_box (Some rect)
        | ScissorEnd ->
            Cell.set scissor_box None
        | Custom _ ->
            ())
    commands;
  Buffer.contents buf

let render = fun commands -> print (render_to_string commands)
