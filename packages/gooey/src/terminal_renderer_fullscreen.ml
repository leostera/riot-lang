open Std
open Std.IO

let start_cell = fun value -> Int.max 0 (Float.to_int (Float.floor value))

let end_cell = fun value -> Int.max 0 (Float.to_int (Float.ceil value))

let rgb_to_color = fun (`rgb (r, g, b)) -> Tty.Color.of_rgb (r, g, b)

let rect_col_start = fun (rect: Geometry.Rect.t) -> start_cell rect.x

let rect_row_start = fun (rect: Geometry.Rect.t) -> start_cell rect.y

let rect_col_end = fun (rect: Geometry.Rect.t) -> end_cell (rect.x +. rect.width)

let rect_row_end = fun (rect: Geometry.Rect.t) -> end_cell (rect.y +. rect.height)

let is_inside_scissor = fun col row scissor ->
  match scissor with
  | None -> true
  | Some rect ->
      col >= rect_col_start rect
      && col < rect_col_end rect
      && row >= rect_row_start rect
      && row < rect_row_end rect

let text_formats = fun color weight decoration ->
  let formats = ref [ Ansi_formatter.Foreground (rgb_to_color color) ] in
  (
    match weight with
    | Style.Bold -> formats := Ansi_formatter.Bold :: !formats
    | Style.Normal -> ()
  );
  (
    match decoration with
    | Style.Underline -> formats := Ansi_formatter.Underline :: !formats
    | Style.Strikethrough -> formats := Ansi_formatter.CrossOut :: !formats
    | Style.NoDecoration -> ()
  );
  List.rev !formats

let slice_text_by_cells = fun text ~skip ~take ->
  if take <= 0 then
    ""
  else
    let graphemes = String.into_grapheme_iter text |> Std.Iter.Iterator.to_list in
    let rec loop col acc =
      function
      | [] -> List.rev acc |> String.concat ""
      | grapheme :: rest ->
          let grapheme_string = Std.Unicode.Grapheme.to_string grapheme in
          let grapheme_width = Std.Unicode.Grapheme.width grapheme in
          let next_col = col + grapheme_width in
          if next_col <= skip then
            loop next_col acc rest
          else if col < skip then
            loop next_col acc rest
          else if next_col > skip + take then
            List.rev acc |> String.concat ""
          else
            loop next_col (grapheme_string :: acc) rest
    in
    loop 0 [] graphemes

let render_to_string = fun commands ->
  let buf = Buffer.create ~size:1_024 in
  let scissor_box = ref None in
  List.iter
    (fun command ->
      match command.Render.command_type with
      | Render.ScissorStart rect ->
          scissor_box := Some rect
      | Render.ScissorEnd ->
          scissor_box := None
      | Render.Rectangle { color; _ } ->
          let tty_color = rgb_to_color color in
          let colored_space = Ansi_formatter.format_string [ Ansi_formatter.Background tty_color ] " " in
          let row_start = rect_row_start command.bounding_box in
          let row_end = rect_row_end command.bounding_box in
          let col_start = rect_col_start command.bounding_box in
          let col_end = rect_col_end command.bounding_box in
          for row = row_start to row_end - 1 do
            for col = col_start to col_end - 1 do
              if is_inside_scissor col row !scissor_box then
                begin
                  Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (col + 1));
                  Buffer.add_string buf colored_space
                end
            done
          done
      | Render.Border { width; color; _ } ->
          let tty_color = rgb_to_color color in
          let fmt = [ Ansi_formatter.Foreground tty_color ] in
          let row_start = rect_row_start command.bounding_box in
          let row_end = rect_row_end command.bounding_box in
          let col_start = rect_col_start command.bounding_box in
          let col_end = rect_col_end command.bounding_box in
          if width.top > 0 then
            for col = col_start to col_end - 1 do
              if is_inside_scissor col row_start !scissor_box then
                begin
                  let ch =
                    if col = col_start && width.left > 0 then
                      "┌"
                    else if col = col_end - 1 && width.right > 0 then
                      "┐"
                    else
                      "─"
                  in
                  Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row_start + 1) (col + 1));
                  Buffer.add_string buf (Ansi_formatter.format_string fmt ch)
                end
            done;
          if width.bottom > 0 then
            for col = col_start to col_end - 1 do
              if is_inside_scissor col (row_end - 1) !scissor_box then
                begin
                  let ch =
                    if col = col_start && width.left > 0 then
                      "└"
                    else if col = col_end - 1 && width.right > 0 then
                      "┘"
                    else
                      "─"
                  in
                  Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq row_end (col + 1));
                  Buffer.add_string buf (Ansi_formatter.format_string fmt ch)
                end
            done;
          for row = row_start + 1 to row_end - 2 do
            if width.left > 0 && is_inside_scissor col_start row !scissor_box then
              begin
                Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (col_start + 1));
                Buffer.add_string buf (Ansi_formatter.format_string fmt "│")
              end;
            if width.right > 0 && is_inside_scissor (col_end - 1) row !scissor_box then
              begin
                Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) col_end);
                Buffer.add_string buf (Ansi_formatter.format_string fmt "│")
              end
          done
      | Render.Text { content; color; weight; decoration; _ } ->
          let lines = String.split_on_char '\n' content in
          List.iteri
            (fun line_index line ->
              let row = rect_row_start command.bounding_box + line_index in
              if row < rect_row_end command.bounding_box then
                let col_start = rect_col_start command.bounding_box in
                let col_end = rect_col_end command.bounding_box in
                let visible_col_start =
                  match !scissor_box with
                  | Some rect -> Int.max col_start (rect_col_start rect)
                  | None -> col_start
                in
                let visible_col_end =
                  match !scissor_box with
                  | Some rect -> Int.min col_end (rect_col_end rect)
                  | None -> col_end
                in
                if visible_col_end > visible_col_start && is_inside_scissor visible_col_start row !scissor_box then
                  let clipped = slice_text_by_cells line ~skip:(visible_col_start - col_start) ~take:(visible_col_end - visible_col_start) in
                  if clipped != "" then
                    begin
                      Buffer.add_string
                        buf
                        (Tty.Escape_seq.cursor_position_seq (row + 1) (visible_col_start + 1));
                      Buffer.add_string buf (Ansi_formatter.format_string (text_formats color weight decoration) clipped)
                    end)
            lines
      | Render.Custom { data } ->
          let lines = String.split_on_char '\n' data in
          List.iteri
            (fun line_index line ->
              let row = rect_row_start command.bounding_box + line_index in
              if row < rect_row_end command.bounding_box then
                let col_start = rect_col_start command.bounding_box in
                let col_end = rect_col_end command.bounding_box in
                let visible_col_start =
                  match !scissor_box with
                  | Some rect -> Int.max col_start (rect_col_start rect)
                  | None -> col_start
                in
                let visible_col_end =
                  match !scissor_box with
                  | Some rect -> Int.min col_end (rect_col_end rect)
                  | None -> col_end
                in
                if visible_col_end > visible_col_start && is_inside_scissor visible_col_start row !scissor_box then
                  let clipped =
                    slice_text_by_cells
                      line
                      ~skip:(visible_col_start - col_start)
                      ~take:(visible_col_end - visible_col_start)
                  in
                  if clipped != "" then
                    begin
                      Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (visible_col_start + 1));
                      Buffer.add_string buf clipped
                    end)
            lines)
    commands;
  Buffer.contents buf

let render = fun commands -> print (render_to_string commands)
