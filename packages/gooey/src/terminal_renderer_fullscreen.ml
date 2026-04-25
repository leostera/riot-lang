open Std
open Std.IO

module Utils = Terminal_render_utils

let visible_text_range = fun ~box ~scissor ->
  let col_start = Utils.rect_col_start box in
  let col_end = Utils.rect_col_end box in
  let visible_col_start =
    match scissor with
    | Some rect -> Int.max col_start (Utils.rect_col_start rect)
    | None -> col_start
  in
  let visible_col_end =
    match scissor with
    | Some rect -> Int.min col_end (Utils.rect_col_end rect)
    | None -> col_end
  in
  (col_start, visible_col_start, visible_col_end)

let render_to_string = fun commands ->
  let buf = Buffer.create ~size:1_024 in
  let scissor_box = ref None in
  List.for_each commands ~fn:(
    fun command ->
      match command.Render.command_type with
      | Render.ScissorStart rect -> scissor_box := Some rect
      | Render.ScissorEnd -> scissor_box := None
      | Render.Rectangle { color; _ } ->
          let tty_color = Utils.rgb_to_color color in
          let colored_space = Ansi_formatter.format_string [ Ansi_formatter.Background tty_color ] " " in
          let row_start = Utils.rect_row_start command.bounding_box in
          let row_end = Utils.rect_row_end command.bounding_box in
          let col_start = Utils.rect_col_start command.bounding_box in
          let col_end = Utils.rect_col_end command.bounding_box in
          for row = row_start to row_end - 1 do
            for col = col_start to col_end - 1 do
              if Utils.is_inside_scissor ~col ~row !scissor_box then
                begin
                  Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (col + 1));
                  Buffer.add_string buf colored_space
                end
            done
          done
      | Render.Border { width; color; _ } ->
          let tty_color = Utils.rgb_to_color color in
          let fmt = [ Ansi_formatter.Foreground tty_color ] in
          let row_start = Utils.rect_row_start command.bounding_box in
          let row_end = Utils.rect_row_end command.bounding_box in
          let col_start = Utils.rect_col_start command.bounding_box in
          let col_end = Utils.rect_col_end command.bounding_box in
          if width.top > 0 then
            for col = col_start to col_end - 1 do
              if Utils.is_inside_scissor ~col ~row:row_start !scissor_box then
                begin
                  let ch =
                    if col = col_start && width.left > 0 then
                      "┌"
                    else
                      if col = col_end - 1 && width.right > 0 then
                        "┐"
                      else "─"
                  in
                  Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row_start + 1) (col + 1));
                  Buffer.add_string buf (Ansi_formatter.format_string fmt ch)
                end
            done;
          if width.bottom > 0 then
            for col = col_start to col_end - 1 do
              if Utils.is_inside_scissor ~col ~row:(row_end - 1) !scissor_box then
                begin
                  let ch =
                    if col = col_start && width.left > 0 then
                      "└"
                    else
                      if col = col_end - 1 && width.right > 0 then
                        "┘"
                      else "─"
                  in
                  Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq row_end (col + 1));
                  Buffer.add_string buf (Ansi_formatter.format_string fmt ch)
                end
            done;
          for row = row_start + 1 to row_end - 2 do
            if width.left > 0 && Utils.is_inside_scissor ~col:col_start ~row !scissor_box then
              begin
                Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (col_start + 1));
                Buffer.add_string buf (Ansi_formatter.format_string fmt "│")
              end;
            if width.right > 0 && Utils.is_inside_scissor ~col:(col_end - 1) ~row !scissor_box then
              begin
                Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) col_end);
                Buffer.add_string buf (Ansi_formatter.format_string fmt "│")
              end
          done
      | Render.Text { content; color; weight; decoration; _ } ->
          let lines = String.split_on_char '\n' content in lines |> List.enumerate |> List.for_each ~fn:(
            fun (line_index, line) ->
              let row = Utils.rect_row_start command.bounding_box + line_index in
              if row < Utils.rect_row_end command.bounding_box then
                let col_start, visible_col_start, visible_col_end = visible_text_range ~box:command.bounding_box ~scissor:!scissor_box in
                if visible_col_end > visible_col_start && Utils.is_inside_scissor ~col:visible_col_start ~row !scissor_box then
                  let clipped = Utils.slice_text_by_cells line ~skip:(visible_col_start - col_start) ~take:(visible_col_end - visible_col_start) in
                  if clipped != "" then
                    begin
                      Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (visible_col_start + 1));
                      Buffer.add_string buf (Ansi_formatter.format_string (Utils.text_formats ~color ~weight ~decoration) clipped)
                    end
          )
      | Render.Custom { data } ->
          let lines = String.split_on_char '\n' data in lines |> List.enumerate |> List.for_each ~fn:(
            fun (line_index, line) ->
              let row = Utils.rect_row_start command.bounding_box + line_index in
              if row < Utils.rect_row_end command.bounding_box then
                let col_start, visible_col_start, visible_col_end = visible_text_range ~box:command.bounding_box ~scissor:!scissor_box in
                if visible_col_end > visible_col_start && Utils.is_inside_scissor ~col:visible_col_start ~row !scissor_box then
                  let clipped = Utils.slice_text_by_cells line ~skip:(visible_col_start - col_start) ~take:(visible_col_end - visible_col_start) in
                  if clipped != "" then
                    begin
                      Buffer.add_string buf (Tty.Escape_seq.cursor_position_seq (row + 1) (visible_col_start + 1));
                      Buffer.add_string buf clipped
                    end
          )
  );
  Buffer.contents buf

let render = fun commands -> print (render_to_string commands)
