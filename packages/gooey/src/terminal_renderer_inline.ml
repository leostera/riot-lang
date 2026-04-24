open Std
open Std.Collections
open Std.IO
module Utils = Terminal_render_utils

type cell = {
  mutable char: string;
  mutable char_width: int;
  mutable fg_color: Tty.Color.t option;
  mutable bg_color: Tty.Color.t option;
  mutable bold: bool;
  mutable underline: bool;
  mutable strike: bool;
}

type custom_segment = {
  row: int;
  col: int;
  command_order: int;
  data: string;
}

let make_cell = fun () ->
  {
    char = " ";
    char_width = 1;
    fg_color = None;
    bg_color = None;
    bold = false;
    underline = false;
    strike = false;
  }

let get_cell = fun grid ~row ~col ->
  let line = Array.get_unchecked grid ~at:row in
  Array.get_unchecked line ~at:col

let reset_text_style = fun cell ->
  cell.bold <- false;
  cell.underline <- false;
  cell.strike <- false

let write_background_cell = fun cell color ->
  cell.char <- " ";
  cell.char_width <- 1;
  cell.fg_color <- None;
  cell.bg_color <- Some color;
  reset_text_style cell

let write_border_cell = fun cell color ch ->
  cell.char <- ch;
  cell.char_width <- 1;
  cell.fg_color <- Some color;
  reset_text_style cell

let write_text_cell = fun cell color weight decoration grapheme grapheme_width ->
  cell.char <- grapheme;
  cell.char_width <- grapheme_width;
  cell.fg_color <- Some color;
  cell.bold <- (
    match weight with
    | Style.Bold -> true
    | Style.Normal -> false
  );
  cell.underline <- (
    match decoration with
    | Style.Underline -> true
    | Style.NoDecoration
    | Style.Strikethrough -> false
  );
  cell.strike <- (
    match decoration with
    | Style.Strikethrough -> true
    | Style.NoDecoration
    | Style.Underline -> false
  )

let write_continuation_cell = fun cell ->
  cell.char <- "";
  cell.char_width <- 0;
  cell.fg_color <- None;
  reset_text_style cell

let cell_formats = fun cell ->
  let formats = ref [] in
  (
    match cell.fg_color with
    | Some color -> formats := Ansi_formatter.Foreground color :: !formats
    | None -> ()
  );
  (
    match cell.bg_color with
    | Some color -> formats := Ansi_formatter.Background color :: !formats
    | None -> ()
  );
  if cell.bold then
    formats := Ansi_formatter.Bold :: !formats;
  if cell.underline then
    formats := Ansi_formatter.Underline :: !formats;
  if cell.strike then
    formats := Ansi_formatter.CrossOut :: !formats;
  List.rev !formats

let cell_to_string = fun cell ->
  if cell.char_width = 0 then
    ""
  else
    let formats = cell_formats cell in
    if formats = [] then
      cell.char
    else
      Ansi_formatter.format_string formats cell.char

let index_commands = fun commands ->
  let rec loop index acc = function
    | [] -> List.rev acc
    | command :: rest -> loop (index + 1) ((index, command) :: acc) rest
  in
  loop 0 [] commands

let render_to_string = fun commands ->
  if commands = [] then
    ""
  else
    let max_row = ref 0 in
    let max_col = ref 0 in
    List.for_each commands
      ~fn:(fun command ->
        match command.Render.command_type with
        | Render.ScissorStart _
        | Render.ScissorEnd ->
            ()
        | Render.Custom { data } ->
            let _ = data in
            max_row := Int.max !max_row (Utils.rect_row_end command.bounding_box);
            max_col := Int.max !max_col (Utils.rect_col_end command.bounding_box)
        | _ ->
            max_row := Int.max !max_row (Utils.rect_row_end command.bounding_box);
            max_col := Int.max !max_col (Utils.rect_col_end command.bounding_box));
    if !max_row = 0 || !max_col = 0 then
      ""
    else
      let grid =
        Array.init
          ~count:!max_row
          ~fn:(fun _ -> Array.init ~count:!max_col ~fn:(fun _ -> make_cell ()))
      in
      let custom_segments = ref [] in
      let scissor_box = ref None in
      List.for_each (index_commands commands)
        ~fn:(fun (index, command) ->
          match command.Render.command_type with
          | Render.ScissorStart rect ->
              scissor_box := Some rect
          | Render.ScissorEnd ->
              scissor_box := None
          | Render.Rectangle { color; _ } ->
              let tty_color = Utils.rgb_to_color color in
              let row_start = Utils.rect_row_start command.bounding_box in
              let row_end = Int.min !max_row (Utils.rect_row_end command.bounding_box) in
              let col_start, col_end = Utils.visible_col_range
                ~box:command.bounding_box
                ~scissor:!scissor_box
                ~limit:!max_col in
              for row = row_start to row_end - 1 do
                if
                  Utils.is_inside_scissor ~col:col_start ~row !scissor_box
                  || (col_end > col_start && Utils.is_inside_scissor ~col:(col_end - 1) ~row !scissor_box)
                then
                  begin
                    for col = col_start to col_end - 1 do
                      if Utils.is_inside_scissor ~col ~row !scissor_box then
                        write_background_cell (get_cell grid ~row ~col) tty_color
                    done
                  end
              done
          | Render.Border { width; color; _ } ->
              let tty_color = Utils.rgb_to_color color in
              let row_start = Utils.rect_row_start command.bounding_box in
              let row_end = Int.min !max_row (Utils.rect_row_end command.bounding_box) in
              let col_start = Utils.rect_col_start command.bounding_box in
              let col_end = Int.min !max_col (Utils.rect_col_end command.bounding_box) in
              if width.top > 0 && row_start < row_end then
                begin
                  for col = col_start to col_end - 1 do
                    if Utils.is_inside_scissor ~col ~row:row_start !scissor_box then
                      let ch =
                        if col = col_start && width.left > 0 then
                          "┌"
                        else if col = col_end - 1 && width.right > 0 then
                          "┐"
                        else
                          "─"
                      in
                      write_border_cell (get_cell grid ~row:row_start ~col) tty_color ch
                  done
                end;
              if width.bottom > 0 && row_end - 1 >= row_start then
                begin
                  for col = col_start to col_end - 1 do
                    if Utils.is_inside_scissor ~col ~row:(row_end - 1) !scissor_box then
                      let ch =
                        if col = col_start && width.left > 0 then
                          "└"
                        else if col = col_end - 1 && width.right > 0 then
                          "┘"
                        else
                          "─"
                      in
                      write_border_cell (get_cell grid ~row:(row_end - 1) ~col) tty_color ch
                  done
                end;
              for row = row_start + 1 to row_end - 2 do
                if width.left > 0 && col_start < col_end then
                  if Utils.is_inside_scissor ~col:col_start ~row !scissor_box then
                    write_border_cell (get_cell grid ~row ~col:col_start) tty_color "│";
                if width.right > 0 && col_end - 1 >= col_start then
                  if Utils.is_inside_scissor ~col:(col_end - 1) ~row !scissor_box then
                    write_border_cell (get_cell grid ~row ~col:(col_end - 1)) tty_color "│"
              done
          | Render.Text {
            content;
            color;
            weight;
            decoration;
            _
          } ->
              let tty_color = Utils.rgb_to_color color in
              let row_start = Utils.rect_row_start command.bounding_box in
              let row_end = Int.min !max_row (Utils.rect_row_end command.bounding_box) in
              let lines = String.split_on_char '\n' content in
              lines |> List.enumerate |> List.for_each
                ~fn:(fun (line_index, line) ->
                  let row = row_start + line_index in
                  if row < row_end then
                    let col_start = Utils.rect_col_start command.bounding_box in
                    let col_end = Int.min !max_col (Utils.rect_col_end command.bounding_box) in
                    let visible_col_start, visible_col_end = Utils.visible_col_range
                      ~box:command.bounding_box
                      ~scissor:!scissor_box
                      ~limit:!max_col in
                    let cursor = ref col_start in
                    let graphemes = String.into_grapheme_iter line |> Std.Iter.Iterator.to_list in
                    List.for_each graphemes
                      ~fn:(fun grapheme ->
                        let grapheme_string = Std.Unicode.Grapheme.to_string grapheme in
                        let grapheme_width = Std.Unicode.Grapheme.width grapheme in
                        let next_col = !cursor + grapheme_width in
                        if next_col <= visible_col_start then
                          cursor := next_col
                        else if next_col > visible_col_end || next_col > col_end then
                          ()
                        else if !cursor >= visible_col_start then
                          begin
                            if !cursor < !max_col then
                              write_text_cell
                                (get_cell grid ~row ~col:!cursor)
                                tty_color
                                weight
                                decoration
                                grapheme_string
                                grapheme_width;
                            if grapheme_width > 1 then
                              for offset = 1 to grapheme_width - 1 do
                                let col = !cursor + offset in
                                if col < !max_col then
                                  write_continuation_cell (get_cell grid ~row ~col)
                              done;
                            cursor := next_col
                          end
                        else
                          cursor := next_col))
          | Render.Custom { data } ->
              let lines = String.split_on_char '\n' data in
              let row_start = Utils.rect_row_start command.bounding_box in
              let row_end = Int.min !max_row (Utils.rect_row_end command.bounding_box) in
              let col_start = Utils.rect_col_start command.bounding_box in
              lines |> List.enumerate |> List.for_each
                ~fn:(fun (line_index, line) ->
                  let row = row_start + line_index in
                  if row < row_end then
                    let visible_col_start, visible_col_end = Utils.visible_col_range
                      ~box:command.bounding_box
                      ~scissor:!scissor_box
                      ~limit:!max_col in
                    if visible_col_end > visible_col_start then
                      let clipped = Utils.slice_text_by_cells
                        line
                        ~skip:(visible_col_start - col_start)
                        ~take:(visible_col_end - visible_col_start) in
                      if clipped != "" then
                        custom_segments := {
                          row;
                          col = visible_col_start;
                          command_order = index;
                          data = clipped
                        }
                        :: !custom_segments));
      let rows =
        List.fold_left !custom_segments ~init:[] ~fn:(fun acc segment -> segment :: acc)
        |> List.sort
          ~compare:(fun left right ->
            let by_row = Int.compare left.row right.row in
            match by_row with
            | Order.LT
            | Order.GT -> by_row
            | Order.EQ ->
                let by_col = Int.compare left.col right.col in
                (
                  match by_col with
                  | Order.LT
                  | Order.GT -> by_col
                  | Order.EQ -> Int.compare left.command_order right.command_order
                ))
      in
      let render_grid_segment buffer row from_col to_col =
        for col = from_col to to_col - 1 do
          if col >= 0 && col < !max_col then
            Buffer.add_string buffer (cell_to_string (get_cell grid ~row ~col))
        done
      in
      let rec render_row_segments = fun buffer row cursor ->
        function
        | [] -> render_grid_segment buffer row cursor !max_col
        | segment :: rest ->
            let segment_col = Int.max 0 segment.col in
            if segment_col > cursor then
              render_grid_segment buffer row cursor segment_col;
            Buffer.add_string buffer segment.data;
            let next_cursor = Int.max cursor (segment_col + Tty.Escape_seq.width segment.data) in
            render_row_segments buffer row next_cursor rest
      in
      let buf = Buffer.create ~size:(!max_row * !max_col * 2) in
      for row = 0 to !max_row - 1 do
        if row > 0 then
          Buffer.add_char buf '\n';
        let row_segments =
          List.filter rows ~fn:(fun segment -> segment.row = row)
        in
        render_row_segments buf row 0 row_segments;
        Buffer.add_string buf (Tty.Escape_seq.erase_line_seq 0)
      done;
      Buffer.contents buf

let render = fun commands -> print (render_to_string commands)
