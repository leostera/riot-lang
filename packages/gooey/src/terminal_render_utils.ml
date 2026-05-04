open Std

let start_cell = fun value -> Int.max 0 (Float.to_int (Float.floor value))

let end_cell = fun value -> Int.max 0 (Float.to_int (Float.ceil value))

let rect_col_start = fun (rect: Geometry.Rect.t) -> start_cell rect.x

let rect_row_start = fun (rect: Geometry.Rect.t) -> start_cell rect.y

let rect_col_end = fun (rect: Geometry.Rect.t) -> end_cell (rect.x +. rect.width)

let rect_row_end = fun (rect: Geometry.Rect.t) -> end_cell (rect.y +. rect.height)

let rgb_to_color = fun (`rgb (r, g, b)) -> Tty.Color.from_rgb (r, g, b)

let is_inside_rect = fun ~col ~row rect ->
  col >= rect_col_start rect
  && col < rect_col_end rect
  && row >= rect_row_start rect
  && row < rect_row_end rect

let is_inside_scissor = fun ~col ~row scissor ->
  match scissor with
  | None -> true
  | Some rect -> is_inside_rect ~col ~row rect

let visible_col_range = fun ~box ~scissor ~limit ->
  let start_col = rect_col_start box in
  let end_col = Int.min limit (rect_col_end box) in
  match scissor with
  | None -> (start_col, end_col)
  | Some rect ->
      let visible_start = Int.max start_col (rect_col_start rect) in
      let visible_end = Int.min end_col (rect_col_end rect) in
      (visible_start, visible_end)

let slice_text_by_cells = fun text ~skip ~take ->
  if take <= 0 then
    ""
  else
    let graphemes =
      String.into_grapheme_iter text
      |> Std.Iter.Iterator.to_list
    in
    let rec loop col acc = fun __tmp1 ->
      match __tmp1 with
      | [] ->
          List.rev acc
          |> String.concat ""
      | grapheme :: rest ->
          let grapheme_string = Std.Unicode.Grapheme.to_string grapheme in
          let grapheme_width = Std.Unicode.Grapheme.width grapheme in
          let next_col = col + grapheme_width in
          if next_col <= skip then
            loop next_col acc rest
          else if col < skip then
            loop next_col acc rest
          else if next_col > skip + take then
            List.rev acc
            |> String.concat ""
          else
            loop next_col (grapheme_string :: acc) rest
    in
    loop 0 [] graphemes

let text_formats = fun ~color ~weight ~decoration ->
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
