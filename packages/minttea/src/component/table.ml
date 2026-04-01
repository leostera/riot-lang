open Std
open Std.IO
open Std.Collections

type column = {
  title: string;
  width: int;
}

type row = string list

type t = {
  columns: column list;
  rows: row list;
  cursor: int;
  focused: bool;
  height: int;  (* 0 = unlimited *)
  width: int;  (* total width *)
  show_header: bool;
  cursor_char: string;
}

let column = fun ~title ~width -> { title; width }

let make = fun columns rows ->
  {
    columns;
    rows;
    cursor = 0;
    focused = false;
    height = 0;
    width = 0;
    show_header = true;
    cursor_char = "> ";
  }

let set_height = fun t ~height:h -> { t with height = max 0 h }

let set_width = fun t ~width:w -> { t with width = max 0 w }

let set_show_header = fun t ~show -> { t with show_header = show }

let set_cursor_char = fun t ~char:c -> { t with cursor_char = c }

let columns = fun t -> t.columns

let rows = fun t -> t.rows

let set_columns = fun t ~columns:cols -> { t with columns = cols }

let set_rows = fun t ~rows ->
  let cursor =
    if t.cursor >= List.length rows then
      max 0 (List.length rows - 1)
    else
      t.cursor
  in
  { t with rows; cursor }

let selected_row = fun t ->
  if List.length t.rows = 0 then
    None
  else
    List.nth_opt t.rows t.cursor

let selected_index = fun t ->
  if List.length t.rows = 0 then
    None
  else
    Some t.cursor

let cursor = fun t -> t.cursor

let clamp_cursor = fun t ->
  let len = List.length t.rows in
  if len = 0 then
    { t with cursor = 0 }
  else
    { t with cursor = max 0 (min (len - 1) t.cursor) }

let select = fun t idx -> { t with cursor = idx } |> clamp_cursor

let move_up = fun t n -> { t with cursor = t.cursor - n } |> clamp_cursor

let move_down = fun t n -> { t with cursor = t.cursor + n } |> clamp_cursor

let goto_top = fun t -> { t with cursor = 0 }

let goto_bottom = fun t ->
  let len = List.length t.rows in
  if len = 0 then
    t
  else
    { t with cursor = len - 1 }

let focus = fun t -> { t with focused = true }

let blur = fun t -> { t with focused = false }

let is_focused = fun t -> t.focused

let handle_key = fun t (key: Event.key) modifier ->
  if not t.focused then
    t
  else
    let open Event in
      let page_size =
        if t.height > 0 then
          t.height
        else
          10
      in
      let half_page = page_size / 2 in
      match ((key: Event.key)) with
      | Up
      | Key "k" when modifier = NoModifier -> move_up t 1
      | Down
      | Key "j" when modifier = NoModifier -> move_down t 1
      | PageUp
      | Key "b" when modifier = NoModifier -> move_up t page_size
      | PageDown
      | Key "f" when modifier = NoModifier -> move_down t page_size
      | Space -> move_down t page_size
      | Key "u" when modifier = Ctrl || modifier = NoModifier -> move_up t half_page
      | Key "d" when modifier = Ctrl || modifier = NoModifier -> move_down t half_page
      | Home
      | Key "g" when modifier = NoModifier -> goto_top t
      | End
      | Key "G" when modifier = Shift -> goto_bottom t
      | _ -> t

(* Rendering helpers *)

let pad_string = fun s width ->
  let len = String.length s in
  if len >= width then
    if width > 0 then
      String.sub s 0 width
    else
      s
  else
    s ^ String.make (width - len) ' '

let truncate_string = fun s width ->
  let len = String.length s in
  if len <= width then
    s
  else if width > 3 then
    String.sub s 0 (width - 3) ^ "..."
  else
    String.sub s 0 width

let render_cell = fun content width ->
  let truncated = truncate_string content width in
  pad_string truncated width

let render_separator = fun (columns: column list) ->
  let parts =
    List.map
      (fun (col: column) ->
        String.make col.width '-')
      columns
  in
  String.concat "  " parts

let render_header = fun (columns: column list) ->
  let headers =
    List.map (fun (col: column) -> render_cell col.title col.width) columns
  in
  String.concat "  " headers

let render_row = fun (columns: column list) row_data is_selected cursor_char ->
  let prefix =
    if is_selected then
      cursor_char
    else
      String.make (String.length cursor_char) ' '
  in
  let cells =
    List.map2 (fun (col: column) cell -> render_cell cell col.width) columns row_data
  in
  prefix ^ String.concat "  " cells

let view = fun tbl ->
  let module B = Buffer in
  let buf = B.create 256 in
  (* Render header *)
  if tbl.show_header && List.length tbl.columns > 0 then
    begin
      B.add_string buf (render_header tbl.columns);
      B.add_char buf '\n';
      B.add_string buf (render_separator tbl.columns);
      B.add_char buf '\n'
    end;
  let total_rows = List.length tbl.rows in
  if total_rows = 0 then
    B.add_string buf "No data"
  else
    begin
      let start_idx, end_idx =
        if tbl.height = 0 then
          (0, total_rows - 1)
        else
          (* Ensure cursor is visible *)
          let start = max 0 (min (total_rows - tbl.height) (tbl.cursor - tbl.height / 2)) in
          (start, min (total_rows - 1) (start + tbl.height - 1))
      in
      (* Render visible rows *)
      let first_row = ref true in
      List.iteri
        (fun idx row_data ->
          if idx >= start_idx && idx <= end_idx then
            begin
              (* Pad row to match column count *)
              let padded_row =
                let col_count = List.length tbl.columns in
                let row_len = List.length row_data in
                if row_len < col_count then
                  let padding = Array.to_list (Array.make (col_count - row_len) "") in
                  row_data @ padding
                else if row_len > col_count then
                  List.take col_count row_data
                else
                  row_data
              in
              if not !first_row then
                B.add_char buf '\n';
              first_row := false;
              let is_selected = tbl.focused && idx = tbl.cursor in
              B.add_string buf (render_row tbl.columns padded_row is_selected tbl.cursor_char)
            end)
        tbl.rows
    end;
    B.contents buf
