open Std

type t = { loc_start : position; loc_end : position }
and position = { pos_line : int; pos_col : int; pos_offset : int }

let none =
  let pos = { pos_line = 0; pos_col = 0; pos_offset = 0 } in
  { loc_start = pos; loc_end = pos }

let make ~start_line ~start_col ~start_offset ~end_line ~end_col ~end_offset =
  {
    loc_start =
      { pos_line = start_line; pos_col = start_col; pos_offset = start_offset };
    loc_end =
      { pos_line = end_line; pos_col = end_col; pos_offset = end_offset };
  }

let to_string loc =
  format "line %d, col %d - line %d, col %d" loc.loc_start.pos_line
    loc.loc_start.pos_col loc.loc_end.pos_line loc.loc_end.pos_col
