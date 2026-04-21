(** Shared terminal-cell helpers for Gooey's built-in renderers. *)
open Std

val start_cell: float -> int

val end_cell: float -> int

val rect_col_start: Geometry.Rect.t -> int

val rect_row_start: Geometry.Rect.t -> int

val rect_col_end: Geometry.Rect.t -> int

val rect_row_end: Geometry.Rect.t -> int

val rgb_to_color: Colors.rgb -> Tty.Color.t

val is_inside_rect: col:int -> row:int -> Geometry.Rect.t -> bool

val is_inside_scissor: col:int -> row:int -> Geometry.Rect.t option -> bool

val visible_col_range: box:Geometry.Rect.t -> scissor:Geometry.Rect.t option -> limit:int -> int * int

val slice_text_by_cells: string -> skip:int -> take:int -> string

val text_formats:
  color:Colors.rgb ->
  weight:Style.font_weight ->
  decoration:Style.text_decoration ->
  Ansi_formatter.format list
