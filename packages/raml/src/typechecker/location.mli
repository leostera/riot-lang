type t = { loc_start : position; loc_end : position }
and position = { pos_line : int; pos_col : int; pos_offset : int }

val none : t

val make :
  start_line:int ->
  start_col:int ->
  start_offset:int ->
  end_line:int ->
  end_col:int ->
  end_offset:int ->
  t

val to_string : t -> string
