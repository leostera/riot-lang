open Std

type wrap_mode = [`None | `Soft]

type t = {
  width: int;
  height: int;
  y_offset: int;
  lines: string list;
  mouse_wheel_enabled: bool;
  mouse_wheel_delta: int;
  wrap_mode: wrap_mode;
}

let make = fun ~width ~height ->
  {
    width;
    height;
    y_offset = 0;
    lines = [];
    mouse_wheel_enabled = true;
    mouse_wheel_delta = 3;
    wrap_mode = `None
  }

let set_content = fun t ~content ->
  let lines = String.split_on_char '\n' content in
  let max_offset = Int.max 0 (List.length lines - t.height) in
  let y_offset = Int.min t.y_offset max_offset in { t with lines; y_offset }

let get_content = fun t -> String.concat "\n" t.lines

let total_lines = fun t ->
  match t.wrap_mode with
  | `None -> List.length t.lines
  | `Soft -> (* Count wrapped lines *)
  List.fold_left t.lines ~init:0 ~fn:(
    fun acc line ->
      if line = "" then
        acc + 1
      else acc + List.length (Util.Ansi.word_wrap ~width:t.width line)
  )

let max_y_offset = fun t ->
  let effective_lines =
    match t.wrap_mode with
    | `None -> List.length t.lines
    | `Soft -> (* Count wrapped lines *)
    List.fold_left t.lines ~init:0 ~fn:(
      fun acc line ->
        if line = "" then
          acc + 1
        else acc + List.length (Util.Ansi.word_wrap ~width:t.width line)
    )
  in
  Int.max 0 (effective_lines - t.height)

let visible_lines = fun t ->
  let start_idx = t.y_offset in
  let end_idx = Int.min (start_idx + t.height) (List.length t.lines) in end_idx - start_idx

let set_width = fun t ~width -> { t with width }

let set_height = fun t ~height -> { t with height }

let width = fun t -> t.width

let height = fun t -> t.height

let y_offset = fun t -> t.y_offset

let set_wrap_mode = fun t ~mode -> { t with wrap_mode = mode }

let wrap_mode = fun t -> t.wrap_mode

let set_y_offset = fun t ~offset ->
  let clamped = Int.max 0 (Int.min offset (max_y_offset t)) in { t with y_offset = clamped }

let at_top = fun t -> t.y_offset <= 0

let at_bottom = fun t -> t.y_offset >= max_y_offset t

let scroll_up = fun t ~lines ->
  if at_top t || lines <= 0 then
    t
  else set_y_offset t ~offset:(t.y_offset - lines)

let scroll_down = fun t ~lines ->
  if at_bottom t || lines <= 0 then
    t
  else set_y_offset t ~offset:(t.y_offset + lines)

let page_up = fun t -> scroll_up t ~lines:t.height

let page_down = fun t -> scroll_down t ~lines:t.height

let half_page_up = fun t -> scroll_up t ~lines:(t.height / 2)

let half_page_down = fun t -> scroll_down t ~lines:(t.height / 2)

let goto_top = fun t -> set_y_offset t ~offset:0

let goto_bottom = fun t -> set_y_offset t ~offset:(max_y_offset t)

let scroll_percent = fun t ->
  if t.height >= List.length t.lines then
    1.0
  else
    let y = float_of_int t.y_offset in
    let h = float_of_int t.height in
    let total = float_of_int (List.length t.lines) in
    let percent = y /. (total -. h) in
    if percent < 0.0 then
      0.0
    else
      if percent > 1.0 then
        1.0
      else percent

let set_mouse_wheel_enabled = fun t ~enabled -> { t with mouse_wheel_enabled = enabled }

let set_mouse_wheel_delta = fun t ~delta -> { t with mouse_wheel_delta = delta }

let view = fun t ->
  (* Apply word wrapping if enabled *)
  let display_lines =
    match t.wrap_mode with
    | `None -> t.lines
    | `Soft -> (* Word wrap each line to fit width *)
    t.lines |> List.map ~fn:(
      fun line ->
        if line = "" then
          [ line ]
        (* Preserve blank lines *)
        else Util.Ansi.word_wrap ~width:t.width line
    ) |> List.concat
  in
  (* Extract visible portion based on scroll position *)
  let start_idx = t.y_offset in
  let end_idx = Int.min (start_idx + t.height) (List.length display_lines) in
  let visible = List.filter_map (List.enumerate display_lines) ~fn:(
    fun (i, line) ->
      if i >= start_idx && i < end_idx then
        Some line
      else None
  ) in
  (* Pad with blank lines if content is shorter than viewport height *)
  let visible_count = List.length visible in
  let padded_visible =
    if visible_count < t.height then
      let blank_lines = List.init ~count:(t.height - visible_count) ~fn:(
        fun _ -> ""
      ) in visible @ blank_lines
    else visible
  in
  String.concat "\n" padded_visible
