open Std

type t = {
  width : int;
  height : int;
  y_offset : int;
  lines : string list;
  mouse_wheel_enabled : bool;
  mouse_wheel_delta : int;
}

let make ~width ~height =
  {
    width;
    height;
    y_offset = 0;
    lines = [];
    mouse_wheel_enabled = true;
    mouse_wheel_delta = 3;
  }

let set_content t content =
  let lines = String.split_on_char '\n' content in
  let max_offset = Int.max 0 (List.length lines - t.height) in
  let y_offset = Int.min t.y_offset max_offset in
  { t with lines; y_offset }

let get_content t = String.concat "\n" t.lines

let total_lines t = List.length t.lines

let max_y_offset t = Int.max 0 (List.length t.lines - t.height)

let visible_lines t =
  let start_idx = t.y_offset in
  let end_idx = Int.min (start_idx + t.height) (List.length t.lines) in
  end_idx - start_idx

let set_width t width = { t with width }
let set_height t height = { t with height }
let width t = t.width
let height t = t.height
let y_offset t = t.y_offset

let set_y_offset t offset =
  let clamped = Int.max 0 (Int.min offset (max_y_offset t)) in
  { t with y_offset = clamped }

let at_top t = t.y_offset <= 0
let at_bottom t = t.y_offset >= max_y_offset t

let scroll_up t n =
  if at_top t || n <= 0 then t
  else set_y_offset t (t.y_offset - n)

let scroll_down t n =
  if at_bottom t || n <= 0 then t
  else set_y_offset t (t.y_offset + n)

let page_up t = scroll_up t t.height
let page_down t = scroll_down t t.height
let half_page_up t = scroll_up t (t.height / 2)
let half_page_down t = scroll_down t (t.height / 2)
let goto_top t = set_y_offset t 0
let goto_bottom t = set_y_offset t (max_y_offset t)

let scroll_percent t =
  if t.height >= List.length t.lines then 1.0
  else
    let y = float_of_int t.y_offset in
    let h = float_of_int t.height in
    let total = float_of_int (List.length t.lines) in
    let percent = y /. (total -. h) in
    Float.max 0.0 (Float.min 1.0 percent)

let set_mouse_wheel_enabled t enabled = { t with mouse_wheel_enabled = enabled }
let set_mouse_wheel_delta t delta = { t with mouse_wheel_delta = delta }

let view t =
  let start_idx = t.y_offset in
  let end_idx = Int.min (start_idx + t.height) (List.length t.lines) in
  let visible = List.filteri (fun i _ -> i >= start_idx && i < end_idx) t.lines in
  String.concat "\n" visible


