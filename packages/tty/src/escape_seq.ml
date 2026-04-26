open Std
open Std.IO

let csi = "\x1b["

let osc = "\x1b]"

(* Text attributes - these are just the codes without CSI *)

let reset_seq = "0"

let bold_seq = "1"

let faint_seq = "2"

let italics_seq = "3"

let underline_seq = "4"

let blink_seq = "5"

let reverse_seq = "7"

let cross_out_seq = "9"

let overline_seq = "53"

let foreground_seq = "38"

let background_seq = "48"

(* Helper to build escape sequences - now returns string instead of printing *)

let escape = fun code -> csi ^ code

(* Cursor positioning - all return strings *)

let cursor_up_seq = fun x -> escape (Int.to_string x ^ "A")

let cursor_down_seq = fun x -> escape (Int.to_string x ^ "B")

let cursor_forward_seq = fun x -> escape (Int.to_string x ^ "C")

let cursor_back_seq = fun x -> escape (Int.to_string x ^ "D")

let cursor_next_line_seq = fun x -> escape (Int.to_string x ^ "E")

let cursor_previous_line_seq = fun x -> escape (Int.to_string x ^ "F")

let cursor_horizontal_seq = fun x -> escape (Int.to_string x ^ "G")

let cursor_position_seq = fun x y -> escape (Int.to_string x ^ ";" ^ Int.to_string y ^ "H")

let erase_display_seq = fun x -> escape (Int.to_string x ^ "J")

let erase_line_seq = fun x -> escape (Int.to_string x ^ "K")

let scroll_up_seq = fun x -> escape (Int.to_string x ^ "S")

let scroll_down_seq = fun x -> escape (Int.to_string x ^ "T")

let save_cursor_position_seq = escape "s"

let restore_cursor_position_seq = escape "u"

let change_scrolling_region_seq = fun x y -> escape (Int.to_string x ^ ";" ^ Int.to_string y ^ "r")

let insert_line_seq = fun x -> escape (Int.to_string x ^ "L")

let delete_line_seq = fun x -> escape (Int.to_string x ^ "M")

(* Explicit values for EraseLineSeq *)

let erase_line_right_seq = escape "0K"

let erase_line_left_seq = escape "1K"

let erase_entire_line_seq = escape "2K"

(* Mouse - all return strings *)

let enable_mouse_press_seq = escape "?9h"

let disable_mouse_press_seq = escape "?9l"

let enable_mouse_seq = escape "?1000h"

let disable_mouse_seq = escape "?1000l"

let enable_mouse_hilite_seq = escape "?1001h"

let disable_mouse_hilite_seq = escape "?1001l"

let enable_mouse_cell_motion_seq = escape "?1002h"

let disable_mouse_cell_motion_seq = escape "?1002l"

let enable_mouse_all_motion_seq = escape "?1003h"

let disable_mouse_all_motion_seq = escape "?1003l"

let enable_mouse_extended_mode_seq = escape "?1006h"

let disable_mouse_extended_mode_seq = escape "?1006l"

let enable_mouse_pixels_mode_seq = escape "?1016h"

let disable_mouse_pixels_mode_seq = escape "?1016l"

(* Screen - all return strings *)

let restore_screen_seq = escape "?47l"

let save_screen_seq = escape "?47h"

let alt_screen_seq = escape "?1049h"

let exit_alt_screen_seq = escape "?1049l"

let reset_scroll_region_seq = escape "r"

(* Bracketed paste - all return strings *)

let enable_bracketed_paste_seq = escape "?2004h"

let disable_bracketed_paste_seq = escape "?2004l"

let start_bracketed_paste_seq = escape "200~"

let end_bracketed_paste_seq = escape "201~"

(* Focus tracking - all return strings *)

let enable_focus_events_seq = escape "?1004h"

let disable_focus_events_seq = escape "?1004l"

(* Kitty keyboard protocol - all return strings *)

let enable_kitty_keyboard_seq = escape ">1u"

let disable_kitty_keyboard_seq = escape "<u"

(* Synchronized output (reduces flicker) - all return strings *)

let begin_sync_seq = escape "?2026h"

let end_sync_seq = escape "?2026l"

(* Session - all return strings *)

let osc_sequence = fun code value -> osc ^ code ^ ";" ^ value ^ "\x07"

let set_window_title_seq = fun value -> osc_sequence "2" value

let set_foreground_color_seq = fun value -> osc_sequence "10" value

let set_background_color_seq = fun value -> osc_sequence "11" value

let set_cursor_color_seq = fun value -> osc_sequence "12" value

let show_cursor_seq = escape "?25h"

let hide_cursor_seq = escape "?25l"

(* Strip ANSI escape sequences from a string *)

let strip = fun str ->
  let buf = Buffer.create ~size:(String.length str) in
  let len = String.length str in
  let rec skip_csi j =
    if j >= len then
      len
    else
      let c = String.get_unchecked str ~at:j in
      if c >= '@' && c <= '~' then
        j + 1
      else
        skip_csi (j + 1)
  in
  let rec skip_osc j =
    if j >= len then
      len
    else
      let c = String.get_unchecked str ~at:j in
      if Char.equal c '\x07' then
        j + 1
      else if Char.equal c '\x1b' then
        if j + 1 < len && Char.equal (String.get_unchecked str ~at:(j + 1)) '\\' then
          j + 2
        else
          skip_osc (j + 1)
      else
        skip_osc (j + 1)
  in
  let rec scan i =
    if i >= len then
      Buffer.contents buf
    else if String.get_unchecked str ~at:i = '\x1b' then
      if i + 1 < len then
        match String.get_unchecked str ~at:(i + 1) with
        | '[' -> scan (skip_csi (i + 2))
        | ']' -> scan (skip_osc (i + 2))
        | _ -> scan (i + 1)
      else
        scan (i + 1)
    else (
      Buffer.add_char buf (String.get_unchecked str ~at:i);
      scan (i + 1)
    )
  in
  scan 0

(* Calculate display width ignoring ANSI codes *)

let width = fun str -> String.width (strip str)
