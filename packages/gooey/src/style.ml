open Std

type color = Colors.rgb

type direction =
  | LeftToRight
  | TopToBottom

type sizing_type =
  | Fit
  | Grow
  | Fixed of float
  | Percent of float

type sizing = {
  width: sizing_type;
  height: sizing_type;
  min_width: float option;
  max_width: float option;
  min_height: float option;
  max_height: float option;
}

type h_align =
  | Left
  | Center
  | Right

type v_align =
  | Top
  | Middle
  | Bottom

type alignment = {
  x: h_align;
  y: v_align;
}

type padding = { left: int; right: int; top: int; bottom: int }

type margin = { left: int; right: int; top: int; bottom: int }

type overflow =
  | Visible
  | Clip

type text_wrap =
  | Words
  | NoWrap
  | Character

type text_align =
  | TextLeft
  | TextCenter
  | TextRight

type font_weight =
  | Normal
  | Bold

type text_decoration =
  | NoDecoration
  | Underline
  | Strikethrough

type corner_radius = { top_left: float; top_right: float; bottom_left: float; bottom_right: float }

type t = {
  direction: direction;
  sizing: sizing;
  grow_weight: float;
  alignment: alignment;
  child_gap: int;
  padding: padding;
  margin: margin;
  overflow: overflow;
  background: color option;
  foreground: color option;
  border_width: int;
  border_color: color option;
  corner_radius: corner_radius;
  text_size: int;
  text_wrap: text_wrap;
  text_align: text_align;
  font_weight: font_weight;
  text_decoration: text_decoration;
  z_index: int;
}

let empty = {
  direction = LeftToRight;
  sizing =
    {
      width = Fit;
      height = Fit;
      min_width = None;
      max_width = None;
      min_height = None;
      max_height = None;
    };
  grow_weight = 1.0;
  alignment = { x = Left; y = Top };
  child_gap = 0;
  padding =
    {
      left = 0;
      right = 0;
      top = 0;
      bottom = 0;
    };
  margin =
    {
      left = 0;
      right = 0;
      top = 0;
      bottom = 0;
    };
  overflow = Visible;
  background = None;
  foreground = None;
  border_width = 0;
  border_color = None;
  corner_radius =
    {
      top_left = 0.0;
      top_right = 0.0;
      bottom_left = 0.0;
      bottom_right = 0.0;
    };
  text_size = 12;
  text_wrap = Words;
  text_align = TextLeft;
  font_weight = Normal;
  text_decoration = NoDecoration;
  z_index = 0;
}

let row = fun t -> { t with direction = LeftToRight }

let column = fun t -> { t with direction = TopToBottom }

let size = fun ~width ~height t -> { t with sizing = { t.sizing with width; height } }

let width = fun w t -> { t with sizing = { t.sizing with width = w } }

let height = fun h t -> { t with sizing = { t.sizing with height = h } }

let min_width = fun w t -> { t with sizing = { t.sizing with min_width = Some w } }

let max_width = fun w t -> { t with sizing = { t.sizing with max_width = Some w } }

let min_height = fun h t -> { t with sizing = { t.sizing with min_height = Some h } }

let max_height = fun h t -> { t with sizing = { t.sizing with max_height = Some h } }

let padding = fun p t -> { t with padding = p }

let margin = fun m t -> { t with margin = m }

let overflow = fun overflow t -> { t with overflow }

let clip = fun t -> { t with overflow = Clip }

let bg = fun color t -> { t with background = Some color }

let fg = fun color t -> { t with foreground = Some color }

let border = fun
  ?(width = 1)
  ?color
  ?(radius = {top_left = 0.0; top_right = 0.0; bottom_left = 0.0; bottom_right = 0.0})
  ()
  t ->
  { t with border_width = width; border_color = color; corner_radius = radius }

let text_size = fun size t -> { t with text_size = size }

let bold = fun t -> { t with font_weight = Bold }

let underline = fun t -> { t with text_decoration = Underline }

let align = fun ~x ~y t -> { t with alignment = { x; y } }

let align_left = fun t -> { t with alignment = { t.alignment with x = Left } }

let align_center = fun t -> { t with alignment = { t.alignment with x = Center } }

let align_right = fun t -> { t with alignment = { t.alignment with x = Right } }

let grow_weight = fun weight t ->
  let weight =
    if (
      match Float.compare weight 0.0 with
      | Order.LT -> true
      | Order.EQ
      | Order.GT -> false
    ) then
      0.0
    else
      weight
  in
  { t with grow_weight = weight }

let grow = fun t -> { t with sizing = { t.sizing with width = Grow; height = Grow } }

let fixed = fun ~width ~height t -> {
  t with
  sizing = { t.sizing with width = Fixed width; height = Fixed height };
}

let text_wrap = fun wrap t -> { t with text_wrap = wrap }

let text_align = fun align t -> { t with text_align = align }

let child_gap = fun gap t -> { t with child_gap = gap }

let z_index = fun z t -> { t with z_index = z }

let strikethrough = fun t -> { t with text_decoration = Strikethrough }

module Padding = struct
  let make ?(left = 0) ?(right = 0) ?(top = 0) ?(bottom = 0) (): padding = {
    left;
    right;
    top;
    bottom;
  }

  let all n: padding = {
    left = n;
    right = n;
    top = n;
    bottom = n;
  }

  let symmetric ~h ~v : padding = {
    left = h;
    right = h;
    top = v;
    bottom = v;
  }

  let empty: padding = {
    left = 0;
    right = 0;
    top = 0;
    bottom = 0;
  }
end

module Margin = struct
  let make ?(left = 0) ?(right = 0) ?(top = 0) ?(bottom = 0) (): margin = {
    left;
    right;
    top;
    bottom;
  }

  let all n: margin = {
    left = n;
    right = n;
    top = n;
    bottom = n;
  }

  let symmetric ~h ~v : margin = {
    left = h;
    right = h;
    top = v;
    bottom = v;
  }

  let empty: margin = {
    left = 0;
    right = 0;
    top = 0;
    bottom = 0;
  }
end

module CornerRadius = struct
  let make = fun
    ?(top_left = 0.0) ?(top_right = 0.0) ?(bottom_left = 0.0) ?(bottom_right = 0.0) () ->
    {
      top_left;
      top_right;
      bottom_left;
      bottom_right;
    }

  let all = fun r ->
    {
      top_left = r;
      top_right = r;
      bottom_left = r;
      bottom_right = r;
    }

  let zero = {
    top_left = 0.0;
    top_right = 0.0;
    bottom_left = 0.0;
    bottom_right = 0.0;
  }
end

(* Note: italic not implemented - terminal support is limited *)

let italic = bold

(* Color helper - parse hex color strings *)

let color = fun hex_str ->
  let hex = String.trim hex_str in
  let hex =
    if String.starts_with ~prefix:"#" hex then
      String.sub hex ~offset:1 ~len:(String.length hex - 1)
    else
      hex
  in
  match String.length hex with
  | 3 ->
      (* Short form like "F00" -> "FF0000" *)
      let r = String.make ~len:2 ~char:(String.unsafe_get hex 0) in
      let g = String.make ~len:2 ~char:(String.unsafe_get hex 1) in
      let b = String.make ~len:2 ~char:(String.unsafe_get hex 2) in
      let r_val = Int.from_string ("0x" ^ r) in
      let g_val = Int.from_string ("0x" ^ g) in
      let b_val = Int.from_string ("0x" ^ b) in
      `rgb (r_val, g_val, b_val)
  | 6 ->
      (* Full form like "FF0000" *)
      let r = String.sub hex ~offset:0 ~len:2 in
      let g = String.sub hex ~offset:2 ~len:2 in
      let b = String.sub hex ~offset:4 ~len:2 in
      let r_val = Int.from_string ("0x" ^ r) in
      let g_val = Int.from_string ("0x" ^ g) in
      let b_val = Int.from_string ("0x" ^ b) in
      `rgb (r_val, g_val, b_val)
  | _ -> raise (Invalid_argument ("Invalid hex color: " ^ hex_str ^ " (expected #RGB or #RRGGBB)"))
