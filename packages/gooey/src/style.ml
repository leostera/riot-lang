open Std

type direction = 
  | LeftToRight 
  | TopToBottom

type sizing_type = 
  | Fit
  | Grow
  | Fixed of float
  | Percent of float

type sizing = {
  width : sizing_type;
  height : sizing_type;
  min_width : float option;
  max_width : float option;
  min_height : float option;
  max_height : float option;
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
  x : h_align;
  y : v_align;
}

type padding = {
  left : int;
  right : int;
  top : int;
  bottom : int;
}

type margin = {
  left : int;
  right : int;
  top : int;
  bottom : int;
}

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

type corner_radius = {
  top_left : float;
  top_right : float;
  bottom_left : float;
  bottom_right : float;
}

type t = {
  direction : direction;
  sizing : sizing;
  alignment : alignment;
  child_gap : int;
  padding : padding;
  margin : margin;
  background : Colors.rgb option;
  foreground : Colors.rgb option;
  border_width : int;
  border_color : Colors.rgb option;
  corner_radius : corner_radius;
  text_size : int;
  text_wrap : text_wrap;
  text_align : text_align;
  font_weight : font_weight;
  text_decoration : text_decoration;
  z_index : int;
}

let empty = {
  direction = LeftToRight;
  sizing = {
    width = Fit;
    height = Fit;
    min_width = None;
    max_width = None;
    min_height = None;
    max_height = None;
  };
  alignment = { x = Left; y = Top };
  child_gap = 0;
  padding = { left = 0; right = 0; top = 0; bottom = 0 };
  margin = { left = 0; right = 0; top = 0; bottom = 0 };
  background = None;
  foreground = None;
  border_width = 0;
  border_color = None;
  corner_radius = { top_left = 0.0; top_right = 0.0; bottom_left = 0.0; bottom_right = 0.0 };
  text_size = 12;
  text_wrap = Words;
  text_align = TextLeft;
  font_weight = Normal;
  text_decoration = NoDecoration;
  z_index = 0;
}

let row t = { t with direction = LeftToRight }
let column t = { t with direction = TopToBottom }

let size ~width ~height t = { t with sizing = { t.sizing with width; height } }
let width w t = { t with sizing = { t.sizing with width = w } }
let height h t = { t with sizing = { t.sizing with height = h } }

let min_width w t = { t with sizing = { t.sizing with min_width = Some w } }
let max_width w t = { t with sizing = { t.sizing with max_width = Some w } }
let min_height h t = { t with sizing = { t.sizing with min_height = Some h } }
let max_height h t = { t with sizing = { t.sizing with max_height = Some h } }

let padding p t = { t with padding = p }
let margin m t = { t with margin = m }

let bg color t = { t with background = Some color }
let fg color t = { t with foreground = Some color }

let border ?(width=1) ?color ?(radius={ top_left=0.0; top_right=0.0; bottom_left=0.0; bottom_right=0.0 }) () t =
  { t with border_width = width; border_color = color; corner_radius = radius }

let text_size size t = { t with text_size = size }
let bold t = { t with font_weight = Bold }
let underline t = { t with text_decoration = Underline }

let align ~x ~y t = { t with alignment = { x; y } }
let align_left t = { t with alignment = { t.alignment with x = Left } }
let align_center t = { t with alignment = { t.alignment with x = Center } }
let align_right t = { t with alignment = { t.alignment with x = Right } }

let grow t = { t with sizing = { t.sizing with width = Grow; height = Grow } }
let fixed ~width ~height t = { t with sizing = { t.sizing with width = Fixed width; height = Fixed height } }

let child_gap gap t = { t with child_gap = gap }
let z_index z t = { t with z_index = z }

module Padding = struct
  let make ?(left=0) ?(right=0) ?(top=0) ?(bottom=0) () : padding = 
    { left; right; top; bottom }
  let all n : padding = { left = n; right = n; top = n; bottom = n }
  let symmetric ~h ~v : padding = { left = h; right = h; top = v; bottom = v }
  let empty : padding = { left = 0; right = 0; top = 0; bottom = 0 }
end

module Margin = struct
  let make ?(left=0) ?(right=0) ?(top=0) ?(bottom=0) () : margin = 
    { left; right; top; bottom }
  let all n : margin = { left = n; right = n; top = n; bottom = n }
  let symmetric ~h ~v : margin = { left = h; right = h; top = v; bottom = v }
  let empty : margin = { left = 0; right = 0; top = 0; bottom = 0 }
end

module CornerRadius = struct
  let make ?(top_left=0.0) ?(top_right=0.0) ?(bottom_left=0.0) ?(bottom_right=0.0) () =
    { top_left; top_right; bottom_left; bottom_right }
  let all r = { top_left = r; top_right = r; bottom_left = r; bottom_right = r }
  let zero = { top_left = 0.0; top_right = 0.0; bottom_left = 0.0; bottom_right = 0.0 }
end

(* Note: italic not implemented - terminal support is limited *)
let italic = bold
