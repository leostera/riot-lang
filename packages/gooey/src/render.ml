open Std

type border_width = { left: int; right: int; top: int; bottom: int }

type rectangle_data = { color: Colors.rgb; corner_radius: Style.corner_radius }

type text_data = {
  content: string;
  color: Colors.rgb;
  size: int;
  weight: Style.font_weight;
  decoration: Style.text_decoration;
}

type border_data = { width: border_width; color: Colors.rgb; corner_radius: Style.corner_radius }

type command_type =
  | Rectangle of rectangle_data
  | Text of text_data
  | Border of border_data
  | ScissorStart of Geometry.Rect.t
  | ScissorEnd
  | Custom of { data: string }

type command = { bounding_box: Geometry.Rect.t; command_type: command_type; z_index: int }

type command_list = command list
