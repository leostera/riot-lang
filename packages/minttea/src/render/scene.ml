open Std

type rect = {
  x : int;
  y : int;
  width : int;
  height : int;
}

type style_attrs = {
  fg : Tty.Color.t option;
  bg : Tty.Color.t option;
  bold : bool;
  italic : bool;
  underline : bool;
  strikethrough : bool;
  reverse : bool;
}

let default_style = {
  fg = None;
  bg = None;
  bold = false;
  italic = false;
  underline = false;
  strikethrough = false;
  reverse = false;
}

type scene_content =
  | TextNode of {
      text : string;
      style : style_attrs;
    }
  | Container of {
      children : scene_node list;
      style : style_attrs option;  (** Optional background/styling for container *)
    }

and scene_node = {
  rect : rect;
  z_index : int;
  clip : rect option;
  content : scene_content;
}

let text_node ~rect ~z_index ~style text =
  { rect; z_index; clip = None; content = TextNode { text; style } }

let container ~rect ~z_index ?style children =
  { rect; z_index; clip = None; content = Container { children; style } }

let sort_by_z nodes =
  List.sort (fun a b -> Int.compare a.z_index b.z_index) nodes

let rec flatten node =
  match node.content with
  | TextNode _ -> [node]
  | Container { children; _ } ->
      (* Include the container itself for background painting, then its children *)
      let all_children = List.concat_map flatten children in
      node :: sort_by_z all_children
