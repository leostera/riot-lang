open Global
open Collections

(** Mermaid diagram format generation *)
type direction =
  | TD
  | TB
  | BT
  | RL
  | LR

type node_shape =
  | Rectangle
  (* [text] *)
  | Round
  (* (text) *)
  | Stadium
  (* ([text]) *)
  | Subroutine
  (* [[text]] *)
  | Cylindrical
  (* [(text)] *)
  | Circle
  (* ((text)) *)
  | Diamond
  (* {text} *)
  | Hexagon
  (* {{text}} *)
  | Parallelogram
  (* [/text/] *)
  | Trapezoid

(* [\text/] *)
type node = { id: string; label: string; shape: node_shape }

type edge_style =
  | Solid
  | Dotted
  | Thick

type edge = { from_node: string; to_node: string; label: string option; style: edge_style }

type t = { direction: direction; nodes: node list; edges: edge list }

let create = fun ?(direction = TD) () -> { direction; nodes = []; edges = [] }

let add_node = fun t ~id ~label ?(shape = Rectangle) () ->
  let node = { id; label; shape } in { t with nodes = node :: t.nodes }

let add_edge = fun t ~from_node ~to_node ?label ?(style = Solid) () ->
  let edge = {
    from_node;
    to_node;
    label;
    style
  }
  in
  { t with edges = edge :: t.edges }

let direction_to_string = function
  | TD -> "TD"
  | TB -> "TB"
  | BT -> "BT"
  | RL -> "RL"
  | LR -> "LR"

let format_node = fun node ->
  let open_bracket, close_bracket =
    match node.shape with
    | Rectangle -> "[", "]"
    | Round -> "(", ")"
    | Stadium -> "([", "])"
    | Subroutine -> "[[", "]]"
    | Cylindrical -> "[(", ")]"
    | Circle -> "((", "))"
    | Diamond -> "{", "}"
    | Hexagon -> "{{", "}}"
    | Parallelogram -> "[/", "/]"
    | Trapezoid -> "[\\", "/]"
  in
  "  " ^ node.id ^ open_bracket ^ "\"" ^ node.label ^ "\"" ^ close_bracket

let format_edge = fun edge ->
  let arrow =
    match edge.style with
    | Solid -> "-->"
    | Dotted -> "-.->"
    | Thick -> "==>"
  in
  match edge.label with
  | None -> "  " ^ edge.from_node ^ " " ^ arrow ^ " " ^ edge.to_node
  | Some label -> "  " ^ edge.from_node ^ " " ^ arrow ^ "|" ^ label ^ "| " ^ edge.to_node

let to_string = fun t ->
  let buffer = StringBuilder.create ~size:1_024 in
  (* Add graph direction *)
  StringBuilder.add_string buffer ("graph " ^ direction_to_string t.direction ^ "\n");
  (* Add nodes *)
  List.for_each (List.reverse t.nodes) ~fn:(
    fun node ->
      StringBuilder.add_string buffer (format_node node);
      StringBuilder.add_string buffer "\n"
  );
  (* Add blank line if we have both nodes and edges *)
  if t.nodes != [] && t.edges != [] then
    StringBuilder.add_string buffer "\n";
  List.for_each (List.reverse t.edges) ~fn:(
    fun edge ->
      StringBuilder.add_string buffer (format_edge edge);
      StringBuilder.add_string buffer "\n"
  );
  StringBuilder.contents buffer
