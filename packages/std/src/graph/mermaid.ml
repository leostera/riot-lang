open Global
(** Mermaid diagram format generation *)

type direction = TD | TB | BT | RL | LR

type node_shape =
  | Rectangle (* [text] *)
  | Round (* (text) *)
  | Stadium (* ([text]) *)
  | Subroutine (* [[text]] *)
  | Cylindrical (* [(text)] *)
  | Circle (* ((text)) *)
  | Diamond (* {text} *)
  | Hexagon (* {{text}} *)
  | Parallelogram (* [/text/] *)
  | Trapezoid (* [\text/] *)

type node = { id : string; label : string; shape : node_shape }
type edge_style = Solid | Dotted | Thick

type edge = {
  from_node : string;
  to_node : string;
  label : string option;
  style : edge_style;
}

type t = { direction : direction; nodes : node list; edges : edge list }

let create ?(direction = TD) () = { direction; nodes = []; edges = [] }

let add_node t ~id ~label ?(shape = Rectangle) () =
  let node = { id; label; shape } in
  { t with nodes = node :: t.nodes }

let add_edge t ~from_node ~to_node ?label ?(style = Solid) () =
  let edge = { from_node; to_node; label; style } in
  { t with edges = edge :: t.edges }

let direction_to_string = function
  | TD -> "TD"
  | TB -> "TB"
  | BT -> "BT"
  | RL -> "RL"
  | LR -> "LR"

let format_node node =
  match node.shape with
  | Rectangle -> format "  %s[\"%s\"]" node.id node.label
  | Round -> format "  %s(\"%s\")" node.id node.label
  | Stadium -> format "  %s([\"%s\"])" node.id node.label
  | Subroutine -> format "  %s[[\"%s\"]]" node.id node.label
  | Cylindrical -> format "  %s[(\"%s\")]" node.id node.label
  | Circle -> format "  %s((\"%s\"))" node.id node.label
  | Diamond -> format "  %s{\"%s\"}" node.id node.label
  | Hexagon -> format "  %s{{\"%s\"}}" node.id node.label
  | Parallelogram -> format "  %s[/\"%s\"/]" node.id node.label
  | Trapezoid -> format "  %s[\\\"%s\"/]" node.id node.label

let format_edge edge =
  let arrow =
    match edge.style with Solid -> "-->" | Dotted -> "-.->" | Thick -> "==>"
  in
  match edge.label with
  | None -> format "  %s %s %s" edge.from_node arrow edge.to_node
  | Some label ->
      format "  %s %s|%s| %s" edge.from_node arrow label edge.to_node

let to_string t =
  let buffer = Buffer.create 1024 in

  (* Add graph direction *)
  Buffer.add_string buffer
    (format "graph %s\n" (direction_to_string t.direction));

  (* Add nodes *)
  List.iter
    (fun node ->
      Buffer.add_string buffer (format_node node);
      Buffer.add_string buffer "\n")
    (List.rev t.nodes);

  (* Add blank line if we have both nodes and edges *)
  if t.nodes <> [] && t.edges <> [] then Buffer.add_string buffer "\n";

  (* Add edges *)
  List.iter
    (fun edge ->
      Buffer.add_string buffer (format_edge edge);
      Buffer.add_string buffer "\n")
    (List.rev t.edges);

  Buffer.contents buffer
