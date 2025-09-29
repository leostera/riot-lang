(** Mermaid diagram format generation *)

(** Direction of graph layout *)
type direction =
  | TD  (** Top to Down *)
  | TB  (** Top to Bottom (same as TD) *)
  | BT  (** Bottom to Top *)
  | RL  (** Right to Left *)
  | LR  (** Left to Right *)

(** Node shapes available in Mermaid *)
type node_shape =
  | Rectangle  (** [text] - Default rectangle *)
  | Round  (** (text) - Rounded edges *)
  | Stadium  (** ([text]) - Stadium-shaped *)
  | Subroutine  (** [[text]] - Subroutine shape *)
  | Cylindrical  (** [(text)] - Cylindrical/database shape *)
  | Circle  (** ((text)) - Circle *)
  | Diamond  (** {text} - Diamond/rhombus *)
  | Hexagon  (** {{text}} - Hexagon *)
  | Parallelogram  (** [/text/] - Parallelogram *)
  | Trapezoid  (** [\text/] - Trapezoid *)

type node = { id : string; label : string; shape : node_shape }

type edge_style =
  | Solid  (** --> Normal arrow *)
  | Dotted  (** -.-> Dotted arrow *)
  | Thick  (** ==> Thick arrow *)

type edge = {
  from_node : string;
  to_node : string;
  label : string option;
  style : edge_style;
}

type t = { direction : direction; nodes : node list; edges : edge list }

val create : ?direction:direction -> unit -> t
(** Create a new Mermaid graph with optional direction (default: TD) *)

val add_node : t -> id:string -> label:string -> ?shape:node_shape -> unit -> t
(** Add a node to the graph *)

val add_edge :
  t ->
  from_node:string ->
  to_node:string ->
  ?label:string ->
  ?style:edge_style ->
  unit ->
  t
(** Add an edge between two nodes *)

val to_string : t -> string
(** Convert to Mermaid diagram string *)
