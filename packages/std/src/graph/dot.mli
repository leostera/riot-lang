(** DOT graph format generation for Graphviz *)

type graph_style = Directed | Undirected

type node = {
  id : string;
  label : string option;
  attrs : (string * string) list;
}

type edge = {
  from_node : string;
  to_node : string;
  label : string option;
  attrs : (string * string) list;
}

type t = {
  name : string;
  style : graph_style;
  nodes : node list;
  edges : edge list;
  graph_attrs : (string * string) list;
}

val create : name:string -> style:graph_style -> t
(** Create an empty graph *)

val add_node :
  t -> id:string -> ?label:string -> ?attrs:(string * string) list -> unit -> t
(** Add a node to the graph *)

val add_edge :
  t ->
  from_node:string ->
  to_node:string ->
  ?label:string ->
  ?attrs:(string * string) list ->
  unit ->
  t
(** Add an edge to the graph *)

val to_string : t -> string
(** Convert graph to DOT format string *)
