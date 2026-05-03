(**
   Graphviz DOT document builder.

   Generate DOT format strings for rendering with Graphviz tools such as
   `dot`, `neato`, and `fdp`.

   ## Examples

   ```ocaml
   open Std

   (* Create a directed graph *)
   let graph = Graph.Dot.create ~name:"dependencies" ~style:Directed
     |> Graph.Dot.add_node ~id:"main" ~label:"main.ml" ()
     |> Graph.Dot.add_node ~id:"utils" ~label:"utils.ml"
         ~attrs:["color", "blue"; "shape", "box"] ()
     |> Graph.Dot.add_edge ~from_node:"main" ~to_node:"utils"
         ~label:"imports" () in

   let dot_string = Graph.Dot.to_string graph in
   (* digraph dependencies {
        main [label="main.ml"];
        utils [label="utils.ml", color="blue", shape="box"];
        main -> utils [label="imports"];
      } *)

   (* Write to file and render *)
   Fs.write (Path.v "graph.dot") dot_string |> ignore;
   (* Then: dot -Tpng graph.dot -o graph.png *)
   ```

   Undirected graph:

   ```ocaml
   let graph = Graph.Dot.create ~name:"network" ~style:Undirected
     |> Graph.Dot.add_node ~id:"A" ()
     |> Graph.Dot.add_node ~id:"B" ()
     |> Graph.Dot.add_edge ~from_node:"A" ~to_node:"B" ()
   (* Uses -- instead of -> for edges *)
   ```

   ## Attributes

   Common node attributes:
   - `shape`: box, circle, diamond, ellipse, plaintext
   - `color`: red, blue, "#ff0000"
   - `style`: filled, dashed, bold
   - `fontsize`: "12", "14"

   Common edge attributes:
   - `color`, `style`, `weight`, `arrowhead`

   See Graphviz documentation for complete attribute list.

   ## When to Use

   - Module dependency visualization
   - Call graphs and data flow diagrams
   - State machine diagrams
   - Any graph visualization with Graphviz
*)

(** Graph style - directed (->) or undirected (--). *)
type graph_style =
  | Directed
  | Undirected
(** Node with optional label and Graphviz attributes. *)
type node = {
  id: string;
  label: string option;
  attrs: (string * string) list;
}
(** Edge with optional label and Graphviz attributes. *)
type edge = {
  from_node: string;
  to_node: string;
  label: string option;
  attrs: (string * string) list;
}
(** DOT graph representation. *)
type t = {
  name: string;
  style: graph_style;
  nodes: node list;
  edges: edge list;
  graph_attrs: (string * string) list;
}

(** Create an empty graph. *)
val create: name:string -> style:graph_style -> t

(** Add a node to the graph. *)
val add_node: t -> id:string -> ?label:string -> ?attrs:(string * string) list -> unit -> t

(** Add an edge to the graph. *)
val add_edge:
  t ->
  from_node:string ->
  to_node:string ->
  ?label:string ->
  ?attrs:(string * string) list ->
  unit ->
  t

(** Convert graph to DOT format string. *)
val to_string: t -> string
