(**
   Graph construction, manipulation, and export to visualization formats.

   Use `Graph` for DOT and Mermaid rendering helpers plus simple dependency
   graphs with topological sorting.

   ## Examples

   ```ocaml
   open Std

   (* Build a simple dependency graph *)
   let graph = Graph.SimpleGraph.make () in
   let node_a = Graph.SimpleGraph.add_node graph "A" in
   let node_b = Graph.SimpleGraph.add_node graph "B" in
   let node_c = Graph.SimpleGraph.add_node graph "C" in

   (* A depends on B, B depends on C *)
   Graph.SimpleGraph.add_edge node_a ~depends_on:node_b;
   Graph.SimpleGraph.add_edge node_b ~depends_on:node_c;

   (* Topological sort *)
   let sorted = Graph.SimpleGraph.topo_sort graph in
   (* [C; B; A] *)

   (* Export to DOT format *)
   let dot = Graph.Dot.create ~name:"deps" ~style:Directed
     |> Graph.Dot.add_node ~id:"A" ~label:"Module A" ()
     |> Graph.Dot.add_node ~id:"B" ~label:"Module B" ()
     |> Graph.Dot.add_edge ~from_node:"A" ~to_node:"B" () in

   let dot_string = Graph.Dot.to_string dot
   (* "digraph deps { A [label="Module A"]; ... }" *)

   (* Export to Mermaid format *)
   let mermaid = Graph.Mermaid.create ~direction:LR ()
     |> Graph.Mermaid.add_node ~id:"A" ~label:"Start" ~shape:Circle ()
     |> Graph.Mermaid.add_node ~id:"B" ~label:"Process" ()
     |> Graph.Mermaid.add_edge ~from_node:"A" ~to_node:"B" () in

   let mermaid_string = Graph.Mermaid.to_string mermaid
   (* "graph LR\n A((Start))\n A --> B\n ..." *)
   ```

   ## Modules

   - [Dot]: Generate Graphviz DOT format
   - [Mermaid]: Generate Mermaid.js diagram format
   - [SimpleGraph]: Basic graph with dependency tracking and topological sort

   ## When to Use

   - Visualizing module dependencies
   - Build system dependency graphs
   - Workflow and state machine diagrams
   - Any directed/undirected graph visualization
*)

(** DOT format generation for Graphviz. *)
module Dot = Dot

(** Mermaid diagram format generation. *)
module Mermaid = Mermaid

(** Simple dependency graph with topological sorting. *)
module SimpleGraph = Simple_graph
