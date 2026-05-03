(**
   Mermaid flowchart document builder.

   Generate Mermaid diagram format for rendering in browsers, Markdown, and
   documentation tools.

   ## Examples

   ```ocaml
   open Std

   (* Create a flowchart *)
   let diagram = Graph.Mermaid.create ~direction:LR ()
     |> Graph.Mermaid.add_node ~id:"start" ~label:"Start" ~shape:Circle ()
     |> Graph.Mermaid.add_node ~id:"process" ~label:"Process Data" ~shape:Rectangle ()
     |> Graph.Mermaid.add_node ~id:"decision" ~label:"Valid?" ~shape:Diamond ()
     |> Graph.Mermaid.add_node ~id:"end" ~label:"End" ~shape:Circle ()
     |> Graph.Mermaid.add_edge ~from_node:"start" ~to_node:"process" ()
     |> Graph.Mermaid.add_edge ~from_node:"process" ~to_node:"decision" ()
     |> Graph.Mermaid.add_edge ~from_node:"decision" ~to_node:"end"
         ~label:"Yes" ~style:Solid ()
     |> Graph.Mermaid.add_edge ~from_node:"decision" ~to_node:"process"
         ~label:"No" ~style:Dotted () in

   let mermaid = Graph.Mermaid.to_string diagram
   (* graph LR
        start((Start))
        process[Process Data]
        decision{Valid?}
        end((End))
        start --> process
        process --> decision
        decision -->|Yes| end
        decision -.->|No| process *)
   ```

   In Markdown:

   ```markdown
   ```mermaid
   graph TD
       A[Start] --> B{Decision}
       B -->|Yes| C[Action 1]
       B -->|No| D[Action 2]
   ```
   ```

   ## Directions

   - **LR**: Left to Right (horizontal flow)
   - **TD/TB**: Top to Down/Bottom (vertical flow)
   - **RL**: Right to Left
   - **BT**: Bottom to Top

   ## Node Shapes

   - **Rectangle**: Default box `[text]`
   - **Round**: Rounded `(text)`
   - **Circle**: Circle `((text))`
   - **Diamond**: Decision `{text}`
   - **Hexagon**: Hexagon `{{text}}`
   - **Stadium**: Pill shape `([text])`

   ## Edge Styles

   - **Solid**: Normal arrow `-->`
   - **Dotted**: Dotted arrow `-.->` (optional/error paths)
   - **Thick**: Thick arrow `==>` (primary path)

   ## When to Use

   - Documentation embedded in Markdown/MDX
   - GitHub README diagrams
   - Browser-based interactive diagrams
   - GitBook, Docusaurus, Astro documentation

   See [Dot] for Graphviz format (better for complex graphs).
*)

(** Direction of graph layout *)
type direction =
  (** Top to Down *)
  | TD
  (** Top to Bottom (same as TD) *)
  | TB
  (** Bottom to Top *)
  | BT
  (** Right to Left *)
  | RL
  (** Left to Right *)
  | LR
(** Node shapes available in Mermaid *)
type node_shape =
  (** [text] - Default rectangle *)
  | Rectangle
  (** (text) - Rounded edges *)
  | Round
  (** ([text]) - Stadium-shaped *)
  | Stadium
  (** [[text]] - Subroutine shape *)
  | Subroutine
  (** [(text)] - Cylindrical/database shape *)
  | Cylindrical
  (** ((text)) - Circle *)
  | Circle
  (** {text} - Diamond/rhombus *)
  | Diamond
  (** {{text}} - Hexagon *)
  | Hexagon
  (** [/text/] - Parallelogram *)
  | Parallelogram
  (** [\text/] - Trapezoid *)
  | Trapezoid
(** Node with label and shape. *)
type node = {
  id: string;
  label: string;
  shape: node_shape;
}
type edge_style =
  (** --> Normal arrow *)
  | Solid
  (** -.-> Dotted arrow *)
  | Dotted
  (** ==> Thick arrow *)
  | Thick
(** Edge with optional label and style. *)
type edge = {
  from_node: string;
  to_node: string;
  label: string option;
  style: edge_style;
}
(** Mermaid diagram representation. *)
type t = {
  direction: direction;
  nodes: node list;
  edges: edge list;
}

(** Create a new Mermaid graph with optional direction (default: TD). *)
val create: ?direction:direction -> unit -> t

(** Add a node to the graph. *)
val add_node: t -> id:string -> label:string -> ?shape:node_shape -> unit -> t

(** Add an edge between two nodes. *)
val add_edge:
  t ->
  from_node:string ->
  to_node:string ->
  ?label:string ->
  ?style:edge_style ->
  unit ->
  t

(** Convert to Mermaid diagram string. *)
val to_string: t -> string
