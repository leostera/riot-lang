open Std

let test_create_defaults_to_top_down = fun _ctx ->
  let rendered =
    Graph.Mermaid.create ()
    |> Graph.Mermaid.to_string
  in
  if String.starts_with rendered ~prefix:"graph TD\n" then
    Ok ()
  else
    Error "Mermaid.create () should default to TD direction"

let test_create_with_direction_renders_requested_header = fun _ctx ->
  let rendered =
    Graph.Mermaid.create ~direction:Graph.Mermaid.LR ()
    |> Graph.Mermaid.to_string
  in
  if String.starts_with rendered ~prefix:"graph LR\n" then
    Ok ()
  else
    Error "Mermaid.create should render the requested direction"

let test_add_node_renders_expected_shapes = fun _ctx ->
  let graph = Graph.Mermaid.create () in
  let graph =
    Graph.Mermaid.add_node graph ~id:"rect" ~label:"Rect" ~shape:Graph.Mermaid.Rectangle ()
  in
  let graph =
    Graph.Mermaid.add_node graph ~id:"circle" ~label:"Circle" ~shape:Graph.Mermaid.Circle ()
  in
  let graph =
    Graph.Mermaid.add_node graph ~id:"diamond" ~label:"Decision" ~shape:Graph.Mermaid.Diamond ()
  in
  let rendered = Graph.Mermaid.to_string graph in
  if
    String.contains rendered "rect[\"Rect\"]"
    && String.contains rendered "circle((\"Circle\"))"
    && String.contains rendered "diamond{\"Decision\"}"
  then
    Ok ()
  else
    Error "Mermaid nodes should render their expected shape delimiters"

let test_default_edge_is_solid = fun _ctx ->
  let graph = Graph.Mermaid.create () in
  let graph = Graph.Mermaid.add_edge graph ~from_node:"A" ~to_node:"B" () in
  let rendered = Graph.Mermaid.to_string graph in
  if String.contains rendered "A --> B" then
    Ok ()
  else
    Error "Mermaid default edges should render with -->"

let test_dotted_edge_renders_dotted_arrow = fun _ctx ->
  let graph = Graph.Mermaid.create () in
  let graph =
    Graph.Mermaid.add_edge graph ~from_node:"A" ~to_node:"B" ~style:Graph.Mermaid.Dotted ()
  in
  let rendered = Graph.Mermaid.to_string graph in
  if String.contains rendered "A -.-> B" then
    Ok ()
  else
    Error "Dotted Mermaid edges should render with -.->"

let test_thick_edge_renders_thick_arrow = fun _ctx ->
  let graph = Graph.Mermaid.create () in
  let graph = Graph.Mermaid.add_edge graph ~from_node:"A" ~to_node:"B" ~style:Graph.Mermaid.Thick () in
  let rendered = Graph.Mermaid.to_string graph in
  if String.contains rendered "A ==> B" then
    Ok ()
  else
    Error "Thick Mermaid edges should render with ==>"

let test_labeled_edge_renders_inline_label = fun _ctx ->
  let graph = Graph.Mermaid.create () in
  let graph = Graph.Mermaid.add_edge graph ~from_node:"A" ~to_node:"B" ~label:"yes" () in
  let rendered = Graph.Mermaid.to_string graph in
  if String.contains rendered "A -->|yes| B" then
    Ok ()
  else
    Error "Mermaid edges should render labels inline"

let test_all_nodes_and_edges_appear_once = fun _ctx ->
  let graph = Graph.Mermaid.create ~direction:Graph.Mermaid.RL () in
  let graph = Graph.Mermaid.add_node graph ~id:"start" ~label:"Start" () in
  let graph = Graph.Mermaid.add_node graph ~id:"finish" ~label:"Finish" () in
  let graph = Graph.Mermaid.add_edge graph ~from_node:"start" ~to_node:"finish" () in
  let rendered = Graph.Mermaid.to_string graph in
  if
    String.contains rendered "start[\"Start\"]"
    && String.contains rendered "finish[\"Finish\"]"
    && String.contains rendered "start --> finish"
  then
    Ok ()
  else
    Error "Mermaid output should contain each added node and edge"

let tests =
  Test.[
    case "create defaults to top-down direction" test_create_defaults_to_top_down;
    case
      "create renders the requested direction"
      test_create_with_direction_renders_requested_header;
    case "nodes render their expected shapes" test_add_node_renders_expected_shapes;
    case "default edges render as solid arrows" test_default_edge_is_solid;
    case "dotted edges render dotted arrows" test_dotted_edge_renders_dotted_arrow;
    case "thick edges render thick arrows" test_thick_edge_renders_thick_arrow;
    case "labeled edges render inline labels" test_labeled_edge_renders_inline_label;
    case "all added nodes and edges appear in the output" test_all_nodes_and_edges_appear_once;
  ]

let main ~args = Test.Cli.main ~name:"graph_mermaid" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
