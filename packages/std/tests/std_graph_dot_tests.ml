open Std

let test_create_directed_graph_renders_digraph_header = fun _ctx ->
  let graph = Graph.Dot.create ~name:"deps" ~style:Graph.Dot.Directed in
  let rendered = Graph.Dot.to_string graph in
  if String.starts_with rendered ~prefix:"digraph deps {" then
    Ok ()
  else
    Error "Directed DOT graphs should render a digraph header"

let test_create_undirected_graph_renders_graph_header = fun _ctx ->
  let graph = Graph.Dot.create ~name:"network" ~style:Graph.Dot.Undirected in
  let rendered = Graph.Dot.to_string graph in
  if String.starts_with rendered ~prefix:"graph network {" then
    Ok ()
  else
    Error "Undirected DOT graphs should render a graph header"

let test_add_node_renders_label_and_attributes = fun _ctx ->
  let graph = Graph.Dot.create ~name:"deps" ~style:Graph.Dot.Directed in
  let graph =
    Graph.Dot.add_node
      graph
      ~id:"A"
      ~label:"Module A"
      ~attrs:[ ("color", "blue"); ("shape", "box"); ]
      ()
  in
  let rendered = Graph.Dot.to_string graph in
  if String.contains rendered "\"A\" [label=\"Module A\", color=\"blue\", shape=\"box\"];" then
    Ok ()
  else
    Error "DOT nodes should render labels and attributes"

let test_add_edge_renders_directed_arrow = fun _ctx ->
  let graph = Graph.Dot.create ~name:"deps" ~style:Graph.Dot.Directed in
  let graph = Graph.Dot.add_edge graph ~from_node:"A" ~to_node:"B" () in
  let rendered = Graph.Dot.to_string graph in
  if String.contains rendered "\"A\" -> \"B\";" then
    Ok ()
  else
    Error "Directed DOT graphs should render -> edges"

let test_add_edge_renders_undirected_arrow = fun _ctx ->
  let graph = Graph.Dot.create ~name:"deps" ~style:Graph.Dot.Undirected in
  let graph = Graph.Dot.add_edge graph ~from_node:"A" ~to_node:"B" () in
  let rendered = Graph.Dot.to_string graph in
  if String.contains rendered "\"A\" -- \"B\";" then
    Ok ()
  else
    Error "Undirected DOT graphs should render -- edges"

let test_graph_level_attributes_are_rendered = fun _ctx ->
  let graph = {
    (Graph.Dot.create ~name:"deps" ~style:Graph.Dot.Directed) with
    graph_attrs = [ ("rankdir", "LR"); ("label", "Dependency Graph"); ];
  }
  in
  let rendered = Graph.Dot.to_string graph in
  if
    String.contains rendered "rankdir=\"LR\";"
    && String.contains rendered "label=\"Dependency Graph\";"
  then
    Ok ()
  else
    Error "DOT graphs should render graph-level attributes"

let test_to_string_preserves_all_nodes_and_edges = fun _ctx ->
  let graph = Graph.Dot.create ~name:"deps" ~style:Graph.Dot.Directed in
  let graph = Graph.Dot.add_node graph ~id:"A" () in
  let graph = Graph.Dot.add_node graph ~id:"B" () in
  let graph = Graph.Dot.add_edge graph ~from_node:"A" ~to_node:"B" () in
  let rendered = Graph.Dot.to_string graph in
  if
    String.contains rendered "\"A\";"
    && String.contains rendered "\"B\";"
    && String.contains rendered "\"A\" -> \"B\";"
  then
    Ok ()
  else
    Error "DOT output should contain all added nodes and edges"

let test_special_characters_are_escaped = fun _ctx ->
  let graph = Graph.Dot.create ~name:"deps" ~style:Graph.Dot.Directed in
  let graph = Graph.Dot.add_node graph ~id:"a\"b\\c" ~label:"quote\"slash\\line\nbreak" () in
  let rendered = Graph.Dot.to_string graph in
  if
    String.contains rendered "\"a\\\"b\\\\c\""
    && String.contains rendered "label=\"quote\\\"slash\\\\line\\nbreak\""
  then
    Ok ()
  else
    Error "DOT output should escape quotes, backslashes, and newlines"

let tests =
  Test.[
    case "directed graphs render a digraph header" test_create_directed_graph_renders_digraph_header;
    case "undirected graphs render a graph header" test_create_undirected_graph_renders_graph_header;
    case "nodes render labels and attributes" test_add_node_renders_label_and_attributes;
    case "directed edges render with arrows" test_add_edge_renders_directed_arrow;
    case "undirected edges render with double dashes" test_add_edge_renders_undirected_arrow;
    case "graph-level attributes are rendered" test_graph_level_attributes_are_rendered;
    case
      "all added nodes and edges appear in the output"
      test_to_string_preserves_all_nodes_and_edges;
    case "special characters are escaped" test_special_characters_are_escaped;
  ]

let main ~args = Test.Cli.main ~name:"graph_dot" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
