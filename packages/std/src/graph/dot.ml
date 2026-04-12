open Global
open Collections

(** DOT graph format generation for Graphviz *)
type graph_style =
  Directed
  | Undirected

type node = {
  id: string;
  label: string option;
  attrs: (string * string) list;
}

type edge = {
  from_node: string;
  to_node: string;
  label: string option;
  attrs: (string * string) list;
}

type t = {
  name: string;
  style: graph_style;
  nodes: node list;
  edges: edge list;
  graph_attrs: (string * string) list;
}

let create = fun ~name ~style ->
  {
    name;
    style;
    nodes = [];
    edges = [];
    graph_attrs = [];
  }

let add_node = fun t ~id ?label ?(attrs = []) () ->
  let node = { id; label; attrs } in
  { t with nodes = node :: t.nodes }

let add_edge = fun t ~from_node ~to_node ?label ?(attrs = []) () ->
  let edge = { from_node; to_node; label; attrs } in
  { t with edges = edge :: t.edges }

let escape_string = fun s ->
  (* Escape quotes and backslashes for DOT format *)
  String.fold_left
    ~fn:(fun acc c ->
      match c with
      | '"' -> acc ^ "\\\""
      | '\\' -> acc ^ "\\\\"
      | '\n' -> acc ^ "\\n"
      | '\t' -> acc ^ "\\t"
      | c -> acc ^ String.make ~len:1 ~char:c)
    ~acc:""
    s

let format_attrs = fun attrs ->
  if attrs = [] then
    ""
  else
    let attr_strs =
      List.map attrs ~fn:(fun (k, v) -> k ^ "=\"" ^ escape_string v ^ "\"")
    in
    " [" ^ String.concat ", " attr_strs ^ "]"

let format_node = fun (node: node) ->
  let label_attr =
    match node.label with
    | Some l -> [ ("label", l) ]
    | None -> []
  in
  let all_attrs = label_attr @ node.attrs in
  "  \"" ^ escape_string node.id ^ "\"" ^ format_attrs all_attrs ^ ";"

let format_edge = fun style edge ->
  let arrow =
    match style with
    | Directed -> "->"
    | Undirected -> "--"
  in
  let label_attr =
    match edge.label with
    | Some l -> [ ("label", l) ]
    | None -> []
  in
  let all_attrs = label_attr @ edge.attrs in
  "  \""
  ^ escape_string edge.from_node
  ^ "\" "
  ^ arrow
  ^ " \""
  ^ escape_string edge.to_node
  ^ "\""
  ^ format_attrs all_attrs
  ^ ";"

let to_string = fun t ->
  let graph_type =
    match t.style with
    | Directed -> "digraph"
    | Undirected -> "graph"
  in
  let graph_attr_str =
    if t.graph_attrs = [] then
      ""
    else
      List.map t.graph_attrs ~fn:(fun (k, v) -> "  " ^ k ^ "=\"" ^ escape_string v ^ "\";\n")
      |> String.concat ""
  in
  let node_strs = List.map t.nodes ~fn:format_node |> List.reverse |> String.concat "\n" in
  let edge_strs = List.map t.edges ~fn:(format_edge t.style) |> List.reverse |> String.concat "\n" in
  graph_type ^ " " ^ t.name ^ " {\n" ^ graph_attr_str ^ (
    if node_strs = "" then
      ""
    else
      node_strs ^ "\n"
  ) ^ (
    if node_strs != "" && edge_strs != "" then
      "\n"
    else
      ""
  ) ^ edge_strs ^ "}\n"
