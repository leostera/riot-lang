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

let create ~name ~style =
  { name; style; nodes = []; edges = []; graph_attrs = [] }

let add_node t ~id ?label ?(attrs = []) () =
  let node = { id; label; attrs } in
  { t with nodes = node :: t.nodes }

let add_edge t ~from_node ~to_node ?label ?(attrs = []) () =
  let edge = { from_node; to_node; label; attrs } in
  { t with edges = edge :: t.edges }

let escape_string s =
  (* Escape quotes and backslashes for DOT format *)
  String.fold_left
    (fun acc c ->
      match c with
      | '"' -> acc ^ "\\\""
      | '\\' -> acc ^ "\\\\"
      | '\n' -> acc ^ "\\n"
      | '\t' -> acc ^ "\\t"
      | c -> acc ^ String.make 1 c)
    "" s

let format_attrs attrs =
  if attrs = [] then ""
  else
    let attr_strs =
      List.map
        (fun (k, v) -> format "%s=\"%s\"" k (escape_string v))
        attrs
    in
    format " [%s]" (String.concat ", " attr_strs)

let format_node (node : node) =
  let label_attr =
    match node.label with Some l -> [ ("label", l) ] | None -> []
  in
  let all_attrs = label_attr @ node.attrs in
  format "  \"%s\"%s;" (escape_string node.id) (format_attrs all_attrs)

let format_edge style edge =
  let arrow = match style with Directed -> "->" | Undirected -> "--" in
  let label_attr =
    match edge.label with Some l -> [ ("label", l) ] | None -> []
  in
  let all_attrs = label_attr @ edge.attrs in
  format "  \"%s\" %s \"%s\"%s;"
    (escape_string edge.from_node)
    arrow
    (escape_string edge.to_node)
    (format_attrs all_attrs)

let to_string t =
  let graph_type =
    match t.style with Directed -> "digraph" | Undirected -> "graph"
  in
  let graph_attr_str =
    if t.graph_attrs = [] then ""
    else
      List.map
        (fun (k, v) -> format "  %s=\"%s\";\n" k (escape_string v))
        t.graph_attrs
      |> String.concat ""
  in
  let node_strs = List.rev_map format_node t.nodes |> String.concat "\n" in
  let edge_strs =
    List.rev_map (format_edge t.style) t.edges |> String.concat "\n"
  in
  format "%s %s {\n%s%s%s%s}\n" graph_type t.name graph_attr_str
    (if node_strs = "" then "" else node_strs ^ "\n")
    (if node_strs <> "" && edge_strs <> "" then "\n" else "")
    edge_strs
