open Std
open Std.Collections

type ('kind, 'text) token = { kind : 'kind; text : 'text; width : int }

type ('kind, 'text) node = {
  kind : 'kind;
  width : int;
  children : ('kind, 'text) element array;
}

and ('kind, 'text) element =
  | Token of ('kind, 'text) token
  | Node of ('kind, 'text) node

let make_token ~kind ~text ~width = { kind; text; width }

let compute_width children =
  Array.fold_left
    (fun acc elem ->
      match elem with Token t -> acc + t.width | Node n -> acc + n.width)
    0 children

let make_node ~kind ~children =
  { kind; children; width = compute_width children }

let width = function Token t -> t.width | Node n -> n.width
let kind = function Token t -> t.kind | Node n -> n.kind
let text = function Token t -> Some t.text | Node _ -> None
let is_token = function Token _ -> true | Node _ -> false
let is_node = function Token _ -> false | Node _ -> true

let replace_child node ~index ~child =
  let new_children = Array.copy node.children in
  new_children.(index) <- child;
  make_node ~kind:node.kind ~children:new_children

let append_child node ~child =
  let new_children = Array.append node.children [| child |] in
  make_node ~kind:node.kind ~children:new_children

let child_count node = Array.length node.children

let child node index =
  if index >= 0 && index < Array.length node.children then
    Some node.children.(index)
  else None

let children node = node.children

let make_node_list ~kind elements =
  make_node ~kind ~children:(Array.of_list elements)

let rec to_json ~kind_to_json ~text_to_json elem =
  match elem with
  | Token tok ->
      Data.Json.Object
        [
          ("type", Data.Json.String "token");
          ("kind", kind_to_json tok.kind);
          ("text", text_to_json tok.text);
          ("width", Data.Json.Int tok.width);
        ]
  | Node node ->
      Data.Json.Object
        [
          ("type", Data.Json.String "node");
          ("kind", kind_to_json node.kind);
          ("width", Data.Json.Int node.width);
          ( "children",
            Data.Json.Array
              (Array.to_list
                 (Array.map (to_json ~kind_to_json ~text_to_json) node.children))
          );
        ]
