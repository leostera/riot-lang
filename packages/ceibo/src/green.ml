open Std
open Std.Collections

type ('kind, 'text) trivia = {
  kind : 'kind;
  text : 'text;
  width : int;
}

type ('kind, 'text) token = {
  kind : 'kind;
  text : 'text;
  width : int;
  leading_trivia : ('kind, 'text) trivia list;
}

type ('kind, 'text) node = {
  kind : 'kind;
  width : int;
  children : ('kind, 'text) element array;
}

and ('kind, 'text) element =
  | Token of ('kind, 'text) token
  | Node of ('kind, 'text) node

let make_trivia = fun ~kind ~text ~width -> {kind; text; width}

let trivia_width = fun (trivia:('kind, 'text) trivia) -> trivia.width

let leading_trivia = fun (token:('kind, 'text) token) -> token.leading_trivia

let token_width = fun (token:('kind, 'text) token) -> token.width

let token_full_width = fun (token:('kind, 'text) token) -> token.width
+ List.fold_left (fun acc (trivia:('kind, 'text) trivia) -> acc + trivia.width) 0 token.leading_trivia

let make_token = fun ~leading_trivia ~kind ~text ~width -> {kind; text; width; leading_trivia}

let rec element_width =
  function
  | Token token -> token_full_width token
  | Node node -> node.width

let compute_width = fun children ->
  Array.fold_left (fun acc elem -> acc + element_width elem) 0 children

let make_node = fun ~kind ~children -> {kind; children; width = compute_width children}

let width = element_width

let kind =
  function
  | Token t -> t.kind
  | Node n -> n.kind

let text =
  function
  | Token t -> Some t.text
  | Node _ -> None

let is_token =
  function
  | Token _ -> true
  | Node _ -> false

let is_node =
  function
  | Token _ -> false
  | Node _ -> true

let replace_child = fun node ~index ~child ->
  let new_children = Array.copy node.children in
  new_children.(index) <- child;
  make_node ~kind:node.kind ~children:new_children

let append_child = fun node ~child ->
  let new_children = Array.append node.children [|child|] in
  make_node ~kind:node.kind ~children:new_children

let child_count = fun node -> Array.length node.children

let child = fun node index ->
  if index >= 0 && index < Array.length node.children then
    Some node.children.(index)
  else
    None

let children = fun node -> node.children

let make_node_list = fun ~kind elements -> make_node ~kind ~children:(Array.of_list elements)

let trivia_to_json = fun ~kind_to_json ~text_to_json (trivia:('kind, 'text) trivia) -> Data.Json.Object [
  ("kind", kind_to_json trivia.kind);
  ("text", text_to_json trivia.text);
  ("width", Data.Json.Int trivia.width)
]

let rec to_json = fun ~kind_to_json ~text_to_json elem ->
  match elem with
  | Token tok -> Data.Json.Object [
    ("type", Data.Json.String "token");
    ("kind", kind_to_json tok.kind);
    ("text", text_to_json tok.text);
    ("width", Data.Json.Int tok.width);
    ("full_width", Data.Json.Int (token_full_width tok));
    (
      "leading_trivia",
      Data.Json.Array (List.map (trivia_to_json ~kind_to_json ~text_to_json) tok.leading_trivia)
    )
  ]
  | Node node -> Data.Json.Object [
    ("type", Data.Json.String "node");
    ("kind", kind_to_json node.kind);
    ("width", Data.Json.Int node.width);
    (
      "children",
      Data.Json.Array (Array.to_list (Array.map (to_json ~kind_to_json ~text_to_json) node.children))
    )
  ]
