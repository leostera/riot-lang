open Std
open Std.Collections

type ('kind, 'text) trivia = { kind: 'kind; text: 'text; width: int }

type ('kind, 'text) token = {
  kind: 'kind;
  text: 'text;
  width: int;
  leading_trivia: ('kind, 'text) trivia list;
}

type ('kind, 'text) node = { kind: 'kind; width: int; children: ('kind, 'text) element list }
and ('kind, 'text) element =
  | Token of ('kind, 'text) token
  | Node of ('kind, 'text) node

let make_trivia = fun ~kind ~text ~width -> { kind; text; width }

let trivia_width = fun (trivia: ('kind, 'text) trivia) -> trivia.width

let leading_trivia = fun (token: ('kind, 'text) token) -> token.leading_trivia

let token_width = fun (token: ('kind, 'text) token) -> token.width

let token_full_width = fun (token: ('kind, 'text) token) -> token.width + List.fold_left token.leading_trivia ~init:0 ~fn:(
  fun acc (trivia: ('kind, 'text) trivia) -> acc + trivia.width
)

let make_token = fun ~leading_trivia ~kind ~text ~width ->
  {
    kind;
    text;
    width;
    leading_trivia
  }

let rec element_width = fun element ->
  match element with
  | Token token -> token_full_width token
  | Node node -> node.width

let compute_width = fun children -> List.fold_left children ~init:0 ~fn:(
  fun acc elem -> acc + element_width elem
)

let make_node = fun ~kind ~children -> { kind; children; width = compute_width children }

let width = element_width

let kind = fun element ->
  match element with
  | Token t -> t.kind
  | Node n -> n.kind

let text = fun element ->
  match element with
  | Token t -> Some t.text
  | Node _ -> None

let is_token = fun element ->
  match element with
  | Token _ -> true
  | Node _ -> false

let is_node = fun element ->
  match element with
  | Token _ -> false
  | Node _ -> true

let replace_child = fun node ~index ~child ->
  let rec loop i = function
    | [] -> []
    | _ :: rest when i = index -> child :: rest
    | current :: rest -> current :: loop (i + 1) rest
  in
  make_node ~kind:node.kind ~children:(loop 0 node.children)

let append_child = fun node ~child -> make_node ~kind:node.kind ~children:(node.children @ [ child ])

let child_count = fun node -> List.length node.children

let child = fun node index ->
  let rec loop i = function
    | [] -> None
    | current :: _ when i = index -> Some current
    | _ :: rest -> loop (i + 1) rest
  in
  if index < 0 then
    None
  else loop 0 node.children

let children = fun node -> node.children

let make_node_list = fun ~kind elements -> make_node ~kind ~children:elements

let trivia_to_json = fun ~kind_to_json ~text_to_json (trivia: ('kind, 'text) trivia) ->
  Data.Json.Object [
    "kind", kind_to_json trivia.kind;
    "text", text_to_json trivia.text;
    "width", Data.Json.Int trivia.width;
  ]

let rec to_json = fun ~kind_to_json ~text_to_json elem ->
  match elem with
  | Token tok ->
      Data.Json.Object [
        "type", Data.Json.String "token";
        "kind", kind_to_json tok.kind;
        "text", text_to_json tok.text;
        "width", Data.Json.Int tok.width;
        "full_width", Data.Json.Int (token_full_width tok);
        "leading_trivia", Data.Json.Array (List.map tok.leading_trivia ~fn:(trivia_to_json ~kind_to_json ~text_to_json));
      ]
  | Node node ->
      Data.Json.Object [
        "type", Data.Json.String "node";
        "kind", kind_to_json node.kind;
        "width", Data.Json.Int node.width;
        "children", Data.Json.Array (List.map node.children ~fn:(to_json ~kind_to_json ~text_to_json));
      ]
