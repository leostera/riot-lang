open Std
open Std.Collections

type ('kind, 'text) syntax_node = {
  green_node : ('kind, 'text) Green.node;
  parent : ('kind, 'text) syntax_node option;
  offset : int;
}

type ('kind, 'text) syntax_token = {
  green_token : ('kind, 'text) Green.token;
  parent : ('kind, 'text) syntax_node option;
  offset : int;
}

type ('kind, 'text) syntax_element =
  | Node of ('kind, 'text) syntax_node
  | Token of ('kind, 'text) syntax_token

let new_token green_token span =
  { green_token; parent = None; offset = span.Span.start }

module SyntaxNode = struct
  let green (node : ('kind, 'text) syntax_node) = node.green_node
  let offset (node : ('kind, 'text) syntax_node) = node.offset

  let span (node : ('kind, 'text) syntax_node) =
    let g = green node in
    Span.make ~start:node.offset ~end_:(node.offset + g.width)

  let parent (node : ('kind, 'text) syntax_node) = node.parent

  let child_count (node : ('kind, 'text) syntax_node) =
    Green.child_count (green node)

  let child (node : ('kind, 'text) syntax_node) index =
    match Green.child (green node) index with
    | None -> None
    | Some elem ->
        let child_offset =
          let rec offset_before i acc =
            if i >= index then acc
            else
              match Green.child (green node) i with
              | None -> acc
              | Some e -> offset_before (i + 1) (acc + Green.width e)
          in
          node.offset + offset_before 0 0
        in
        Some
          (match elem with
          | Green.Token t ->
              Token
                { green_token = t; parent = Some node; offset = child_offset }
          | Green.Node n ->
              Node { green_node = n; parent = Some node; offset = child_offset })

  let children (node : ('kind, 'text) syntax_node) =
    let count = child_count node in
    Array.init count (fun i -> Option.unwrap (child node i))

  let kind (node : ('kind, 'text) syntax_node) = (green node).kind

  let next_sibling (node : ('kind, 'text) syntax_node) =
    match node.parent with
    | None -> None
    | Some parent -> (
        let rec find_self i =
          if i >= child_count parent then None
          else
            match child parent i with
            | Some (Node n) when n.offset = node.offset -> Some i
            | _ -> find_self (i + 1)
        in
        match find_self 0 with
        | None -> None
        | Some i ->
            if i + 1 < child_count parent then
              match child parent (i + 1) with
              | Some (Node n) -> Some n
              | _ -> None
            else None)

  let prev_sibling (node : ('kind, 'text) syntax_node) =
    match node.parent with
    | None -> None
    | Some parent -> (
        let rec find_self i =
          if i >= child_count parent then None
          else
            match child parent i with
            | Some (Node n) when n.offset = node.offset -> Some i
            | _ -> find_self (i + 1)
        in
        match find_self 0 with
        | None -> None
        | Some i ->
            if i > 0 then
              match child parent (i - 1) with
              | Some (Node n) -> Some n
              | _ -> None
            else None)

  let rec first_token (node : ('kind, 'text) syntax_node) =
    if child_count node = 0 then None
    else
      match child node 0 with
      | Some (Token t) -> Some t
      | Some (Node n) -> first_token n
      | None -> None

  let rec last_token (node : ('kind, 'text) syntax_node) =
    let count = child_count node in
    if count = 0 then None
    else
      match child node (count - 1) with
      | Some (Token t) -> Some t
      | Some (Node n) -> last_token n
      | None -> None

  let rec preorder (node : ('kind, 'text) syntax_node) f =
    f (Node node);
    Array.iter
      (function Token t -> f (Token t) | Node n -> preorder n f)
      (children node)

  let rec postorder (node : ('kind, 'text) syntax_node) f =
    Array.iter
      (function Token t -> f (Token t) | Node n -> postorder n f)
      (children node);
    f (Node node)
end

module SyntaxToken = struct
  let green (token : ('kind, 'text) syntax_token) = token.green_token
  let offset (token : ('kind, 'text) syntax_token) = token.offset

  let span (token : ('kind, 'text) syntax_token) =
    let g = green token in
    Span.make ~start:token.offset ~end_:(token.offset + g.width)

  let kind (token : ('kind, 'text) syntax_token) = (green token).kind
  let text (token : ('kind, 'text) syntax_token) = (green token).text
end

let new_root green_node = { green_node; parent = None; offset = 0 }

let rec to_json ~kind_to_json ~text_to_json elem =
  match elem with
  | Token tok ->
      Data.Json.Object
        [
          ("type", Data.Json.String "token");
          ("kind", kind_to_json (SyntaxToken.kind tok));
          ("text", text_to_json (SyntaxToken.text tok));
          ("span", Span.to_json (SyntaxToken.span tok));
        ]
  | Node node ->
      Data.Json.Object
        [
          ("type", Data.Json.String "node");
          ("kind", kind_to_json (SyntaxNode.kind node));
          ("span", Span.to_json (SyntaxNode.span node));
          ( "children",
            Data.Json.Array
              (Array.to_list
                 (Array.map
                    (to_json ~kind_to_json ~text_to_json)
                    (SyntaxNode.children node))) );
        ]
