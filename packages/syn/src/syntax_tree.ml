open Std
open Std.Collections
open Std.Data

type token_leaf = {
  kind: Syntax_kind2.t;
  raw_lo: int;
  raw_hi: int;
  body_raw: int;
}

type missing = {
  kind: Syntax_kind2.t;
  offset: int;
}

type child =
  | Node of int
  | Token of int
  | Missing of missing

type node = {
  kind: Syntax_kind2.t;
  first_child: int;
  child_count: int;
  raw_lo: int;
  raw_hi: int;
  full_width: int;
}

type t = {
  source: string;
  raw_tokens: Raw_token.t array;
  significant_tokens: int array;
  tokens: token_leaf array;
  nodes: node array;
  children: child array;
  root: int;
}

type frame = {
  kind: Syntax_kind2.t;
  children: child Vector.t;
}

let raw_start = fun raw_tokens raw_index ->
  if raw_index < 0 || raw_index >= Array.length raw_tokens then
    0
  else
    (Array.get_unchecked raw_tokens ~at:raw_index).Raw_token.span.Ceibo.Span.start

let raw_end = fun raw_tokens raw_index ->
  if raw_index < 0 || raw_index >= Array.length raw_tokens then
    0
  else
    (Array.get_unchecked raw_tokens ~at:raw_index).Raw_token.span.Ceibo.Span.end_

let child_range = fun nodes tokens child ->
  match child with
  | Token token_id ->
      let token = Vector.get_unchecked tokens ~at:token_id in
      Some (token.raw_lo, token.raw_hi)
  | Node node_id ->
      let node = Vector.get_unchecked nodes ~at:node_id in
      Some (node.raw_lo, node.raw_hi)
  | Missing _ ->
      None

let finish_frame = fun ~raw_tokens ~nodes ~tokens ~children_store frame ->
  let first_child = Vector.length children_store in
  let frame_children = Vector.to_array frame.children in
  Array.iter frame_children ~fn:(fun child -> Vector.push children_store ~value:child);
  let child_count = Array.length frame_children in
  let range =
    Array.fold_left frame_children ~init:None
      ~fn:(fun acc child ->
        match child_range nodes tokens child with
        | None -> acc
        | Some (lo, hi) -> (
            match acc with
            | None -> Some (lo, hi)
            | Some (acc_lo, acc_hi) -> Some (Int.min acc_lo lo, Int.max acc_hi hi)
          ))
  in
  let raw_lo, raw_hi, full_width =
    match range with
    | None -> (0, 0, 0)
    | Some (raw_lo, raw_hi) ->
        let width =
          if raw_hi <= raw_lo then
            0
          else
            raw_end raw_tokens (raw_hi - 1) - raw_start raw_tokens raw_lo
        in
        (raw_lo, raw_hi, width)
  in
  {
    kind = frame.kind;
    first_child;
    child_count;
    raw_lo;
    raw_hi;
    full_width;
  }

let build = fun ~source ~raw_tokens ~significant_tokens events ->
  let tokens = Vector.create () in
  let nodes = Vector.create () in
  let children_store = Vector.create () in
  let stack = ref [] in
  let root = ref None in
  let next_raw_lo = ref 0 in
  let push_child child =
    match !stack with
    | frame :: _ -> Vector.push frame.children ~value:child
    | [] -> ()
  in
  let push_node kind =
    stack := { kind; children = Vector.create () } :: !stack
  in
  let pop_node () =
    match !stack with
    | [] -> ()
    | frame :: rest ->
        stack := rest;
        let node = finish_frame ~raw_tokens ~nodes ~tokens ~children_store frame in
        let node_id = Vector.length nodes in
        Vector.push nodes ~value:node;
        (
          match rest with
          | _ :: _ -> push_child (Node node_id)
          | [] -> root := Some node_id
        )
  in
  let push_token raw_index =
    if raw_index >= 0 && raw_index < Array.length raw_tokens then
      let raw = Array.get_unchecked raw_tokens ~at:raw_index in
      let token_id = Vector.length tokens in
      let token = {
        kind = raw.Raw_token.kind;
        raw_lo = !next_raw_lo;
        raw_hi = raw_index + 1;
        body_raw = raw_index
      } in
      next_raw_lo := raw_index + 1;
      Vector.push tokens ~value:token;
      push_child (Token token_id)
  in
  Array.iter events
    ~fn:(
      function
      | Event.StartNode (Some kind) -> push_node kind
      | Event.StartNode None -> push_node Syntax_kind2.ERROR
      | Event.FinishNode -> pop_node ()
      | Event.Token raw_index -> push_token raw_index
      | Event.Missing (kind, offset) -> push_child (Missing { kind; offset })
      | Event.Error _ -> ()
    );
  let root =
    match !root with
    | Some root -> root
    | None ->
        let node = {
          kind = Syntax_kind2.SOURCE_FILE;
          first_child = 0;
          child_count = 0;
          raw_lo = 0;
          raw_hi = 0;
          full_width = 0;
        }
        in
        Vector.push nodes ~value:node;
        0
  in
  {
    source;
    raw_tokens;
    significant_tokens;
    tokens = Vector.to_array tokens;
    nodes = Vector.to_array nodes;
    children = Vector.to_array children_store;
    root;
  }

let root = fun tree -> Array.get_unchecked tree.nodes ~at:tree.root

let node = fun tree node_id -> Array.get_unchecked tree.nodes ~at:node_id

let token = fun tree token_id -> Array.get_unchecked tree.tokens ~at:token_id

let child = fun tree child_id -> Array.get_unchecked tree.children ~at:child_id

let children = fun tree node ->
  let rec loop index acc =
    if index >= node.child_count then
      List.reverse acc
    else
      loop (index + 1) (Array.get_unchecked tree.children ~at:(node.first_child + index) :: acc)
  in
  loop 0 []

let raw_range_text = fun tree ~raw_lo ~raw_hi ->
  if raw_hi <= raw_lo then
    ""
  else
    let start = raw_start tree.raw_tokens raw_lo in
    let end_ = raw_end tree.raw_tokens (raw_hi - 1) in
    String.sub tree.source ~offset:start ~len:(end_ - start)

let token_text = fun tree token ->
  Raw_token.text ~source:tree.source (Array.get_unchecked tree.raw_tokens ~at:token.body_raw)

let node_text = fun tree node -> raw_range_text tree ~raw_lo:node.raw_lo ~raw_hi:node.raw_hi

let span_json = fun span ->
  Json.Object [ ("start", Json.Int span.Ceibo.Span.start); ("end", Json.Int span.Ceibo.Span.end_) ]

let raw_token_json = fun tree index token ->
  Json.Object [
    ("index", Json.Int index);
    ("kind", Json.String (Syntax_kind2.to_string token.Raw_token.kind));
    ("span", span_json token.Raw_token.span);
    ("text", Json.String (Raw_token.text ~source:tree.source token))
  ]

let rec child_json = fun tree child ->
  match child with
  | Token token_id ->
      let token = token tree token_id in
      Json.Object [
        ("kind", Json.String (Syntax_kind2.to_string token.kind));
        ("raw_lo", Json.Int token.raw_lo);
        ("raw_hi", Json.Int token.raw_hi);
        ("text", Json.String (token_text tree token))
      ]
  | Missing missing ->
      Json.Object [
        ("kind", Json.String "MISSING");
        ("expected", Json.String (Syntax_kind2.to_string missing.kind));
        ("offset", Json.Int missing.offset)
      ]
  | Node node_id ->
      node_json tree node_id

and node_json = fun tree node_id ->
  let node = node tree node_id in
  Json.Object [
    ("kind", Json.String (Syntax_kind2.to_string node.kind));
    ("raw_lo", Json.Int node.raw_lo);
    ("raw_hi", Json.Int node.raw_hi);
    ("full_width", Json.Int node.full_width);
    ("children", Json.Array (List.map (children tree node) ~fn:(child_json tree)))
  ]

let to_json = fun tree ->
  Json.Object [
    (
      "raw_tokens",
      Json.Array (Array.to_list tree.raw_tokens
      |> List.mapi ~fn:(fun index token -> raw_token_json tree index token))
    );
    ("tree", node_json tree tree.root)
  ]
