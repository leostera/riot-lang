open Std
open Std.Collections
open Std.Data

module Slice = IO.IoVec.IoSlice

type token_leaf = {
  kind: Syntax_kind.t;
  raw_lo: int;
  raw_hi: int;
  body_raw: int;
}

type missing = {
  kind: Syntax_kind.t;
  offset: int;
}

type child =
  | Node of int
  | Token of int
  | Missing of missing

type node = {
  kind: Syntax_kind.t;
  first_child: int;
  child_count: int;
  raw_lo: int;
  raw_hi: int;
  full_width: int;
  token_width: int;
}

type t = {
  source: Slice.t;
  raw_tokens: Raw_token.t Vector.t;
  significant_tokens: int Vector.t;
  tokens: token_leaf Vector.t;
  nodes: node Vector.t;
  children: child Vector.t;
  root: int;
}

type tree = t

type frame = {
  kind: Syntax_kind.t;
  first_pending_child: int;
  mutable has_range: bool;
  mutable frame_raw_lo: int;
  mutable frame_raw_hi: int;
  mutable frame_token_width: int;
}

let raw_at = fun raw_tokens raw_index -> Vector.get_unchecked raw_tokens ~at:raw_index

let truncate_vector = fun vector ~len ->
  let length = Vector.length vector in
  if Int.(len < 0) || Int.(len > length) then
    panic "Syntax_tree.truncate_vector received an out-of-bounds length"
  else if Int.equal len 0 then
    Vector.clear vector
  else if Int.(len < length) then
    ignore (Vector.split_off vector ~at:len)

let raw_start = fun raw_tokens raw_index ->
  if Int.(raw_index < 0) || Int.(raw_index >= Vector.length raw_tokens) then
    0
  else
    (raw_at raw_tokens raw_index).Raw_token.span.Span.start

let raw_end = fun raw_tokens raw_index ->
  if Int.(raw_index < 0) || Int.(raw_index >= Vector.length raw_tokens) then
    0
  else
    (raw_at raw_tokens raw_index).Raw_token.span.Span.end_

let include_range = fun frame ~lo ~hi ->
  if frame.has_range then (
    frame.frame_raw_lo <- Int.min frame.frame_raw_lo lo;
    frame.frame_raw_hi <- Int.max frame.frame_raw_hi hi
  ) else (
    frame.has_range <- true;
    frame.frame_raw_lo <- lo;
    frame.frame_raw_hi <- hi
  )

let include_child_range = fun ~(nodes:node Vector.t) ~(tokens:token_leaf Vector.t) frame child ->
  match child with
  | Token token_id ->
      let token = Vector.get_unchecked tokens ~at:token_id in
      (* Token leaves own the raw trivia range before their significant body
         token, so node spans stay lossless without trivia child edges.
      *)
      include_range frame ~lo:token.raw_lo ~hi:token.raw_hi
  | Node node_id ->
      let node = Vector.get_unchecked nodes ~at:node_id in
      include_range frame ~lo:node.raw_lo ~hi:node.raw_hi
  | Missing _ -> ()

let include_child_token_width = fun
  ~(raw_tokens:Raw_token.t Vector.t)
  ~(nodes:node Vector.t)
  ~(tokens:token_leaf Vector.t)
  frame
  child ->
  match child with
  | Token token_id ->
      let token = Vector.get_unchecked tokens ~at:token_id in
      frame.frame_token_width <- Int.(frame.frame_token_width
      + Raw_token.width (raw_at raw_tokens token.body_raw))
  | Node node_id ->
      let node = Vector.get_unchecked nodes ~at:node_id in
      frame.frame_token_width <- Int.(frame.frame_token_width + node.token_width)
  | Missing _ -> ()

module Builder = struct
  type checkpoint = {
    token_leaves_len: int;
    node_store_len: int;
    child_store_len: int;
    pending_children_len: int;
    frame_stack_len: int;
    diagnostics_len: int;
    root_id: int option;
    next_raw_lo: int;
    event_count: int;
  }

  type marker = { depth: int }

  type completed = {
    child: child;
    kind: Syntax_kind.t;
  }

  type t = {
    source: Slice.t;
    raw_tokens: Raw_token.t Vector.t;
    significant_tokens: int Vector.t;
    token_leaves: token_leaf Vector.t;
    node_store: node Vector.t;
    child_store: child Vector.t;
    pending_children: child Vector.t;
    frame_stack: frame Vector.t;
    diagnostics: Diagnostic.t Vector.t;
    mutable root_id: int option;
    mutable next_raw_lo: int;
    mutable event_count: int;
  }

  let create = fun ~source ~token_stream ?(event_capacity = 0) ?(diagnostic_capacity = 0) () ->
    let significant_count = Vector.length token_stream.Raw_token.significant in
    let event_capacity = Int.max 1 event_capacity in
    {
      source;
      raw_tokens = token_stream.Raw_token.raw;
      significant_tokens = token_stream.Raw_token.significant;
      token_leaves = Vector.with_capacity ~size:significant_count;
      node_store = Vector.with_capacity ~size:(Int.max 1 (event_capacity / 2));
      child_store = Vector.with_capacity ~size:event_capacity;
      pending_children = Vector.with_capacity ~size:event_capacity;
      frame_stack = Vector.with_capacity ~size:64;
      diagnostics = Vector.with_capacity ~size:diagnostic_capacity;
      root_id = None;
      next_raw_lo = 0;
      event_count = 0;
    }

  let push_child = fun builder child ->
    let depth = Vector.length builder.frame_stack in
    if Int.(depth > 0) then
      let frame = Vector.get_unchecked builder.frame_stack ~at:Int.(depth - 1) in
      Vector.push builder.pending_children ~value:child;
    include_child_range ~nodes:builder.node_store ~tokens:builder.token_leaves frame child;
    include_child_token_width
      ~raw_tokens:builder.raw_tokens
      ~nodes:builder.node_store
      ~tokens:builder.token_leaves
      frame
      child

  let start_node = fun builder ->
    let depth = Vector.length builder.frame_stack in
    builder.event_count <- Int.(builder.event_count + 1);
    Vector.push
      builder.frame_stack
      ~value:{
        kind = Syntax_kind.ERROR;
        first_pending_child = Vector.length builder.pending_children;
        has_range = false;
        frame_raw_lo = 0;
        frame_raw_hi = 0;
        frame_token_width = 0;
      };
    { depth }

  let copy_pending_children = fun builder first_child limit ->
    let rec loop index =
      if Int.(index < limit) then (
        Vector.push
          builder.child_store
          ~value:(Vector.get_unchecked builder.pending_children ~at:index);
        loop Int.(index + 1)
      )
    in
    loop first_child

  let complete = fun builder _marker kind ->
    builder.event_count <- Int.(builder.event_count + 1);
    let depth = Vector.length builder.frame_stack in
    if Int.(depth <= 0) then
      panic "Syntax_tree.Builder.complete called with no open node"
    else
      let frame = Vector.get_unchecked builder.frame_stack ~at:Int.(depth - 1) in
      truncate_vector builder.frame_stack ~len:Int.(depth - 1);
    let pending_limit = Vector.length builder.pending_children in
    let first_child = Vector.length builder.child_store in
    copy_pending_children builder frame.first_pending_child pending_limit;
    let child_count = Int.(pending_limit - frame.first_pending_child) in
    truncate_vector builder.pending_children ~len:frame.first_pending_child;
    let (raw_lo, raw_hi, full_width) =
      if frame.has_range then
        let width =
          if Int.(frame.frame_raw_hi <= frame.frame_raw_lo) then
            0
          else
            Int.(raw_end builder.raw_tokens (frame.frame_raw_hi - 1)
            - raw_start builder.raw_tokens frame.frame_raw_lo)
        in
        (frame.frame_raw_lo, frame.frame_raw_hi, width)
      else
        (0, 0, 0)
    in
    let node = {
      kind;
      first_child;
      child_count;
      raw_lo;
      raw_hi;
      full_width;
      token_width = frame.frame_token_width;
    }
    in
    let node_id = Vector.length builder.node_store in
    Vector.push builder.node_store ~value:node;
    let child = Node node_id in
    if Int.(Vector.length builder.frame_stack > 0) then
      push_child builder child
    else
      builder.root_id <- Some node_id;
    { child; kind }

  let same_child = fun left right ->
    match (left, right) with
    | (Node left, Node right)
    | (Token left, Token right) -> Int.(left = right)
    | (Missing left, Missing right) ->
        Syntax_kind.is left.kind right.kind && Int.(left.offset = right.offset)
    | _ -> false

  let precede = fun builder completed ->
    let pending_len = Vector.length builder.pending_children in
    if Int.(pending_len <= 0) then
      panic "Syntax_tree.Builder.precede called without a completed child"
    else
      let last_index = Int.(pending_len - 1) in
      let last_child = Vector.get_unchecked builder.pending_children ~at:last_index in
      if not (same_child last_child completed.child) then
        panic "Syntax_tree.Builder.precede expected the completed child to be last"
      else (
        truncate_vector builder.pending_children ~len:last_index;
        let marker = start_node builder in
        push_child builder completed.child;
        marker
      )

  let token = fun builder ~raw_index ->
    builder.event_count <- Int.(builder.event_count + 1);
    if Int.(raw_index >= 0) && Int.(raw_index < Vector.length builder.raw_tokens) then
      let raw = raw_at builder.raw_tokens raw_index in
      let token_id = Vector.length builder.token_leaves in
      let token = {
        kind = raw.Raw_token.kind;
        raw_lo = builder.next_raw_lo;
        raw_hi = Int.(raw_index + 1);
        body_raw = raw_index;
      }
      in
      builder.next_raw_lo <- Int.(raw_index + 1);
    Vector.push builder.token_leaves ~value:token;
    push_child builder (Token token_id)

  let missing = fun builder ~kind ~offset ->
    builder.event_count <- Int.(builder.event_count + 1);
    push_child builder (Missing { kind; offset })

  let error = fun builder diagnostic ->
    builder.event_count <- Int.(builder.event_count + 1);
    Vector.push builder.diagnostics ~value:diagnostic

  let length = fun builder -> builder.event_count

  let checkpoint = fun builder ->
    {
      token_leaves_len = Vector.length builder.token_leaves;
      node_store_len = Vector.length builder.node_store;
      child_store_len = Vector.length builder.child_store;
      pending_children_len = Vector.length builder.pending_children;
      frame_stack_len = Vector.length builder.frame_stack;
      diagnostics_len = Vector.length builder.diagnostics;
      root_id = builder.root_id;
      next_raw_lo = builder.next_raw_lo;
      event_count = builder.event_count;
    }

  let restore = fun builder checkpoint ->
    truncate_vector builder.token_leaves ~len:checkpoint.token_leaves_len;
    truncate_vector builder.node_store ~len:checkpoint.node_store_len;
    truncate_vector builder.child_store ~len:checkpoint.child_store_len;
    truncate_vector builder.pending_children ~len:checkpoint.pending_children_len;
    truncate_vector builder.frame_stack ~len:checkpoint.frame_stack_len;
    truncate_vector builder.diagnostics ~len:checkpoint.diagnostics_len;
    builder.root_id <- checkpoint.root_id;
    builder.next_raw_lo <- checkpoint.next_raw_lo;
    builder.event_count <- checkpoint.event_count

  let diagnostics = fun builder -> builder.diagnostics

  let finish = fun builder ->
    let root =
      match builder.root_id with
      | Some root -> root
      | None ->
          let node = {
            kind = Syntax_kind.SOURCE_FILE;
            first_child = 0;
            child_count = 0;
            raw_lo = 0;
            raw_hi = 0;
            full_width = 0;
            token_width = 0;
          }
          in
          Vector.push builder.node_store ~value:node;
          0
    in
    {
      source = builder.source;
      raw_tokens = builder.raw_tokens;
      significant_tokens = builder.significant_tokens;
      tokens = builder.token_leaves;
      nodes = builder.node_store;
      children = builder.child_store;
      root;
    }
end

let build = fun ~source ~token_stream ~events ->
  let event_count = Event.Buffer.length events in
  let significant_count = Vector.length token_stream.Raw_token.significant in
  let tokens: token_leaf Vector.t = Vector.with_capacity ~size:significant_count in
  let nodes: node Vector.t = Vector.with_capacity ~size:(Int.max 1 (event_count / 2)) in
  let children_store: child Vector.t = Vector.with_capacity ~size:event_count in
  let pending_children: child Vector.t = Vector.with_capacity ~size:event_count in
  let frame_stack: frame Vector.t = Vector.with_capacity ~size:64 in
  let root = ref None in
  let next_raw_lo = ref 0 in
  let raw_tokens = token_stream.Raw_token.raw in
  let push_child child =
    let depth = Vector.length frame_stack in
    if Int.(depth > 0) then
      let frame = Vector.get_unchecked frame_stack ~at:Int.(depth - 1) in
      Vector.push pending_children ~value:child;
    include_child_range ~nodes ~tokens frame child;
    include_child_token_width ~raw_tokens ~nodes ~tokens frame child
  in
  let push_node kind =
    Vector.push
      frame_stack
      ~value:{
        kind;
        first_pending_child = Vector.length pending_children;
        has_range = false;
        frame_raw_lo = 0;
        frame_raw_hi = 0;
        frame_token_width = 0;
      }
  in
  let copy_pending_children first_child limit =
    let rec loop index =
      if Int.(index < limit) then (
        Vector.push children_store ~value:(Vector.get_unchecked pending_children ~at:index);
        loop Int.(index + 1)
      )
    in
    loop first_child
  in
  let pop_node () =
    let depth = Vector.length frame_stack in
    if Int.(depth > 0) then (
      let frame = Vector.get_unchecked frame_stack ~at:Int.(depth - 1) in
      truncate_vector frame_stack ~len:Int.(depth - 1);
      let pending_limit = Vector.length pending_children in
      let first_child = Vector.length children_store in
      copy_pending_children frame.first_pending_child pending_limit;
      let child_count = Int.(pending_limit - frame.first_pending_child) in
      truncate_vector pending_children ~len:frame.first_pending_child;
      let (raw_lo, raw_hi, full_width) =
        if frame.has_range then
          let width =
            if Int.(frame.frame_raw_hi <= frame.frame_raw_lo) then
              0
            else
              Int.(raw_end raw_tokens (frame.frame_raw_hi - 1)
              - raw_start raw_tokens frame.frame_raw_lo)
          in
          (frame.frame_raw_lo, frame.frame_raw_hi, width)
        else
          (0, 0, 0)
      in
      let node = {
        kind = frame.kind;
        first_child;
        child_count;
        raw_lo;
        raw_hi;
        full_width;
        token_width = frame.frame_token_width;
      }
      in
      let node_id = Vector.length nodes in
      Vector.push nodes ~value:node;
      if Int.(Vector.length frame_stack > 0) then
        push_child (Node node_id)
      else
        root := Some node_id
    )
  in
  let push_token raw_index =
    if Int.(raw_index >= 0) && Int.(raw_index < Vector.length raw_tokens) then
      let raw = raw_at raw_tokens raw_index in
      let token_id = Vector.length tokens in
      let token = {
        kind = raw.Raw_token.kind;
        raw_lo = !next_raw_lo;
        raw_hi = Int.(raw_index + 1);
        body_raw = raw_index;
      }
      in
      next_raw_lo := Int.(raw_index + 1);
    Vector.push tokens ~value:token;
    push_child (Token token_id)
  in
  let rec loop_events index =
    if Int.(index < event_count) then (
      (
        match Event.Buffer.get_unchecked events ~at:index with
        | Event.StartNode (Some kind) -> push_node kind
        | Event.StartNode None -> push_node Syntax_kind.ERROR
        | Event.FinishNode -> pop_node ()
        | Event.Token raw_index -> push_token raw_index
        | Event.Missing (kind, offset) -> push_child (Missing { kind; offset })
        | Event.Error _ -> ()
      );
      loop_events Int.(index + 1)
    )
  in
  loop_events 0;
  let root =
    match !root with
    | Some root -> root
    | None ->
        let node = {
          kind = Syntax_kind.SOURCE_FILE;
          first_child = 0;
          child_count = 0;
          raw_lo = 0;
          raw_hi = 0;
          full_width = 0;
          token_width = 0;
        }
        in
        Vector.push nodes ~value:node;
        0
  in
  {
    source;
    raw_tokens;
    significant_tokens = token_stream.Raw_token.significant;
    tokens;
    nodes;
    children = children_store;
    root;
  }

let root = fun (tree: t) -> Vector.get_unchecked tree.nodes ~at:tree.root

let node = fun (tree: t) node_id -> Vector.get_unchecked tree.nodes ~at:node_id

let token = fun (tree: t) token_id -> Vector.get_unchecked tree.tokens ~at:token_id

let child = fun (tree: t) child_id -> Vector.get_unchecked tree.children ~at:child_id

let child_at = fun (tree: t) (node: node) index ->
  if index < 0 || index >= node.child_count then
    None
  else
    Some (Vector.get_unchecked tree.children ~at:(node.first_child + index))

let for_each_child = fun (tree: t) (node: node) ~fn ->
  let rec loop index =
    if index < node.child_count then (
      fn (Vector.get_unchecked tree.children ~at:(node.first_child + index));
      loop (index + 1)
    )
  in
  loop 0

let raw_range_text = fun tree ~raw_lo ~raw_hi ->
  if raw_hi <= raw_lo then
    ""
  else
    let start = raw_start tree.raw_tokens raw_lo in
    let end_ = raw_end tree.raw_tokens (raw_hi - 1) in
    let span = Span.make ~start ~end_ in
    Slice.sub_unchecked tree.source ~off:start ~len:(Span.width span)
    |> Slice.to_string

let token_width = fun tree token -> Raw_token.width (raw_at tree.raw_tokens token.body_raw)

let node_token_width = fun _tree node -> node.token_width

let token_contains_char = fun tree token needle ->
  Raw_token.contains_char
    ~source:tree.source
    (raw_at tree.raw_tokens token.body_raw)
    needle

let token_text_is = fun tree token expected ->
  let slice = Raw_token.slice ~source:tree.source (raw_at tree.raw_tokens token.body_raw) in
  let len = Slice.length slice in
  if not (Int.equal len (String.length expected)) then
    false
  else
    let rec loop index =
      if Int.(index >= len) then
        true
      else if
        Char.equal (Slice.get_unchecked slice ~at:index) (String.get_unchecked expected ~at:index)
      then
        loop Int.(index + 1)
      else
        false
    in
    loop 0

let token_has_newline = fun tree token ->
  Raw_token.has_newline
    (raw_at tree.raw_tokens token.body_raw)

let token_text_slice = fun tree token ->
  Raw_token.slice
    ~source:tree.source
    (raw_at tree.raw_tokens token.body_raw)

let token_text = fun tree token ->
  Raw_token.text_slice
    ~source:tree.source
    (raw_at tree.raw_tokens token.body_raw)

let node_text = fun tree node -> raw_range_text tree ~raw_lo:node.raw_lo ~raw_hi:node.raw_hi

let span_json = fun span ->
  Json.Object [ ("start", Json.Int span.Span.start); ("end", Json.Int span.Span.end_); ]

let raw_token_json = fun tree index token ->
  Json.Object [
    ("index", Json.Int index);
    ("kind", Json.String (Syntax_kind.to_string token.Raw_token.kind));
    ("span", span_json token.Raw_token.span);
    ("text", Json.String (Raw_token.text_slice ~source:tree.source token));
  ]

let rec child_json = fun tree child ->
  match child with
  | Token token_id ->
      let token = token tree token_id in
      Json.Object [
        ("kind", Json.String (Syntax_kind.to_string token.kind));
        ("raw_lo", Json.Int token.raw_lo);
        ("raw_hi", Json.Int token.raw_hi);
        ("text", Json.String (token_text tree token));
      ]
  | Missing missing ->
      Json.Object [
        ("kind", Json.String "MISSING");
        ("expected", Json.String (Syntax_kind.to_string missing.kind));
        ("offset", Json.Int missing.offset);
      ]
  | Node node_id -> node_json tree node_id

and node_json = fun tree node_id ->
  let node = node tree node_id in
  let children_json = ref [] in
  for_each_child
    tree
    node
    ~fn:(fun child -> children_json := child_json tree child :: !children_json);
  Json.Object [
    ("kind", Json.String (Syntax_kind.to_string node.kind));
    ("raw_lo", Json.Int node.raw_lo);
    ("raw_hi", Json.Int node.raw_hi);
    ("full_width", Json.Int node.full_width);
    ("children", Json.Array (List.reverse !children_json));
  ]

let to_json = fun tree ->
  let rec collect_raw_tokens index acc =
    if index < 0 then
      acc
    else
      collect_raw_tokens
        (index - 1)
        (raw_token_json
          tree
          index
          (raw_at tree.raw_tokens index) :: acc)
  in
  Json.Object [
    ("raw_tokens", Json.Array (collect_raw_tokens (Vector.length tree.raw_tokens - 1) []));
    ("tree", node_json tree tree.root);
  ]
