open Std

(** Ceibo - Generic Red-Green Syntax Trees

    Ceibo provides a lossless, immutable syntax tree implementation based on the
    red-green tree architecture from Roslyn, Swift, and Rust Analyzer.

    # Architecture Overview

    The tree is split into two layers:

    - **Green Layer**: Position-independent, immutable nodes that can be shared
      across multiple trees. Green nodes have no parent pointers and cache their
      total width for fast positioning.

    - **Red Layer**: Ephemeral, position-aware views over the green tree. Red
      nodes are fabricated lazily with parent links and absolute offsets.
      They're garbage collected when no longer referenced.

    # Type Parameters

    Ceibo is fully generic to remain language-agnostic:

    - `'kind`: Token/node kind type (e.g., an enum of syntax elements)
    - `'text`: Text storage type (e.g., string, rope, or byte buffer)

    # Key Properties

    - **Lossless**: Every byte of input is represented in the tree
    - **Immutable**: Modifications create new trees with structural sharing
    - **Efficient**: Green nodes are shared, red nodes are lazy
    - **Incremental**: Unchanged subtrees can be reused across edits

    # Example Usage

    ```ocaml (* Define your kinds *) type kind = LET_EXPR | IDENT | INT | PLUS

    (* Build a green tree *) let green = Builder.make_node ~kind:LET_EXPR
    [ Builder.make_token ~kind:IDENT ~text:"x" ~width:1; Builder.make_token
     ~kind:PLUS ~text:"+" ~width:1; Builder.make_token ~kind:INT ~text:"42"
     ~width:2; ]

    (* Get positioned red view *) let root = Red.new_root green let first_child
    = Red.child root 0 ```

    # When to Use Green vs Red

    - Use **Green** when building or transforming trees
    - Use **Red** when traversing or querying positions *)
(** # Span

    Source position spans with start/end offsets. *)

module Span: sig
  (** Span type representing a range in source text. *)
  (** `make ~start ~end_` creates a span. *)
  type t = {
    start: int;
    end_: int;
  }
  val make: start:int -> end_:int -> t

  (** `length span` returns the length of the span. *)
  val length: t -> int

  (** `contains span offset` checks if offset is within span. *)
  val contains: t -> int -> bool

  (** `overlaps s1 s2` checks if two spans overlap. *)
  val overlaps: t -> t -> bool

  (** `union s1 s2` creates a span covering both spans. *)
  val union: t -> t -> t

  val to_string: t -> string

  (** `to_string span` formats span for debugging. *)
end

(** # Green Layer

    The green layer represents syntax trees without position information. Green
    nodes are immutable and position-independent, allowing them to be safely
    shared across multiple trees.

    ## Design Principles

    - **No positions**: Nodes don't know where they appear in source
    - **No parents**: Nodes have no parent pointers
    - **Width caching**: Total width cached for O(1) positioning in red layer
    - **Structural sharing**: Unchanged subtrees shared across versions

    ## Memory Model

    Green nodes are regular OCaml values managed by the GC. Immutability and
    lack of parent pointers means the same green node can appear in multiple
    trees without duplication. *)

module Green: sig
  (** ## Types *)

  type ('kind, 'text) trivia = {
    kind: 'kind;
    text: 'text;
    width: int;
  }
  (** Green token - leaf node containing source text and its leading trivia.

      Tokens represent atomic elements like keywords, identifiers, and literals.
      Parametrized by:
      - `'kind`: The type used to identify token kinds (e.g., an enum)
      - `'text`: The type used to store text (e.g., string or rope)

      Each token has:
      - A kind (what type of token)
      - The token body text
      - Width of just the token body
      - Leading trivia that appears immediately before the token body

      Trivia is not represented as a standalone child element in the tree. *)
  (** Green node - interior node with children.

      Nodes represent grammatical constructions like expressions and
      declarations. Parametrized by the same `'kind` and `'text` as tokens.

      Each node has:
      - A kind (what type of node)
      - Children (array of non-trivia tokens/nodes)
      - Width (total byte length of all children, cached) *)
  type ('kind, 'text) token = {
    kind: 'kind;
    text: 'text;
    width: int;
    leading_trivia: ('kind, 'text) trivia list;
  }
  type ('kind, 'text) node = {
    kind: 'kind;
    width: int;
    children: ('kind, 'text) element array;
  }

  (** Element can be either a token or a node.

      Most tree operations work on elements generically, not caring whether
      they're dealing with tokens or nodes. *)
  and ('kind, 'text) element =
    | Token of ('kind, 'text) token
    | Node of ('kind, 'text) node
  (** ## Construction *)
  (** ## Construction *)
  (** `make_token ~kind ~text ~width` creates a new token.

      The width should be the byte length of the text for correct positioning.

      Example: `make_token ~kind:IDENT ~text:"foo" ~width:3` *)
  (** ## Construction *)
  (** ## Construction *)
  (** `make_trivia ~kind ~text ~width` creates a trivia entry that can be
      attached to a token's `leading_trivia`. *)
  val make_trivia: kind:'kind -> text:'text -> width:int -> ('kind, 'text) trivia

  (** `make_token ~leading_trivia ~kind ~text ~width` creates a new token.

      The width should be the byte length of the token body text for correct
      positioning.

      Example: `make_token ~leading_trivia:[] ~kind:IDENT ~text:"foo" ~width:3` *)
  val make_token:
    leading_trivia:('kind, 'text) trivia list ->
    kind:'kind ->
    text:'text ->
    width:int ->
    ('kind, 'text) token

  (** `make_node ~kind ~children` creates a new node from an array of children.

      The width is automatically computed as the sum of all children widths.

      Example: `make_node ~kind:BIN_EXPR ~children:[|tok1; tok2; tok3|]` *)
  val make_node: kind:'kind -> children:('kind, 'text) element array -> ('kind, 'text) node

  (** `make_node_list ~kind elements` creates a node from a list.

      Convenience wrapper around `make_node` that converts a list to an array.

      Example: `make_node_list ~kind:TUPLE_EXPR [elem1; elem2; elem3]` *)
  val make_node_list: kind:'kind -> ('kind, 'text) element list -> ('kind, 'text) node

  (** ## Accessors *)
  (** `width elem` returns the total byte length of the element.

      For tokens, this includes leading trivia plus the token body width. For
      nodes, this is the sum of all children widths (cached).

      This is O(1) due to caching. *)
  val width: ('kind, 'text) element -> int

  val trivia_width: ('kind, 'text) trivia -> int

  val token_width: ('kind, 'text) token -> int

  val token_full_width: ('kind, 'text) token -> int

  val leading_trivia: ('kind, 'text) token -> ('kind, 'text) trivia list

  (** `kind elem` returns the kind identifier. *)

  (** `text elem` returns the text if element is a token, None if it's a node.
  *)
  val kind: ('kind, 'text) element -> 'kind

  val text: ('kind, 'text) element -> 'text option

  (** `is_token elem` returns true if element is a token. *)
  val is_token: ('kind, 'text) element -> bool

  (** `is_node elem` returns true if element is a node. *)
  val is_node: ('kind, 'text) element -> bool

  (** ## Node Operations *)
  (** `replace_child node ~index ~child` creates a new node with one child
      replaced.

      This enables structural sharing - the new node shares all children except
      the replaced one with the original node.

      Example: `replace_child node ~index:2 ~child:new_elem` *)
  val replace_child: ('kind, 'text) node ->
    index:int ->
    child:('kind, 'text) element ->
    ('kind, 'text) node

  (** `append_child node ~child` creates a new node with a child appended. *)
  val append_child: ('kind, 'text) node -> child:('kind, 'text) element -> ('kind, 'text) node

  (** `child_count node` returns the number of children. *)
  val child_count: ('kind, 'text) node -> int

  (** `child node i` returns the child at index `i`, or `None` if out of bounds.
  *)
  val child: ('kind, 'text) node -> int -> ('kind, 'text) element option

  (** `children node` returns all children as an array. *)
  val children: ('kind, 'text) node -> ('kind, 'text) element array

  (** ## Serialization *)
  val to_json: kind_to_json:('kind -> Data.Json.t) ->
    text_to_json:('text -> Data.Json.t) ->
    ('kind, 'text) element ->
    Data.Json.t

  (** `to_json ~kind_to_json ~text_to_json elem` serializes a green element to
      JSON.

      You must provide functions to convert your custom `'kind` and `'text`
      types to JSON values.

      Example: ```ocaml let kind_to_json kind = Data.Json.String (show_kind
      kind) in let text_to_json text = Data.Json.String text in let json =
      Green.to_json ~kind_to_json ~text_to_json (Green.Node tree) ``` *)
end

(** # Red Layer

    Position-aware views over green trees with parent links. Red nodes are
    fabricated lazily and garbage collected when done.

    ## Design Principles

    - **Lazy fabrication**: Red nodes created on-demand during traversal
    - **Parent links**: Each red node knows its parent (except root)
    - **Absolute positions**: Offset from start of source computed from green
      widths
    - **Ephemeral**: Red nodes are GC'd when no longer referenced

    ## When to Use Red

    Use red nodes when you need:
    - Absolute source positions for tokens/nodes
    - Parent/sibling navigation
    - Tree traversal with position information

    Don't use red for:
    - Building trees (use Green or Builder instead)
    - Long-term storage (just keep the green tree) *)

module Red: sig
  (** ## Types *)

  (** Syntax node - positioned view of green node. *)
  (** Syntax token - positioned view of green token. *)
  type ('kind, 'text) syntax_node
  type ('kind, 'text) syntax_token
  type ('kind, 'text) syntax_trivia
  (** `new_root green` creates a root red node at offset 0. *)
  type ('kind, 'text) syntax_element =
    | Node of ('kind, 'text) syntax_node
    | Token of ('kind, 'text) syntax_token
  val new_root: ('kind, 'text) Green.node -> ('kind, 'text) syntax_node

  (** `new_token green span` creates a standalone red token at the given span.

      This is mainly useful for synthetic CST helpers that need a lightweight
      token wrapper without fabricating a full parsed tree. *)
  val new_token: ('kind, 'text) Green.token -> Span.t -> ('kind, 'text) syntax_token

  (** ## SyntaxNode Operations *)

  module SyntaxNode: sig
    (** `green node` returns the underlying green node. *)
    val green: ('kind, 'text) syntax_node -> ('kind, 'text) Green.node

    (** `offset node` returns the absolute byte offset in source. *)
    val offset: ('kind, 'text) syntax_node -> int

    (** `span node` returns the source span covered by the node. *)
    val span: ('kind, 'text) syntax_node -> Span.t

    (** `parent node` returns the parent node, or `None` if root. *)
    val parent: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option

    (** `child_count node` returns the number of children. *)
    val child_count: ('kind, 'text) syntax_node -> int

    (** `child node i` returns the child at index `i` (lazy fabrication). *)
    val child: ('kind, 'text) syntax_node -> int -> ('kind, 'text) syntax_element option

    (** `children node` returns all non-trivia children.

        Trivia remains attached to tokens and does not appear as a standalone
        child element. *)
    val children: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element array

    (** `children_list node` returns all non-trivia children as a list. *)
    val children_list: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element list

    (** `direct_tokens node` returns only the direct non-trivia token children. *)
    val direct_tokens: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token list

    (** `direct_nodes node` returns only the direct node children. *)
    val direct_nodes: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node list

    (** `kind node` returns the kind from the underlying green node. *)

    (** `next_sibling node` returns the next sibling, or `None` if last. *)
    val kind: ('kind, 'text) syntax_node -> 'kind

    val next_sibling: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option

    (** `prev_sibling node` returns the previous sibling, or `None` if first. *)
    val prev_sibling: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option

    (** `first_token node` finds the leftmost token in the subtree. *)
    val first_token: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option

    (** `last_token node` finds the rightmost token in the subtree. *)
    val last_token: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option

    (** `preorder node f` visits nodes in pre-order. *)
    val preorder: ('kind, 'text) syntax_node -> (('kind, 'text) syntax_element -> unit) -> unit

    (** `postorder node f` visits nodes in post-order. *)
    val postorder: ('kind, 'text) syntax_node -> (('kind, 'text) syntax_element -> unit) -> unit

    val tokens: ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token list

    (** `tokens node` returns every token in the subtree in source order. *)
  end

  (** ## SyntaxToken Operations *)

  module SyntaxTrivia: sig
    val green: ('kind, 'text) syntax_trivia -> ('kind, 'text) Green.trivia

    val offset: ('kind, 'text) syntax_trivia -> int

    val span: ('kind, 'text) syntax_trivia -> Span.t

    val kind: ('kind, 'text) syntax_trivia -> 'kind

    val text: ('kind, 'text) syntax_trivia -> 'text
  end

  module SyntaxToken: sig
    (** `green token` returns the underlying green token. *)
    val green: ('kind, 'text) syntax_token -> ('kind, 'text) Green.token

    (** `offset token` returns the absolute byte offset in source. *)
    val offset: ('kind, 'text) syntax_token -> int

    (** `span token` returns the source span covered by the token. *)
    val span: ('kind, 'text) syntax_token -> Span.t

    (** `kind token` returns the kind from the underlying green token. *)

    (** `text token` returns the text from the underlying green token. *)
    val kind: ('kind, 'text) syntax_token -> 'kind

    val text: ('kind, 'text) syntax_token -> 'text

    (** `leading_trivia token` returns the trivia attached to the token in
        source order, with absolute offsets and spans. *)
    val leading_trivia: ('kind, 'text) syntax_token -> ('kind, 'text) syntax_trivia list
  end

  (** ## Serialization *)
  val to_json: kind_to_json:('kind -> Data.Json.t) ->
    text_to_json:('text -> Data.Json.t) ->
    ('kind, 'text) syntax_element ->
    Data.Json.t

  (** `to_json ~kind_to_json ~text_to_json elem` serializes a red element to
      JSON.

      Each red node/token includes its span (start/end offsets) for debugging.

      Example: ```ocaml let kind_to_json kind = Data.Json.String (show_kind
      kind) in let text_to_json text = Data.Json.String text in let red_root =
      Red.new_root green_tree in let json = Red.to_json ~kind_to_json
      ~text_to_json (Red.Node red_root) ``` *)
end

(** # Builder

    Convenient API for constructing green trees.

    The builder provides two styles:

    1. **Stack-based**: Use `create`, `start_node`, `token`,
    `token_with_leading_trivia`, `finish_node`, `build`
    2. **Direct**: Use `make_token`, `make_token_with_leading_trivia`, and
    `make_node` helper functions

    ## Stack-Based Example

    ```ocaml let green = Builder.create () |> Builder.start_node ~kind:BIN_EXPR
    |> Builder.token ~kind:INT ~text:"1" ~width:1 |> Builder.token ~kind:PLUS
    ~text:"+" ~width:1 |> Builder.token ~kind:INT ~text:"2" ~width:1 |>
    Builder.finish_node |> Builder.build ROOT ```

    ## Direct Example

    ```ocaml let green = Builder.make_node ~kind:BIN_EXPR
    [ Builder.make_token ~kind:INT ~text:"1" ~width:1; Builder.make_token
     ~kind:PLUS ~text:"+" ~width:1; Builder.make_token ~kind:INT ~text:"2"
     ~width:1; ] ``` *)

module Builder: sig
  (** Builder state (stack-based construction). *)
  (** `create ()` creates a new builder. *)
  type ('kind, 'text) t
  val create: unit -> ('kind, 'text) t

  (** `token builder ~kind ~text ~width` adds a token with no leading trivia to
      the current node. *)
  val token: ('kind, 'text) t -> kind:'kind -> text:'text -> width:int -> ('kind, 'text) t

  (** `token_with_leading_trivia builder ~leading_trivia ~kind ~text ~width`
      adds a token with explicit leading trivia to the current node. *)
  val token_with_leading_trivia:
    ('kind, 'text) t ->
    leading_trivia:('kind, 'text) Green.trivia list ->
    kind:'kind ->
    text:'text ->
    width:int ->
    ('kind, 'text) t

  (** `start_node builder ~kind` starts a new node (pushes stack frame). *)
  val start_node: ('kind, 'text) t -> kind:'kind -> ('kind, 'text) t

  (** `finish_node builder` finishes the current node (pops stack frame). *)
  val finish_node: ('kind, 'text) t -> ('kind, 'text) t

  (** `build builder default_kind` builds the final green node from the builder
      state.

      The `default_kind` is used if the builder has multiple top-level elements
      (they get wrapped in a node with this kind). *)
  val build: ('kind, 'text) t -> 'kind -> ('kind, 'text) Green.node

  (** ## Direct Construction *)
  (** `make_token ~kind ~text ~width` creates a token element directly with no
      leading trivia. *)
  val make_token: kind:'kind -> text:'text -> width:int -> ('kind, 'text) Green.element

  (** `make_token_with_leading_trivia ~leading_trivia ~kind ~text ~width`
      creates a token element directly with explicit leading trivia. *)
  val make_token_with_leading_trivia:
    leading_trivia:('kind, 'text) Green.trivia list ->
    kind:'kind ->
    text:'text ->
    width:int ->
    ('kind, 'text) Green.element

  val make_node: kind:'kind -> ('kind, 'text) Green.element list -> ('kind, 'text) Green.element

  (** `make_node ~kind elements` creates a node element directly from a list. *)
end
