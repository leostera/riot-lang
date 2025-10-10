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

module Span : sig
  type t = { start : int; end_ : int }
  (** Span type representing a range in source text. *)

  val make : start:int -> end_:int -> t
  (** `make ~start ~end_` creates a span. *)

  val length : t -> int
  (** `length span` returns the length of the span. *)

  val contains : t -> int -> bool
  (** `contains span offset` checks if offset is within span. *)

  val overlaps : t -> t -> bool
  (** `overlaps s1 s2` checks if two spans overlap. *)

  val union : t -> t -> t
  (** `union s1 s2` creates a span covering both spans. *)

  val to_string : t -> string
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

module Green : sig
  (** ## Types *)

  type ('kind, 'text) token
  (** Green token - leaf node containing source text.

      Tokens represent atomic elements like keywords, identifiers, and literals.
      Parametrized by:
      - `'kind`: The type used to identify token kinds (e.g., an enum)
      - `'text`: The type used to store text (e.g., string or rope)

      Each token has:
      - A kind (what type of token)
      - The source text
      - Width (byte length for positioning) *)

  type ('kind, 'text) node
  (** Green node - interior node with children.

      Nodes represent grammatical constructions like expressions and
      declarations. Parametrized by the same `'kind` and `'text` as tokens.

      Each node has:
      - A kind (what type of node)
      - Children (array of tokens/nodes)
      - Width (total byte length of all children, cached) *)

  (** Element can be either a token or a node.

      Most tree operations work on elements generically, not caring whether
      they're dealing with tokens or nodes. *)
  type ('kind, 'text) element =
    | Token of ('kind, 'text) token
    | Node of ('kind, 'text) node

  (** ## Construction *)

  val make_token : kind:'kind -> text:'text -> width:int -> ('kind, 'text) token
  (** `make_token ~kind ~text ~width` creates a new token.

      The width should be the byte length of the text for correct positioning.

      Example: `make_token ~kind:IDENT ~text:"foo" ~width:3` *)

  val make_node :
    kind:'kind -> children:('kind, 'text) element array -> ('kind, 'text) node
  (** `make_node ~kind ~children` creates a new node from an array of children.

      The width is automatically computed as the sum of all children widths.

      Example: `make_node ~kind:BIN_EXPR ~children:[|tok1; tok2; tok3|]` *)

  val make_node_list :
    kind:'kind -> ('kind, 'text) element list -> ('kind, 'text) node
  (** `make_node_list ~kind elements` creates a node from a list.

      Convenience wrapper around `make_node` that converts a list to an array.

      Example: `make_node_list ~kind:TUPLE_EXPR [elem1; elem2; elem3]` *)

  (** ## Accessors *)

  val width : ('kind, 'text) element -> int
  (** `width elem` returns the total byte length of the element.

      For tokens, this is the width provided at construction. For nodes, this is
      the sum of all children widths (cached).

      This is O(1) due to caching. *)

  val kind : ('kind, 'text) element -> 'kind
  (** `kind elem` returns the kind identifier. *)

  val text : ('kind, 'text) element -> 'text option
  (** `text elem` returns the text if element is a token, None if it's a node.
  *)

  val is_token : ('kind, 'text) element -> bool
  (** `is_token elem` returns true if element is a token. *)

  val is_node : ('kind, 'text) element -> bool
  (** `is_node elem` returns true if element is a node. *)

  (** ## Node Operations *)

  val replace_child :
    ('kind, 'text) node ->
    index:int ->
    child:('kind, 'text) element ->
    ('kind, 'text) node
  (** `replace_child node ~index ~child` creates a new node with one child
      replaced.

      This enables structural sharing - the new node shares all children except
      the replaced one with the original node.

      Example: `replace_child node ~index:2 ~child:new_elem` *)

  val append_child :
    ('kind, 'text) node -> child:('kind, 'text) element -> ('kind, 'text) node
  (** `append_child node ~child` creates a new node with a child appended. *)

  val child_count : ('kind, 'text) node -> int
  (** `child_count node` returns the number of children. *)

  val child : ('kind, 'text) node -> int -> ('kind, 'text) element option
  (** `child node i` returns the child at index `i`, or `None` if out of bounds.
  *)

  val children : ('kind, 'text) node -> ('kind, 'text) element array
  (** `children node` returns all children as an array. *)

  (** ## Serialization *)

  val to_json :
    kind_to_json:('kind -> Data.Json.t) ->
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

module Red : sig
  (** ## Types *)

  type ('kind, 'text) syntax_node
  (** Syntax node - positioned view of green node. *)

  type ('kind, 'text) syntax_token
  (** Syntax token - positioned view of green token. *)

  (** Generic syntax element for tree traversal. *)
  type ('kind, 'text) syntax_element =
    | Node of ('kind, 'text) syntax_node
    | Token of ('kind, 'text) syntax_token

  (** ## Construction *)

  val new_root : ('kind, 'text) Green.node -> ('kind, 'text) syntax_node
  (** `new_root green` creates a root red node at offset 0. *)

  (** ## SyntaxNode Operations *)

  module SyntaxNode : sig
    val green : ('kind, 'text) syntax_node -> ('kind, 'text) Green.node
    (** `green node` returns the underlying green node. *)

    val offset : ('kind, 'text) syntax_node -> int
    (** `offset node` returns the absolute byte offset in source. *)

    val span : ('kind, 'text) syntax_node -> Span.t
    (** `span node` returns the source span covered by the node. *)

    val parent : ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
    (** `parent node` returns the parent node, or `None` if root. *)

    val child_count : ('kind, 'text) syntax_node -> int
    (** `child_count node` returns the number of children. *)

    val child :
      ('kind, 'text) syntax_node -> int -> ('kind, 'text) syntax_element option
    (** `child node i` returns the child at index `i` (lazy fabrication). *)

    val children :
      ('kind, 'text) syntax_node -> ('kind, 'text) syntax_element array
    (** `children node` returns all children. *)

    val kind : ('kind, 'text) syntax_node -> 'kind
    (** `kind node` returns the kind from the underlying green node. *)

    val next_sibling :
      ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
    (** `next_sibling node` returns the next sibling, or `None` if last. *)

    val prev_sibling :
      ('kind, 'text) syntax_node -> ('kind, 'text) syntax_node option
    (** `prev_sibling node` returns the previous sibling, or `None` if first. *)

    val first_token :
      ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option
    (** `first_token node` finds the leftmost token in the subtree. *)

    val last_token :
      ('kind, 'text) syntax_node -> ('kind, 'text) syntax_token option
    (** `last_token node` finds the rightmost token in the subtree. *)

    val preorder :
      ('kind, 'text) syntax_node ->
      (('kind, 'text) syntax_element -> unit) ->
      unit
    (** `preorder node f` visits nodes in pre-order. *)

    val postorder :
      ('kind, 'text) syntax_node ->
      (('kind, 'text) syntax_element -> unit) ->
      unit
    (** `postorder node f` visits nodes in post-order. *)
  end

  (** ## SyntaxToken Operations *)

  module SyntaxToken : sig
    val green : ('kind, 'text) syntax_token -> ('kind, 'text) Green.token
    (** `green token` returns the underlying green token. *)

    val offset : ('kind, 'text) syntax_token -> int
    (** `offset token` returns the absolute byte offset in source. *)

    val span : ('kind, 'text) syntax_token -> Span.t
    (** `span token` returns the source span covered by the token. *)

    val kind : ('kind, 'text) syntax_token -> 'kind
    (** `kind token` returns the kind from the underlying green token. *)

    val text : ('kind, 'text) syntax_token -> 'text
    (** `text token` returns the text from the underlying green token. *)
  end

  (** ## Serialization *)

  val to_json :
    kind_to_json:('kind -> Data.Json.t) ->
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

    1. **Stack-based**: Use `create`, `start_node`, `token`, `finish_node`,
    `build` 2. **Direct**: Use `make_token` and `make_node` helper functions

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

module Builder : sig
  type ('kind, 'text) t
  (** Builder state (stack-based construction). *)

  val create : unit -> ('kind, 'text) t
  (** `create ()` creates a new builder. *)

  val token :
    ('kind, 'text) t ->
    kind:'kind ->
    text:'text ->
    width:int ->
    ('kind, 'text) t
  (** `token builder ~kind ~text ~width` adds a token to the current node. *)

  val start_node : ('kind, 'text) t -> kind:'kind -> ('kind, 'text) t
  (** `start_node builder ~kind` starts a new node (pushes stack frame). *)

  val finish_node : ('kind, 'text) t -> ('kind, 'text) t
  (** `finish_node builder` finishes the current node (pops stack frame). *)

  val build : ('kind, 'text) t -> 'kind -> ('kind, 'text) Green.node
  (** `build builder default_kind` builds the final green node from the builder
      state.

      The `default_kind` is used if the builder has multiple top-level elements
      (they get wrapped in a node with this kind). *)

  (** ## Direct Construction *)

  val make_token :
    kind:'kind -> text:'text -> width:int -> ('kind, 'text) Green.element
  (** `make_token ~kind ~text ~width` creates a token element directly. *)

  val make_node :
    kind:'kind ->
    ('kind, 'text) Green.element list ->
    ('kind, 'text) Green.element
  (** `make_node ~kind elements` creates a node element directly from a list. *)
end
