open Std
open Markdown_syntax_kind
open Markdown_diagnostic

(** Parsed inline markdown content. *)
type inline_node =
  | Text of string
  | Emphasis of inline_node list
  | Strong of inline_node list
  | Strikethrough of inline_node list
  | Code_span of string
  | Hard_break
  | Raw_html of string
  | Link of {
      label: inline_node list;
      destination: string;
      title: string option;
    }
  | Image of {
      alt: inline_node list;
      destination: string;
      title: string option;
    }
(** Parsed table row. *)
type table_row = {
  (** Cell content for each table column. *)
  cells: inline_node list list;
  (** Column alignments. *)
  alignments: table_alignment list;
}

and table_alignment =
  | Default
  | Left
  | Center
  | Right
(** Parsed block-level markdown content. *)
type block_node =
  | Heading of {
      level: int;
      inlines: inline_node list;
      span: Markdown_span.t;
    }
  | Paragraph of {
      inlines: inline_node list;
      span: Markdown_span.t;
    }
  | Block_quote of {
      blocks: block_node list;
      span: Markdown_span.t;
    }
  | List of {
      ordered: bool;
      start: int;
      tight: bool;
      items: block_node list list;
      span: Markdown_span.t;
    }
  | Task_list_item of {
      checked: bool;
      blocks: block_node list;
      span: Markdown_span.t;
    }
  | List_item of {
      blocks: block_node list;
      span: Markdown_span.t;
    }
  | Code_block of {
      info: string;
      code: string;
      span: Markdown_span.t;
      fenced: bool;
    }
  | Horizontal_rule of Markdown_span.t
  | Raw_html of {
      html: string;
      span: Markdown_span.t;
    }
  | Table of {
      header: table_row;
      rows: table_row list;
      span: Markdown_span.t;
    }
  | Error_block of {
      message: string;
      span: Markdown_span.t;
    }
(** Lightweight Markdown-specific syntax token used between block parsing and lowering. *)
type syntax_token
(** Lightweight Markdown-specific syntax node used between block parsing and lowering. *)
type syntax_node

module SyntaxToken: sig
  val kind: syntax_token -> Markdown_syntax_kind.t

  val text: syntax_token -> string

  val span: syntax_token -> Markdown_span.t
end

module SyntaxNode: sig
  val kind: syntax_node -> Markdown_syntax_kind.t

  val span: syntax_node -> Markdown_span.t

  val direct_tokens: syntax_node -> syntax_token list

  val direct_nodes: syntax_node -> syntax_node list
end

(**
   Markdown flavor.

   Use [Markdown] for baseline markdown parsing and [Gfm] when you want
   GitHub-style extensions such as tables and task lists.
*)
type flavor =
  | Markdown
  | Gfm
(** Result of parsing raw markdown source. *)
type parsed = {
  (** Original source text. *)
  source: string;
  (** Low-level lexer tokens. *)
  tokens: Markdown_token.t list;
  (** Markdown-specific syntax tree. *)
  tree: syntax_node;
  (** Parse diagnostics produced while reading the source. *)
  diagnostics: Markdown_diagnostic.t list;
}
type edit = { start: int; end_: int; text: string }
type update_stats = {
  reused_prefix_blocks: int;
  reparsed_blocks: int;
  reused_suffix_blocks: int;
  reparsed_full: bool;
}
type update_result = {
  parsed: parsed;
  stats: update_stats;
}

(**
   Parse markdown source into tokens, a syntax tree, and diagnostics.

   Use this lower-level parser interface when you need direct access to the
   lexer tokens or syntax tree, not just rendered HTML.
*)
val parse: ?flavor:flavor -> string -> parsed

(**
   Apply a text edit to a previous parse result.

   Single-line edits inside simple top-level blocks are reparsed locally and
   the untouched syntax nodes are reused. Structural edits conservatively
   fall back to a full parse.
*)
val update: ?flavor:flavor -> previous:parsed -> edit:edit -> unit -> update_result
