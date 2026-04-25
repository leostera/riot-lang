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
  | Link of { label: inline_node list; destination: string; title: string option }
  | Image of { alt: inline_node list; destination: string; title: string option }

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
  | Heading of { level: int; inlines: inline_node list; span: Ceibo.Span.t }
  | Paragraph of { inlines: inline_node list; span: Ceibo.Span.t }
  | Block_quote of { blocks: block_node list; span: Ceibo.Span.t }
  | List of {
    ordered: bool;
    start: int;
    tight: bool;
    items: block_node list list;
    span: Ceibo.Span.t;
  }
  | Task_list_item of { checked: bool; blocks: block_node list; span: Ceibo.Span.t }
  | List_item of { blocks: block_node list; span: Ceibo.Span.t }
  | Code_block of { info: string; code: string; span: Ceibo.Span.t; fenced: bool }
  | Horizontal_rule of Ceibo.Span.t
  | Raw_html of { html: string; span: Ceibo.Span.t }
  | Table of { header: table_row; rows: table_row list; span: Ceibo.Span.t }
  | Error_block of { message: string; span: Ceibo.Span.t }

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
  (** Green syntax tree. *)
  tree: (Markdown_syntax_kind.t, string) Ceibo.Green.node;
  (** Parse diagnostics produced while reading the source. *)
  diagnostics: Markdown_diagnostic.t list;
}

(**
   Parse markdown source into tokens, a green tree, and lowered block nodes.

   Use this lower-level parser interface when you need direct access to the
   lexer tokens or syntax tree, not just rendered HTML.
*)
val parse: ?flavor:flavor -> string -> parsed
