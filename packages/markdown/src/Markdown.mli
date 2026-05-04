open Std

(** Source spans used by markdown tokens, diagnostics, and parsed blocks. *)
module Span = Markdown_span

(** Stable error identifiers used by the markdown diagnostic system. *)
module Error: sig
  type id = string

  (** Format an error identifier as a string. *)
  val to_string: id -> string

  (** Wrap a string as an error identifier. *)
  val from_string: string -> id

  (** Encode an error identifier as JSON. *)
  val to_json: id -> Data.Json.t

  (** Decode an error identifier from JSON. *)
  val from_json: Data.Json.t -> (id, string) result
end

(** Syntax kinds used by the markdown parser. *)
module Syntax_kind: sig
  type t =
    | Document
    | Heading_1
    | Heading_2
    | Heading_3
    | Heading_4
    | Heading_5
    | Heading_6
    | Paragraph
    | Block_quote
    | Ordered_list_tight
    | Ordered_list_loose
    | Unordered_list_tight
    | Unordered_list_loose
    | List_item
    | Task_list_item_checked
    | Task_list_item_unchecked
    | Fenced_code_block
    | Indented_code_block
    | Horizontal_rule
    | Raw_html_block
    | Table
    | Table_header
    | Table_row
    | Table_cell_default
    | Table_cell_left
    | Table_cell_center
    | Table_cell_right
    | Info_string
    | Raw_html
    | Text
    | Error

  val to_string: t -> string
end

(** Markdown parse diagnostic types and helpers. *)
module Diagnostic: sig
  type found_token = { kind: string; text: string }
  type kind =
    | Invalid_markdown of {
        found: found_token;
      }
    | Unsupported_feature of {
        found: found_token;
        feature: string;
      }
    | Unclosed_fenced_code_block of {
        found: found_token;
        opener: string;
      }
    | Unexpected_control_character of {
        found: found_token;
        code: int;
      }
    | Parser_internal of {
        message: string;
        found: found_token;
      }
  type t = {
    kind: kind;
    span: Markdown_span.t;
  }

  (** Create a diagnostic directly. *)
  val make: kind:kind -> span:Markdown_span.t -> t

  (** Create an invalid-markdown diagnostic. *)
  val invalid_markdown: found:found_token -> span:Markdown_span.t -> t

  (** Create an unsupported-feature diagnostic. *)
  val unsupported_feature: found:found_token -> feature:string -> span:Markdown_span.t -> t

  (** Create an unclosed-fenced-code-block diagnostic. *)
  val unclosed_fenced_code_block: found:found_token -> opener:string -> span:Markdown_span.t -> t

  (** Create an unexpected-control-character diagnostic. *)
  val unexpected_control_character: found:found_token -> code:int -> span:Markdown_span.t -> t

  (** Create a parser-internal diagnostic. *)
  val parser_internal: found:found_token -> message:string -> span:Markdown_span.t -> t

  (** Return the token that triggered the diagnostic. *)
  val found_token: t -> found_token

  (** Return the stable error identifier for the diagnostic kind. *)
  val error_id: t -> Error.id

  (** Return the string form of the error identifier. *)
  val id: t -> string

  (** Return the expected-input message for the diagnostic. *)
  val expected_message: t -> string

  (** Return an optional suggested fix message. *)
  val fix_message: t -> string option

  (** Return a short hint message. *)
  val hint_message: t -> string

  (** Return the main user-facing diagnostic message. *)
  val main_message: t -> string

  (** Encode a diagnostic as JSON. *)
  val to_json: t -> Data.Json.t

  (** Decode a diagnostic from JSON. *)
  val from_json: Data.Json.t -> (t, string) result
end

(** Render diagnostics in a source-oriented format. *)
module Diagnostic_reporter: sig
  (** Print formatted diagnostics to standard output. *)
  val print: file:string -> source:string -> Diagnostic.t list -> unit

  (** Format diagnostics as a string. *)
  val format: file:string -> source:string -> Diagnostic.t list -> string
end

(** One CommonMark spec fixture. *)
type fixture = {
  markdown: string;
  html: string;
  example: int option;
  section: string option;
}
(** High-level parse result returned by [parse] and [parse_gfm]. *)
type parse_result = {
  (** Root markdown syntax tree. *)
  root: Markdown_parser.syntax_node;
  (** Original source text. *)
  source: string;
  (** Parse diagnostics. *)
  diagnostics: Diagnostic.t list;
  (** Lowered block representation. *)
  blocks: Markdown_parser.block_node list;
}

(** Parse markdown using baseline markdown rules. *)
val parse: string -> parse_result

(** Parse markdown using GitHub-Flavored Markdown rules. *)
val parse_gfm: string -> parse_result

(** Render a parsed markdown document to HTML. *)
val to_html: parse_result -> string

(**
   Incrementally editable markdown document.

   Single-line edits inside simple top-level blocks reuse the unchanged syntax
   nodes around the edit. Structural edits conservatively fall back to a full
   parse so the public result stays equivalent to parsing from scratch.
*)
module Document: sig
  type t
  type edit = { start: int; end_: int; text: string }
  type update_stats = {
    reused_prefix_blocks: int;
    reparsed_blocks: int;
    reused_suffix_blocks: int;
    reparsed_full: bool;
  }

  val parse: string -> t

  val parse_gfm: string -> t

  val update: t -> edit:edit -> t

  val source: t -> string

  val diagnostics: t -> Diagnostic.t list

  val last_update: t -> update_stats option

  val to_parse_result: t -> parse_result

  val to_html: t -> string
end

(** Parse and render baseline markdown to HTML. *)
val compile: string -> string

(** Parse and render GitHub-Flavored Markdown to HTML. *)
val compile_gfm: string -> string

(** Return all bundled CommonMark spec fixtures. *)
val all_spec_fixtures: unit -> fixture list
