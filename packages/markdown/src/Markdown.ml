open Std

module Error = Markdown_error
module Span = Markdown_span
module Syntax_kind = Markdown_syntax_kind
module Diagnostic = Markdown_diagnostic
module Diagnostic_reporter = Markdown_diagnostic_reporter

type parse_result = {
  root: Markdown_parser.syntax_node;
  source: string;
  diagnostics: Diagnostic.t list;
  blocks: Markdown_parser.block_node list;
}

type flavor =
  | Markdown
  | Gfm

let parse_flavor = fun __tmp1 ->
  match __tmp1 with
  | Markdown -> Markdown_parser.Markdown
  | Gfm -> Markdown_parser.Gfm

let lower_flavor = fun __tmp1 ->
  match __tmp1 with
  | Markdown -> Markdown_parser.Markdown
  | Gfm -> Markdown_parser.Gfm

let parse_result_of_parsed = fun flavor (parsed: Markdown_parser.parsed) ->
  let blocks = Markdown_lower.lower ~flavor:(lower_flavor flavor) parsed.tree in
  {
    root = parsed.tree;
    source = parsed.source;
    diagnostics = parsed.diagnostics;
    blocks;
  }

let parse = fun source ->
  let parsed = Markdown_parser.parse source in
  parse_result_of_parsed Markdown parsed

let parse_gfm = fun source ->
  let parsed = Markdown_parser.parse ~flavor:Markdown_parser.Gfm source in
  parse_result_of_parsed Gfm parsed

let to_html = fun parse_result -> Markdown_renderer.render parse_result.blocks

let compile = fun source ->
  let parse_result = parse source in
  to_html parse_result

let compile_gfm = fun source ->
  let parse_result = parse_gfm source in
  to_html parse_result

module Document = struct
  type edit = Markdown_parser.edit = { start: int; end_: int; text: string }

  type update_stats = Markdown_parser.update_stats = {
    reused_prefix_blocks: int;
    reparsed_blocks: int;
    reused_suffix_blocks: int;
    reparsed_full: bool;
  }

  type t = {
    flavor: flavor;
    parsed: Markdown_parser.parsed;
    last_update: update_stats option;
  }

  let parse_with_flavor = fun flavor source -> {
    flavor;
    parsed = Markdown_parser.parse ~flavor:(parse_flavor flavor) source;
    last_update = None;
  }

  let parse = fun source -> parse_with_flavor Markdown source

  let parse_gfm = fun source -> parse_with_flavor Gfm source

  let update = fun t ~edit ->
    let result =
      Markdown_parser.update ~flavor:(parse_flavor t.flavor) ~previous:t.parsed ~edit ()
    in
    { t with parsed = result.parsed; last_update = Some result.stats }

  let source = fun t -> t.parsed.source

  let diagnostics = fun t -> t.parsed.diagnostics

  let last_update = fun t -> t.last_update

  let to_parse_result = fun t -> parse_result_of_parsed t.flavor t.parsed

  let to_html = fun t ->
    let parse_result = to_parse_result t in
    Markdown_renderer.render parse_result.blocks
end
