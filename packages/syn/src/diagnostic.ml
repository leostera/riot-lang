open Std

(** Structured parse error kinds *)
type kind =
  | MissingToken of { expected : string }
  | UnexpectedToken of { expected : string option; found : string }
  | UnexpectedEof of { expected : string }
  | InvalidSyntax of { context : string }
  | UnclosedDelimiter of { delimiter : string; opened_at : int }
  | MismatchedDelimiter of { expected : string; found : string }

type t = { kind : kind; span : Ceibo.Span.t }
(** Parse error information *)

let make ~kind ~span = { kind; span }

let make_missing_token ~expected ~span =
  make ~kind:(MissingToken { expected }) ~span

let make_unexpected_token ~expected ~found ~span =
  make ~kind:(UnexpectedToken { expected; found }) ~span

let make_unexpected_eof ~expected ~span =
  make ~kind:(UnexpectedEof { expected }) ~span

let make_invalid_syntax ~context ~span =
  make ~kind:(InvalidSyntax { context }) ~span

let make_unclosed_delimiter ~delimiter ~opened_at ~span =
  make ~kind:(UnclosedDelimiter { delimiter; opened_at }) ~span

let make_mismatched_delimiter ~expected ~found ~span =
  make ~kind:(MismatchedDelimiter { expected; found }) ~span

let kind_to_message = function
  | MissingToken { expected } -> format "Missing %s" expected
  | UnexpectedToken { expected = Some exp; found } ->
      format "Expected %s but found %s" exp found
  | UnexpectedToken { expected = None; found } ->
      format "Unexpected token %s" found
  | UnexpectedEof { expected } ->
      format "Unexpected end of file, expected %s" expected
  | InvalidSyntax { context } -> format "Invalid syntax in %s" context
  | UnclosedDelimiter { delimiter; opened_at } ->
      format "Unclosed %s (opened at position %d)" delimiter opened_at
  | MismatchedDelimiter { expected; found } ->
      format "Mismatched delimiter: expected %s but found %s" expected found

let to_string err =
  format "Parse error at %s: %s"
    (Ceibo.Span.to_string err.span)
    (kind_to_message err.kind)

(** Convert diagnostic to JSON for machine consumption *)
let to_json err =
  let open Data.Json in
  let kind_obj =
    match err.kind with
    | MissingToken { expected } ->
        Object
          [ ("type", String "missing_token"); ("expected", String expected) ]
    | UnexpectedToken { expected = Some exp; found } ->
        Object
          [
            ("type", String "unexpected_token");
            ("expected", String exp);
            ("found", String found);
          ]
    | UnexpectedToken { expected = None; found } ->
        Object [ ("type", String "unexpected_token"); ("found", String found) ]
    | UnexpectedEof { expected } ->
        Object
          [ ("type", String "unexpected_eof"); ("expected", String expected) ]
    | InvalidSyntax { context } ->
        Object
          [ ("type", String "invalid_syntax"); ("context", String context) ]
    | UnclosedDelimiter { delimiter; opened_at } ->
        Object
          [
            ("type", String "unclosed_delimiter");
            ("delimiter", String delimiter);
            ("opened_at", Int opened_at);
          ]
    | MismatchedDelimiter { expected; found } ->
        Object
          [
            ("type", String "mismatched_delimiter");
            ("expected", String expected);
            ("found", String found);
          ]
  in
  Object
    [
      ("kind", kind_obj);
      ( "span",
        Object [ ("start", Int err.span.start); ("end", Int err.span.end_) ] );
    ]

(* parse_result removed - now in Parser module *)
