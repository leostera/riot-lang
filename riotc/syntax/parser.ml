// Entry-point parsing for the native compiler's initial `main` contract.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Syntax.Ast.{
  Expr, Program, Main, Function, FunctionExpression, PatternUnit
}
use Syntax.Lexer.{SourceSpan, UnterminatedString, UnterminatedEscape, lex}
use Syntax.Parser.Common.{ParseStep, take_identifier, take_symbol}
use Syntax.Parser.Expressions.{parse_expression}

type ParserError = ParserError {
  message: string,
  source_span: Option<SourceSpan>
}

let parser_error = fn message -> ParserError {
  message: message,
  source_span: None
}

let finish_main_expression = fn expression -> fn remaining_tokens ->
  match expression {
    | Function(FunctionExpression {
        parameter: PatternUnit,
        body
      }) -> match remaining_tokens {
      | Nil -> Ok(Main(body))
      | _ -> Err(parser_error "unexpected tokens after the main function")
    }
    | _ -> Err(parser_error "expected `main` to be a function from unit")
  }

let parse_main_expression = fn tokens ->
  match (parse_expression tokens) {
    | Err(message) -> Err(parser_error message)
    | Ok(ParseStep { value: expression, remaining_tokens }) ->
      finish_main_expression expression remaining_tokens
  }

let parse_main_binding = fn tokens ->
  match (take_identifier "let" tokens) {
    | Err(message) -> Err(parser_error message)
    | Ok(name_tokens) -> match (take_identifier "main" name_tokens) {
      | Err(message) -> Err(parser_error message)
      | Ok(after_name) -> match (take_symbol "=" after_name) {
        | Err(message) -> Err(parser_error message)
        | Ok(expression_tokens) -> parse_main_expression expression_tokens
      }
    }
  }

let parse_program = fn source ->
  match (lex source) {
    | Err(error) -> match error {
      | UnterminatedString(span) -> Err(ParserError {
        message: "unterminated string literal",
        source_span: Some(span)
      })
      | UnterminatedEscape(span) -> Err(ParserError {
        message: "unterminated escape in string literal",
        source_span: Some(span)
      })
    }
    | Ok(tokens) -> parse_main_binding tokens
  }

let parse_error_message = fn (ParserError { message, source_span: _ }) -> message
let parse_error_span = fn (ParserError { message: _, source_span }) -> source_span
