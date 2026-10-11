// Entry-point parsing for the native compiler's initial `main` contract.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Syntax.Ast.{
  Expr, Program, SourceModule, ValueDeclaration, ValueDeclarationData,
  ValueName, Main, Function, FunctionExpression, PatternUnit
}
use Syntax.Lexer.{SourceSpan, UnterminatedString, UnterminatedEscape, lex}
use Syntax.Parser.Common.{ParseStep}
use Syntax.Parser.SourceModule.{parse_source_module_tokens}

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

let finish_main_declaration = fn source_module ->
  match source_module {
    | SourceModule {
        imports: Nil,
        declarations: Cons(ValueDeclaration(ValueDeclarationData {
          name: ValueName("main"),
          body
        }), Nil)
      } -> finish_main_expression body Nil
    | _ -> Err(parser_error "expected a module containing only `let main`")
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
    | Ok(tokens) -> match (parse_source_module_tokens tokens) {
      | Err(message) -> Err(parser_error message)
      | Ok(source_module) -> finish_main_declaration source_module
    }
  }

let parse_error_message = fn (ParserError { message, source_span: _ }) -> message
let parse_error_span = fn (ParserError { message: _, source_span }) -> source_span
