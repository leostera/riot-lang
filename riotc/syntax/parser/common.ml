// Shared token-stream results and small parsing primitives.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Syntax.Lexer.{Token, TokenInfo, SourceSpan, LexError, UnterminatedString, UnterminatedEscape, IdentifierToken, SymbolToken}
use String.{length, get, slice, equal}

type ParseStep<'a> = ParseStep {
  value: 'a,
  remaining_tokens: List<Token>
}

type ParserError = ParserError {
  message: string,
  source_span: Option<SourceSpan>
}

type DecimalScan = DecimalScan {
  digits: string,
  character_index: i32,
  value: i32
}

let parse_decimal = fn (DecimalScan { digits, character_index, value }) ->
  match (get digits character_index) {
    | Some(codepoint) -> parse_decimal (DecimalScan {
      digits: digits,
      character_index: character_index + 1,
      value: value * 10 + codepoint - 48
    })
    | None -> value
  }

let valid_i32_literal = fn digits ->
  match (length digits <= 9) {
    | true -> true
    | false -> match (length digits == 10) {
      | false -> false
      | true -> {
        let first_nine = parse_decimal (DecimalScan {
          digits: slice digits 0 9,
          character_index: 0,
          value: 0
        });
        match (first_nine < 214748364) {
          | true -> true
          | false -> match (first_nine == 214748364) {
            | true -> match (get digits 9) {
              | Some(last_digit) -> last_digit <= 55
              | None -> false
            }
            | false -> false
          }
        }
      }
    }
  }

let is_constructor_name = fn name ->
  match (get name 0) {
    | Some(codepoint) -> match (codepoint >= 65) {
      | true -> codepoint <= 90
      | false -> false
    }
    | None -> false
  }

let take_identifier = fn expected -> fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: actual, span: _ }), remaining) ->
      match (equal actual expected) {
        | true -> Ok(remaining)
        | false -> Err("expected `" + expected + "`")
      }
    | _ -> Err("expected `" + expected + "`")
  }

let take_symbol = fn expected -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: actual, span: _ }), remaining) ->
      match (equal actual expected) {
        | true -> Ok(remaining)
        | false -> Err("expected `" + expected + "`")
      }
    | _ -> Err("expected `" + expected + "`")
  }

let reverse_accumulator = fn remaining -> fn reversed ->
  match remaining {
    | Nil -> reversed
    | Cons(item, rest) -> reverse_accumulator rest (Cons(item, reversed))
  }

let reverse_list = fn values -> reverse_accumulator values Nil

let parser_error_from_lexer = fn error ->
  match error {
    | UnterminatedString(span) -> ParserError {
      message: "unterminated string literal",
      source_span: Some(span)
    }
    | UnterminatedEscape(span) -> ParserError {
      message: "unterminated escape in string literal",
      source_span: Some(span)
    }
  }

let parse_error_message = fn (ParserError { message, source_span: _ }) -> message

let parse_error_span = fn (ParserError { message: _, source_span }) -> source_span
