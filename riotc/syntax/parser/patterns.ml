// Pattern grammar, separate from expression parsing so recursive pattern structure
// does not depend on parser expression callbacks.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use Syntax.Ast.{
  Pattern, ValueName, ConstructorName, IntegerLiteral, TextLiteral,
  Variable, PatternUnit, Wildcard, PatternInteger, PatternBoolean,
  PatternString, TuplePattern, ConstructorPattern, ConstructorPatternData,
  RecordPattern, RecordPatternData, RecordPatternField, AscribedPattern,
  AscribedPatternData
}
use Syntax.Lexer.{Token, TokenInfo, IdentifierToken, IntegerToken, TextToken, SymbolToken}
use Syntax.Parser.Common.{
  ParseStep, valid_i32_literal, take_symbol, reverse_list, is_constructor_name
}
use Syntax.Parser.Types.{parse_type}
use Syntax.Parser.Literals.{decode_string_literal}
use Syntax.Parser.PatternParts.{parse_pattern_list, parse_pattern_list_tail}
use String.{equal}

type RecordPatternState = RecordPatternState {
  constructor_name: ConstructorName,
  reversed_fields: List<RecordPatternField>
}

let parse_integer_pattern = fn digits -> fn remaining ->
  match (valid_i32_literal digits) {
    | true -> Ok(ParseStep {
      value: PatternInteger(IntegerLiteral(digits)),
      remaining_tokens: remaining
    })
    | false -> Err("integer pattern is outside i32 range")
  }

let parse_negative_pattern = fn digits -> fn remaining ->
  match (equal digits "2147483648") {
    | true -> Ok(ParseStep {
      value: PatternInteger(IntegerLiteral("-2147483648")),
      remaining_tokens: remaining
    })
    | false -> match (valid_i32_literal digits) {
      | true -> Ok(ParseStep {
        value: PatternInteger(IntegerLiteral("-" + digits)),
        remaining_tokens: remaining
      })
      | false -> Err("integer pattern is outside i32 range")
    }
  }

let parse_named_pattern = fn parse_nested_pattern -> fn name -> fn tokens ->
  match (is_constructor_name name) {
    | true -> parse_constructor_pattern parse_nested_pattern name tokens
    | false -> Ok(ParseStep {
      value: Variable(ValueName(name)),
      remaining_tokens: tokens
    })
  }

let parse_constructor_pattern = fn parse_nested_pattern -> fn name -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), remaining) ->
      parse_record_pattern_fields parse_nested_pattern (RecordPatternState {
        constructor_name: ConstructorName(name),
        reversed_fields: Nil
      }) remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), remaining) ->
      parse_constructor_pattern_arguments parse_nested_pattern name remaining
    | _ -> Ok(ParseStep {
      value: ConstructorPattern(ConstructorPatternData {
        constructor_name: ConstructorName(name),
        arguments: Nil
      }),
      remaining_tokens: tokens
    })
  }

let parse_constructor_pattern_arguments = fn parse_nested_pattern -> fn name -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining) ->
      Ok(ParseStep {
        value: ConstructorPattern(ConstructorPatternData {
          constructor_name: ConstructorName(name),
          arguments: Nil
        }),
        remaining_tokens: remaining
      })
    | _ -> match (parse_pattern_list parse_nested_pattern tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep {
          value: arguments,
          remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining)
        }) -> Ok(ParseStep {
        value: ConstructorPattern(ConstructorPatternData {
          constructor_name: ConstructorName(name),
          arguments: arguments
        }),
        remaining_tokens: remaining
      })
      | _ -> Err("expected `)` after constructor pattern")
    }
  }

let parse_parenthesized_pattern = fn parse_nested_pattern -> fn tokens ->
  match (parse_nested_pattern tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: first, remaining_tokens }) ->
      parse_parenthesized_pattern_tail parse_nested_pattern first remaining_tokens
  }

let parse_parenthesized_pattern_tail = fn parse_nested_pattern -> fn (first: Pattern) -> fn (tokens: List<Token>) ->
  (match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ":", span: _ }), remaining) ->
      match (parse_type remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep {
            value: annotation,
            remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), rest)
          }) -> Ok(ParseStep {
          value: AscribedPattern(AscribedPatternData {
            pattern: first,
            annotation: annotation
          }),
          remaining_tokens: rest
        })
        | _ -> Err("expected `)` after pattern annotation")
      }
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), _) ->
      match (parse_pattern_list_tail parse_nested_pattern (Cons(first, Nil)) tokens) {
        | Err(message) -> Err(message)
        | Ok(ParseStep {
            value: patterns,
            remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), rest)
          }) -> Ok(ParseStep {
          value: TuplePattern(patterns),
          remaining_tokens: rest
        })
        | _ -> Err("expected `)` after tuple pattern")
      }
    | Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining) ->
      Ok(ParseStep { value: first, remaining_tokens: remaining })
    | _ -> Err("expected `,`, `:`, or `)` after pattern")
  }: Result<ParseStep<Pattern>, string>)
let parse_record_pattern_fields = fn parse_nested_pattern -> fn state -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) ->
      finish_record_pattern state remaining
    | _ -> parse_record_pattern_field parse_nested_pattern state tokens
  }

let parse_record_pattern_field = fn parse_nested_pattern -> fn state -> fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), remaining) ->
      parse_record_pattern_field_value parse_nested_pattern state (ValueName(name)) remaining
    | _ -> Err("expected a record field name in pattern")
  }

let parse_record_pattern_field_value = fn parse_nested_pattern -> fn (RecordPatternState {
  constructor_name,
  reversed_fields
}) -> fn name -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ":", span: _ }), remaining) ->
      match (parse_nested_pattern remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: pattern, remaining_tokens }) ->
          continue_record_pattern_fields parse_nested_pattern constructor_name
            (Cons(RecordPatternField { name: name, pattern: pattern }, reversed_fields))
            remaining_tokens
      }
    | _ -> continue_record_pattern_fields parse_nested_pattern constructor_name
      (Cons(RecordPatternField {
        name: name,
        pattern: Variable(name)
      }, reversed_fields)) tokens
  }

let continue_record_pattern_fields = fn parse_nested_pattern -> fn constructor_name -> fn reversed_fields -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), remaining) ->
      parse_record_pattern_fields parse_nested_pattern (RecordPatternState {
        constructor_name: constructor_name,
        reversed_fields: reversed_fields
      }) remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) ->
      finish_record_pattern (RecordPatternState {
        constructor_name: constructor_name,
        reversed_fields: reversed_fields
      }) remaining
    | _ -> Err("expected `,` or `}` after record pattern field")
  }

let finish_record_pattern = fn (RecordPatternState {
  constructor_name,
  reversed_fields
}) -> fn remaining ->
  Ok(ParseStep {
    value: RecordPattern(RecordPatternData {
      constructor_name: constructor_name,
      fields: reverse_list reversed_fields
    }),
    remaining_tokens: remaining
  })

let parse_pattern_node = fn parse_nested_pattern -> fn (tokens: List<Token>) ->
  (match tokens {
    | Cons(IntegerToken(TokenInfo { lexeme: digits, span: _ }), remaining) ->
      parse_integer_pattern digits remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "-", span: _ }), Cons(IntegerToken(TokenInfo { lexeme: digits, span: _ }), remaining)) ->
      parse_negative_pattern digits remaining
    | Cons(TextToken(TokenInfo { lexeme, span: _ }), remaining) -> Ok(ParseStep {
      value: PatternString(TextLiteral(decode_string_literal lexeme)),
      remaining_tokens: remaining
    })
    | Cons(IdentifierToken(TokenInfo { lexeme: "true", span: _ }), remaining) ->
      Ok(ParseStep { value: PatternBoolean(true), remaining_tokens: remaining })
    | Cons(IdentifierToken(TokenInfo { lexeme: "false", span: _ }), remaining) ->
      Ok(ParseStep { value: PatternBoolean(false), remaining_tokens: remaining })
    | Cons(IdentifierToken(TokenInfo { lexeme: "_", span: _ }), remaining) ->
      Ok(ParseStep { value: Wildcard, remaining_tokens: remaining })
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), remaining) ->
      parse_named_pattern parse_nested_pattern name remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining)) ->
      Ok(ParseStep { value: PatternUnit, remaining_tokens: remaining })
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), remaining) ->
      parse_parenthesized_pattern parse_nested_pattern remaining
    | _ -> Err("expected a pattern")
  }: Result<ParseStep<Pattern>, string>)

let parse_pattern = fn (tokens: List<Token>) ->
  (parse_pattern_node parse_pattern tokens: Result<ParseStep<Pattern>, string>)
