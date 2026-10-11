// Parser for the RiotML bootstrap language.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Syntax.Ast.{
  Expr, Program, TypeExpr, TypeName, TypeVariableName, IntegerLiteral,
  Integer, Boolean, Unit, Binary, BinaryExpression, BinaryOperator, Addition,
  Subtraction, Multiplication, Division, Remainder, Equal, NotEqual,
  LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Negate, Main,
  NamedType, NamedTypeExpression, ArrowType, ArrowTypeExpression,
  TupleType, TypeVariable, Pattern, PatternBoolean, PatternInteger,
  Wildcard, Match, MatchExpression, MatchArm
}
use Syntax.Lexer.{
  Token, TokenInfo, SourceSpan, LexError, UnterminatedString, UnterminatedEscape,
  IdentifierToken, IntegerToken, TextToken, SymbolToken, lex
}
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

type TypeListState = TypeListState {
  reversed_types: List<TypeExpr>,
  remaining_tokens: List<Token>
}

type BinaryParseRequest = BinaryParseRequest {
  left_expression: Expr,
  operator: BinaryOperator,
  right_tokens: List<Token>
}

type MatchParseState = MatchParseState {
  scrutinee: Expr,
  reversed_arms: List<MatchArm>
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

let take_identifier = fn expected -> fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: actual, span: _ }), remaining) -> match (equal actual expected) {
      | true -> Ok(remaining)
      | false -> Err("expected `" + expected + "`")
    }
    | _ -> Err("expected `" + expected + "`")
  }

let take_symbol = fn expected -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: actual, span: _ }), remaining) -> match (equal actual expected) {
      | true -> Ok(remaining)
      | false -> Err("expected `" + expected + "`")
    }
    | _ -> Err("expected `" + expected + "`")
  }

let parse_type = fn tokens ->
  match (parse_type_atom tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: left_type, remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: "->", span: _ }), rest) }) ->
      match (parse_type rest) {
        | Ok(ParseStep { value: right_type, remaining_tokens }) ->
          Ok(ParseStep {
            value: ArrowType(ArrowTypeExpression {
              argument: left_type,
              result: right_type
            }),
            remaining_tokens: remaining_tokens
          })
        | Err(message) -> Err(message)
      }
    | Ok(type_step) -> Ok(type_step)
  }

let parse_type_atom = fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "'", span: _ }), Cons(IdentifierToken(TokenInfo { lexeme: variable, span: _ }), remaining)) ->
      Ok(ParseStep {
        value: TypeVariable(TypeVariableName(variable)),
        remaining_tokens: remaining
      })
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), rest) -> parse_parenthesized_type rest
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), Cons(SymbolToken(TokenInfo { lexeme: "<", span: _ }), rest)) ->
      parse_named_type_arguments name rest
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), rest) -> Ok(ParseStep {
      value: NamedType(NamedTypeExpression {
        name: TypeName(name),
        arguments: Nil
      }),
      remaining_tokens: rest
    })
    | _ -> Err("expected a type")
  }

let parse_parenthesized_type = fn tokens ->
  match (parse_type_list tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep {
        value: Cons(single_type, Nil),
        remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining)
      }) -> Ok(ParseStep { value: single_type, remaining_tokens: remaining })
    | Ok(ParseStep { value: types, remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining) }) ->
      Ok(ParseStep { value: TupleType(types), remaining_tokens: remaining })
    | _ -> Err("expected `)` after tuple type")
  }

let parse_named_type_arguments = fn name -> fn tokens ->
  match (parse_type_list tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep {
        value: arguments,
        remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ">", span: _ }), remaining)
      }) -> Ok(ParseStep {
      value: NamedType(NamedTypeExpression {
        name: TypeName(name),
        arguments: arguments
      }),
      remaining_tokens: remaining
    })
    | _ -> Err("expected `>` after type arguments")
  }

let parse_type_list = fn tokens ->
  match (parse_type tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: first_type, remaining_tokens }) ->
      parse_type_list_tail (TypeListState {
        reversed_types: Cons(first_type, Nil),
        remaining_tokens: remaining_tokens
      })
  }

let parse_type_list_tail = fn (TypeListState { reversed_types, remaining_tokens }) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), rest) -> match (parse_type rest) {
      | Ok(ParseStep { value: next_type, remaining_tokens: remaining }) ->
        parse_type_list_tail (TypeListState {
          reversed_types: Cons(next_type, reversed_types),
          remaining_tokens: remaining
        })
      | Err(message) -> Err(message)
    }
    | _ -> Ok(ParseStep {
      value: reverse_types reversed_types,
      remaining_tokens: remaining_tokens
    })
  }

let reverse_type_accumulator = fn remaining -> fn reversed ->
  match remaining {
    | Nil -> reversed
    | Cons(ty, rest) -> reverse_type_accumulator rest (Cons(ty, reversed))
  }

let reverse_types = fn types -> reverse_type_accumulator types Nil

let parse_integer = fn digits -> fn remaining_tokens ->
  match (valid_i32_literal digits) {
    | true -> Ok(ParseStep {
      value: Integer(IntegerLiteral(digits)),
      remaining_tokens: remaining_tokens
    })
    | false -> Err("integer literal is outside i32 range")
  }

let parse_negative_integer = fn digits -> fn remaining_tokens ->
  match (equal digits "2147483648") {
    | true -> Ok(ParseStep {
      value: Integer(IntegerLiteral("-2147483648")),
      remaining_tokens: remaining_tokens
    })
    | false -> match (valid_i32_literal digits) {
      | true -> Ok(ParseStep {
        value: Negate(Integer(IntegerLiteral(digits))),
        remaining_tokens: remaining_tokens
      })
      | false -> Err("integer literal is outside i32 range")
    }
  }

let parse_parenthesized_expression = fn tokens ->
  match (parse_expr tokens) {
    | Ok(ParseStep {
        value: expression,
        remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining)
      }) -> Ok(ParseStep { value: expression, remaining_tokens: remaining })
    | Ok(_) -> Err("expected `)` after expression")
    | Err(message) -> Err(message)
  }

let parse_primary = fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "-", span: _ }), Cons(IntegerToken(TokenInfo { lexeme: digits, span: _ }), remaining)) ->
      parse_negative_integer digits remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "-", span: _ }), remaining) -> match (parse_primary remaining) {
      | Ok(ParseStep { value: expression, remaining_tokens }) -> Ok(ParseStep {
        value: Negate(expression),
        remaining_tokens: remaining_tokens
      })
      | Err(message) -> Err(message)
    }
    | Cons(IntegerToken(TokenInfo { lexeme: digits, span: _ }), remaining) -> parse_integer digits remaining
    | Cons(IdentifierToken(TokenInfo { lexeme: "true", span: _ }), remaining) -> Ok(ParseStep {
      value: Boolean(true),
      remaining_tokens: remaining
    })
    | Cons(IdentifierToken(TokenInfo { lexeme: "false", span: _ }), remaining) -> Ok(ParseStep {
      value: Boolean(false),
      remaining_tokens: remaining
    })
    | Cons(IdentifierToken(TokenInfo { lexeme: "match", span: _ }), remaining) ->
      parse_match_expression remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining)) ->
      Ok(ParseStep { value: Unit, remaining_tokens: remaining })
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), remaining) -> parse_parenthesized_expression remaining
    | Cons(TextToken(_), _) -> Err("text expressions are not supported by the native compiler slice")
    | _ -> Err("expected an expression")
  }

let make_binary_expression = fn operator -> fn left -> fn right ->
  Binary(BinaryExpression {
    operator: operator,
    left: left,
    right: right
  })

let parse_binary_right = fn (BinaryParseRequest {
  left_expression,
  operator,
  right_tokens
}) -> fn parse_right ->
  match (parse_right right_tokens) {
    | Ok(ParseStep { value: right_expression, remaining_tokens }) -> Ok(ParseStep {
      value: make_binary_expression operator left_expression right_expression,
      remaining_tokens: remaining_tokens
    })
    | Err(message) -> Err(message)
  }

let parse_term = fn tokens ->
  match (parse_primary tokens) {
    | Ok(parsed) -> parse_term_tail parsed
    | Err(message) -> Err(message)
  }

let parse_term_tail = fn (ParseStep { value: left_expression, remaining_tokens }) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "*", span: _ }), rest) -> parse_term_operator left_expression Multiplication rest
    | Cons(SymbolToken(TokenInfo { lexeme: "/", span: _ }), rest) -> parse_term_operator left_expression Division rest
    | Cons(SymbolToken(TokenInfo { lexeme: "%", span: _ }), rest) -> parse_term_operator left_expression Remainder rest
    | _ -> Ok(ParseStep { value: left_expression, remaining_tokens: remaining_tokens })
  }

let parse_term_operator = fn left_expression -> fn operator -> fn right_tokens -> {
  let request = BinaryParseRequest {
    left_expression: left_expression,
    operator: operator,
    right_tokens: right_tokens
  };
  parse_term_tail_result (parse_binary_right request parse_primary)
}

let parse_term_tail_result = fn result ->
  match result {
    | Ok(parsed) -> parse_term_tail parsed
    | Err(message) -> Err(message)
  }

let parse_sum = fn tokens ->
  match (parse_term tokens) {
    | Ok(parsed) -> parse_sum_tail parsed
    | Err(message) -> Err(message)
  }

let parse_sum_tail = fn (ParseStep { value: left_expression, remaining_tokens }) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "+", span: _ }), rest) -> parse_sum_operator left_expression Addition rest
    | Cons(SymbolToken(TokenInfo { lexeme: "-", span: _ }), rest) -> parse_sum_operator left_expression Subtraction rest
    | _ -> Ok(ParseStep { value: left_expression, remaining_tokens: remaining_tokens })
  }

let parse_sum_operator = fn left_expression -> fn operator -> fn right_tokens -> {
  let request = BinaryParseRequest {
    left_expression: left_expression,
    operator: operator,
    right_tokens: right_tokens
  };
  parse_sum_tail_result (parse_binary_right request parse_term)
}

let parse_sum_tail_result = fn result ->
  match result {
    | Ok(parsed) -> parse_sum_tail parsed
    | Err(message) -> Err(message)
  }

let parse_expr = fn tokens ->
  match (parse_sum tokens) {
    | Ok(parsed) -> parse_comparison_tail parsed
    | Err(message) -> Err(message)
  }

let parse_comparison_tail = fn (ParseStep { value: left_expression, remaining_tokens }) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "==", span: _ }), rest) ->
      parse_comparison_operator left_expression Equal rest
    | Cons(SymbolToken(TokenInfo { lexeme: "!=", span: _ }), rest) ->
      parse_comparison_operator left_expression NotEqual rest
    | Cons(SymbolToken(TokenInfo { lexeme: "<", span: _ }), rest) ->
      parse_comparison_operator left_expression LessThan rest
    | Cons(SymbolToken(TokenInfo { lexeme: "<=", span: _ }), rest) ->
      parse_comparison_operator left_expression LessThanOrEqual rest
    | Cons(SymbolToken(TokenInfo { lexeme: ">", span: _ }), rest) ->
      parse_comparison_operator left_expression GreaterThan rest
    | Cons(SymbolToken(TokenInfo { lexeme: ">=", span: _ }), rest) ->
      parse_comparison_operator left_expression GreaterThanOrEqual rest
    | _ -> Ok(ParseStep {
      value: left_expression,
      remaining_tokens: remaining_tokens
    })
  }

let parse_comparison_operator = fn left_expression -> fn operator -> fn right_tokens ->
  match (parse_sum right_tokens) {
    | Ok(ParseStep { value: right_expression, remaining_tokens }) ->
      parse_comparison_tail_result (Ok(ParseStep {
        value: make_binary_expression operator left_expression right_expression,
        remaining_tokens: remaining_tokens
      }))
    | Err(message) -> Err(message)
  }

let parse_comparison_tail_result = fn result ->
  match result {
    | Ok(parsed) -> parse_comparison_tail parsed
    | Err(message) -> Err(message)
  }

let parse_match_expression = fn tokens ->
  match (parse_expr tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: scrutinee, remaining_tokens }) ->
      parse_match_opening scrutinee remaining_tokens
  }

let parse_match_opening = fn scrutinee -> fn tokens ->
  match (take_symbol "{" tokens) {
    | Err(message) -> Err(message)
    | Ok(arm_tokens) -> parse_match_arms (MatchParseState {
      scrutinee: scrutinee,
      reversed_arms: Nil
    }) arm_tokens
  }

let parse_match_pattern = fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: "true", span: _ }), remaining) ->
      Ok(ParseStep { value: PatternBoolean(true), remaining_tokens: remaining })
    | Cons(IdentifierToken(TokenInfo { lexeme: "false", span: _ }), remaining) ->
      Ok(ParseStep { value: PatternBoolean(false), remaining_tokens: remaining })
    | Cons(IdentifierToken(TokenInfo { lexeme: "_", span: _ }), remaining) ->
      Ok(ParseStep { value: Wildcard, remaining_tokens: remaining })
    | Cons(IntegerToken(TokenInfo { lexeme: digits, span: _ }), remaining) ->
      match (valid_i32_literal digits) {
        | true -> Ok(ParseStep {
          value: PatternInteger(IntegerLiteral(digits)),
          remaining_tokens: remaining
        })
        | false -> Err("integer pattern is outside i32 range")
      }
    | _ -> Err("expected a boolean, integer, or wildcard match pattern")
  }

let parse_match_arms = fn state -> fn tokens ->
  match (take_symbol "|" tokens) {
    | Err(message) -> Err(message)
    | Ok(pattern_tokens) -> parse_match_arm_pattern state pattern_tokens
  }

let parse_match_arm_pattern = fn state -> fn tokens ->
  match (parse_match_pattern tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: pattern, remaining_tokens }) ->
      parse_match_arm_arrow state pattern remaining_tokens
  }

let parse_match_arm_arrow = fn state -> fn pattern -> fn tokens ->
  match (take_symbol "->" tokens) {
    | Err(message) -> Err(message)
    | Ok(body_tokens) -> parse_match_arm_body state pattern body_tokens
  }

let parse_match_arm_body = fn state -> fn pattern -> fn tokens ->
  match (parse_expr tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: body, remaining_tokens }) ->
      finish_match_arm state pattern body remaining_tokens
  }

let finish_match_arm = fn (MatchParseState { scrutinee, reversed_arms }) ->
  fn pattern -> fn body -> fn remaining_tokens -> {
    let arm = MatchArm { pattern: pattern, body: body };
    let arms_in_reverse_order = Cons(arm, reversed_arms);
    let next_state = MatchParseState {
      scrutinee: scrutinee,
      reversed_arms: arms_in_reverse_order
    };
    match remaining_tokens {
      | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), rest) ->
        Ok(ParseStep {
          value: Match(MatchExpression {
            scrutinee: scrutinee,
            arms: reverse_match_arms arms_in_reverse_order
          }),
          remaining_tokens: rest
        })
      | Cons(SymbolToken(TokenInfo { lexeme: "|", span: _ }), _) -> parse_match_arms next_state remaining_tokens
      | _ -> Err("expected `|` or `}` after a match arm")
    }
  }

let reverse_match_arm_accumulator = fn remaining -> fn reversed ->
  match remaining {
    | Nil -> reversed
    | Cons(arm, rest) -> reverse_match_arm_accumulator rest (Cons(arm, reversed))
  }

let reverse_match_arms = fn arms -> reverse_match_arm_accumulator arms Nil

let parse_main_body = fn tokens ->
  match (take_identifier "let" tokens) {
    | Err(message) -> Err(message)
    | Ok(after_let) -> parse_main_name after_let
  }

let parse_main_name = fn tokens ->
  match (take_identifier "main" tokens) {
    | Err(message) -> Err(message)
    | Ok(after_name) -> parse_main_equals after_name
  }

let parse_main_equals = fn tokens ->
  match (take_symbol "=" tokens) {
    | Err(message) -> Err(message)
    | Ok(after_equals) -> parse_main_function after_equals
  }

let parse_main_function = fn tokens ->
  match (take_identifier "fn" tokens) {
    | Err(message) -> Err(message)
    | Ok(after_fn) -> parse_main_parameter_list after_fn
  }

let parse_main_parameter_list = fn tokens ->
  match (take_symbol "(" tokens) {
    | Err(message) -> Err(message)
    | Ok(after_open_paren) -> parse_main_parameter_end after_open_paren
  }

let parse_main_parameter_end = fn tokens ->
  match (take_symbol ")" tokens) {
    | Err(message) -> Err(message)
    | Ok(after_close_paren) -> parse_main_arrow after_close_paren
  }

let parse_main_arrow = fn tokens ->
  match (take_symbol "->" tokens) {
    | Err(message) -> Err(message)
    | Ok(body_tokens) -> parse_entry_expression body_tokens
  }

let parse_entry_expression = fn tokens ->
  match (parse_expr tokens) {
    | Ok(ParseStep { value: expression, remaining_tokens: Nil }) -> Ok(Main(expression))
    | Ok(_) -> Err("unexpected tokens after the main expression")
    | Err(message) -> Err(message)
  }

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

let parse_program = fn source ->
  match (lex source) {
    | Err(error) -> Err(parser_error_from_lexer error)
    | Ok(tokens) -> match (parse_main_body tokens) {
      | Err(message) -> Err(ParserError { message: message, source_span: None })
      | Ok(program) -> Ok(program)
    }
  }
