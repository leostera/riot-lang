// Expression and pattern parsing with an explicit precedence ladder.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Syntax.Ast.{
  Expr, Pattern, ValueName, ConstructorName, IntegerLiteral, TextLiteral,
  make_expression, expression_with_span, expression_span,
  BinaryOperator, Addition, Subtraction, Multiplication, Division, Remainder,
  Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual,
  Integer, Boolean, StringLiteral, Unit, VariableExpr, TupleExpr,
  Binary, BinaryExpression, Negate, Function, FunctionExpression, Apply,
  ApplicationExpression, Let, LetExpression, Match, MatchExpression,
  MatchArm, ConstructorExpr, ConstructorExpression, Ascription,
  AscriptionExpression, Block
}
use Syntax.Lexer.{
  Token, TokenInfo, IdentifierToken, IntegerToken, TextToken, SymbolToken,
  SourceSpan
}
use Syntax.Parser.Common.{
  ParseStep, valid_i32_literal, take_symbol, reverse_list, is_constructor_name
}
use Syntax.Parser.Types.{parse_type}
use Syntax.Parser.Patterns.{parse_pattern}
use Syntax.Parser.Literals.{decode_string_literal}
use Syntax.Parser.ExpressionParts.{
  RecordExpressionState, parse_expression_list, parse_expression_list_tail,
  parse_record_expression_fields
}
use String.{equal}

let addition_operator = Addition
let subtraction_operator = Subtraction
let multiplication_operator = Multiplication
let division_operator = Division
let remainder_operator = Remainder

type BinaryParseRequest = BinaryParseRequest {
  left_expression: Expr,
  operator: BinaryOperator,
  right_tokens: List<Token>
}

type ExpressionState = ExpressionState {
  expression: Expr,
  remaining_tokens: List<Token>
}

type MatchParseState = MatchParseState {
  scrutinee: Expr,
  reversed_arms: List<MatchArm>
}

let token_span = fn token ->
  match token {
    | IdentifierToken(TokenInfo { lexeme: _, span }) -> span
    | IntegerToken(TokenInfo { lexeme: _, span }) -> span
    | TextToken(TokenInfo { lexeme: _, span }) -> span
    | SymbolToken(TokenInfo { lexeme: _, span }) -> span
  }

let same_token_position = fn first -> fn second -> {
  let SourceSpan { start_codepoint: first_start, end_codepoint: first_end } = token_span first;
  let SourceSpan { start_codepoint: second_start, end_codepoint: second_end } = token_span second;
  match (first_start == second_start) {
    | false -> false
    | true -> first_end == second_end
  }
}

let token_before_suffix = fn suffix_token -> fn tokens -> fn previous_token ->
  match tokens {
    | Nil -> previous_token
    | Cons(token, remaining) -> match (same_token_position token suffix_token) {
      | true -> previous_token
      | false -> token_before_suffix suffix_token remaining (Some(token))
    }
  }

let last_token = fn tokens ->
  match tokens {
    | Nil -> None
    | Cons(token, Nil) -> Some(token)
    | Cons(_, remaining) -> last_token remaining
  }

let span_from_first_and_last = fn first_token -> fn last_token -> {
  let SourceSpan { start_codepoint, end_codepoint: _ } = token_span first_token;
  let SourceSpan { start_codepoint: _, end_codepoint } = token_span last_token;
  SourceSpan { start_codepoint: start_codepoint, end_codepoint: end_codepoint }
}

let span_of_consumed_tokens = fn tokens -> fn remaining_tokens ->
  match tokens {
    | Nil -> SourceSpan { start_codepoint: 0, end_codepoint: 0 }
    | Cons(first_token, _) -> match remaining_tokens {
      | Nil -> match (last_token tokens) {
        | None -> SourceSpan { start_codepoint: 0, end_codepoint: 0 }
        | Some(last) -> span_from_first_and_last first_token last
      }
      | Cons(first_remaining, _) -> match (token_before_suffix
          first_remaining tokens None) {
        | None -> SourceSpan { start_codepoint: 0, end_codepoint: 0 }
        | Some(last) -> span_from_first_and_last first_token last
      }
    }
  }

let parse_function_expression = fn parse_nested_expression -> fn tokens ->
  match (parse_pattern tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: parameter, remaining_tokens }) ->
      parse_function_arrow parse_nested_expression parameter remaining_tokens
  }

let parse_function_arrow = fn parse_nested_expression -> fn parameter -> fn tokens ->
  match (take_symbol "->" tokens) {
    | Err(message) -> Err(message)
    | Ok(body_tokens) -> match (parse_nested_expression body_tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: body, remaining_tokens }) -> Ok(ParseStep {
        value: make_expression (Function(FunctionExpression {
          parameter: parameter,
          body: body
        })),
        remaining_tokens: remaining_tokens
      })
    }
  }

let parse_let_expression = fn parse_nested_expression -> fn tokens ->
  match (parse_pattern tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: pattern, remaining_tokens }) ->
      parse_let_annotation parse_nested_expression pattern remaining_tokens
  }

let parse_let_annotation = fn parse_nested_expression -> fn pattern -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ":", span: _ }), rest) ->
      match (parse_type rest) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: annotation, remaining_tokens }) ->
          parse_let_equals parse_nested_expression pattern (Some(annotation)) remaining_tokens
      }
    | _ -> parse_let_equals parse_nested_expression pattern None tokens
  }

let parse_let_equals = fn parse_nested_expression -> fn pattern -> fn annotation -> fn tokens ->
  match (take_symbol "=" tokens) {
    | Err(message) -> Err(message)
    | Ok(value_tokens) -> match (parse_nested_expression value_tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value, remaining_tokens }) ->
        parse_let_body parse_nested_expression pattern annotation value remaining_tokens
    }
  }

let parse_let_body = fn parse_nested_expression -> fn pattern -> fn annotation -> fn value -> fn tokens ->
  match (take_symbol ";" tokens) {
    | Err(message) -> Err(message)
    | Ok(body_tokens) -> match (parse_nested_expression body_tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: body, remaining_tokens }) -> Ok(ParseStep {
        value: make_expression (Let(LetExpression {
          pattern: pattern,
          annotation: annotation,
          value: value,
          body: body
        })),
        remaining_tokens: remaining_tokens
      })
    }
  }

let parse_block_expression = fn parse_nested_expression -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) ->
      Ok(ParseStep { value: make_expression (Block(Nil)), remaining_tokens: remaining })
    | _ -> parse_block_items parse_nested_expression tokens Nil
  }

let parse_block_items = fn parse_nested_expression -> fn tokens -> fn reversed_items ->
  match (parse_nested_expression tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: expression, remaining_tokens }) ->
      parse_block_item_end parse_nested_expression expression reversed_items remaining_tokens
  }

let parse_block_item_end = fn parse_nested_expression -> fn expression -> fn reversed_items -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ";", span: _ }), remaining) ->
      parse_block_items parse_nested_expression remaining (Cons(expression, reversed_items))
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) -> Ok(ParseStep {
      value: make_expression (Block(reverse_list (Cons(expression, reversed_items)))),
      remaining_tokens: remaining
    })
    | _ -> Err("expected `;` or `}` after a block expression")
  }

let parse_match_expression = fn parse_nested_expression -> fn tokens ->
  match (parse_nested_expression tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: scrutinee, remaining_tokens }) ->
      parse_match_opening parse_nested_expression scrutinee remaining_tokens
  }

let parse_match_opening = fn parse_nested_expression -> fn scrutinee -> fn tokens ->
  match (take_symbol "{" tokens) {
    | Err(message) -> Err(message)
    | Ok(arm_tokens) -> parse_match_arms parse_nested_expression (MatchParseState {
      scrutinee: scrutinee,
      reversed_arms: Nil
    }) arm_tokens
  }

let parse_match_arms = fn parse_nested_expression -> fn state -> fn tokens ->
  match (take_symbol "|" tokens) {
    | Err(message) -> Err(message)
    | Ok(pattern_tokens) -> match (parse_pattern pattern_tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: pattern, remaining_tokens }) ->
        parse_match_arm_arrow parse_nested_expression state pattern remaining_tokens
    }
  }

let parse_match_arm_arrow = fn parse_nested_expression -> fn state -> fn pattern -> fn tokens ->
  match (take_symbol "->" tokens) {
    | Err(message) -> Err(message)
    | Ok(body_tokens) -> match (parse_nested_expression body_tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: body, remaining_tokens }) ->
        finish_match_arm parse_nested_expression state pattern body remaining_tokens
    }
  }

let finish_match_arm = fn parse_nested_expression -> fn (MatchParseState { scrutinee, reversed_arms }) ->
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
          value: make_expression (Match(MatchExpression {
            scrutinee: scrutinee,
            arms: reverse_list arms_in_reverse_order
          })),
          remaining_tokens: rest
        })
      | Cons(SymbolToken(TokenInfo { lexeme: "|", span: _ }), _) ->
        parse_match_arms parse_nested_expression next_state remaining_tokens
      | _ -> Err("expected `|` or `}` after a match arm")
    }
  }

let parse_comparison = fn parse_nested_expression -> fn tokens ->
  match (parse_sum parse_nested_expression tokens) {
    | Err(message) -> Err(message)
    | Ok(parsed) -> parse_comparison_tail parse_nested_expression parsed
  }

let parse_comparison_tail = fn parse_nested_expression -> fn (ParseStep { value: left_expression, remaining_tokens }) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "==", span: _ }), rest) ->
      parse_comparison_operator parse_nested_expression left_expression Equal rest
    | Cons(SymbolToken(TokenInfo { lexeme: "!=", span: _ }), rest) ->
      parse_comparison_operator parse_nested_expression left_expression NotEqual rest
    | Cons(SymbolToken(TokenInfo { lexeme: "<", span: _ }), rest) ->
      parse_comparison_operator parse_nested_expression left_expression LessThan rest
    | Cons(SymbolToken(TokenInfo { lexeme: "<=", span: _ }), rest) ->
      parse_comparison_operator parse_nested_expression left_expression LessThanOrEqual rest
    | Cons(SymbolToken(TokenInfo { lexeme: ">", span: _ }), rest) ->
      parse_comparison_operator parse_nested_expression left_expression GreaterThan rest
    | Cons(SymbolToken(TokenInfo { lexeme: ">=", span: _ }), rest) ->
      parse_comparison_operator parse_nested_expression left_expression GreaterThanOrEqual rest
    | _ -> Ok(ParseStep { value: left_expression, remaining_tokens: remaining_tokens })
  }

let parse_comparison_operator = fn parse_nested_expression -> fn left_expression -> fn operator -> fn right_tokens ->
  match (parse_sum parse_nested_expression right_tokens) {
    | Ok(ParseStep { value: right_expression, remaining_tokens }) ->
      parse_comparison_tail parse_nested_expression (ParseStep {
        value: make_binary_expression operator left_expression right_expression,
        remaining_tokens: remaining_tokens
      })
    | Err(message) -> Err(message)
  }

let parse_sum = fn parse_nested_expression -> fn tokens ->
  match (parse_term parse_nested_expression tokens) {
    | Err(message) -> Err(message)
    | Ok(parsed) -> parse_sum_tail parse_nested_expression parsed
  }

let parse_sum_tail = fn parse_nested_expression -> fn (ParseStep { value: left_expression, remaining_tokens }) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "+", span: _ }), rest) ->
      match (parse_term parse_nested_expression rest) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: right_expression, remaining_tokens }) ->
          parse_sum_tail parse_nested_expression (ParseStep {
            value: make_binary_expression addition_operator left_expression right_expression,
            remaining_tokens: remaining_tokens
          })
      }
    | Cons(SymbolToken(TokenInfo { lexeme: "-", span: _ }), rest) ->
      match (parse_term parse_nested_expression rest) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: right_expression, remaining_tokens }) ->
          parse_sum_tail parse_nested_expression (ParseStep {
            value: make_binary_expression subtraction_operator left_expression right_expression,
            remaining_tokens: remaining_tokens
          })
      }
    | _ -> Ok(ParseStep { value: left_expression, remaining_tokens: remaining_tokens })
  }

let parse_term = fn parse_nested_expression -> fn tokens ->
  match (parse_application parse_nested_expression tokens) {
    | Err(message) -> Err(message)
    | Ok(parsed) -> parse_term_tail parse_nested_expression parsed
  }

let parse_term_tail = fn parse_nested_expression -> fn (ParseStep { value: left_expression, remaining_tokens }) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "*", span: _ }), rest) ->
      match (parse_application parse_nested_expression rest) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: right_expression, remaining_tokens }) ->
          parse_term_tail parse_nested_expression (ParseStep {
            value: make_binary_expression multiplication_operator left_expression right_expression,
            remaining_tokens: remaining_tokens
          })
      }
    | Cons(SymbolToken(TokenInfo { lexeme: "/", span: _ }), rest) ->
      match (parse_application parse_nested_expression rest) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: right_expression, remaining_tokens }) ->
          parse_term_tail parse_nested_expression (ParseStep {
            value: make_binary_expression division_operator left_expression right_expression,
            remaining_tokens: remaining_tokens
          })
      }
    | Cons(SymbolToken(TokenInfo { lexeme: "%", span: _ }), rest) ->
      match (parse_application parse_nested_expression rest) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: right_expression, remaining_tokens }) ->
          parse_term_tail parse_nested_expression (ParseStep {
            value: make_binary_expression remainder_operator left_expression right_expression,
            remaining_tokens: remaining_tokens
          })
      }
    | _ -> Ok(ParseStep { value: left_expression, remaining_tokens: remaining_tokens })
  }

let make_binary_expression = fn (operator: BinaryOperator) -> fn (left: Expr) -> fn (right: Expr) ->
  expression_with_span
    (make_expression (Binary(BinaryExpression { operator: operator, left: left, right: right })))
    (expression_span_from_children left right)

let parse_application = fn parse_nested_expression -> fn tokens ->
  (match (parse_prefix parse_nested_expression tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: expression, remaining_tokens }) ->
      parse_application_tail parse_nested_expression expression remaining_tokens
  }: Result<ParseStep<Expr>, string>)

let expression_span_from_children = fn left -> fn right -> {
  let SourceSpan { start_codepoint, end_codepoint: _ } = expression_span left;
  let SourceSpan { start_codepoint: _, end_codepoint } = expression_span right;
  SourceSpan { start_codepoint: start_codepoint, end_codepoint: end_codepoint }
}

let parse_application_tail = fn parse_nested_expression -> fn expression -> fn tokens ->
  match (starts_expression tokens) {
    | false -> Ok(ParseStep { value: expression, remaining_tokens: tokens })
    | true -> match (parse_prefix parse_nested_expression tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: argument, remaining_tokens }) -> {
        let applied = expression_with_span (make_expression (Apply(ApplicationExpression {
          callee: expression,
          argument: argument
        }))) (expression_span_from_children expression argument);
        parse_application_tail parse_nested_expression applied remaining_tokens
      }
    }
  }

let starts_expression = fn tokens ->
  match tokens {
    | Cons(IntegerToken(_), _) -> true
    | Cons(TextToken(_), _) -> true
    | Cons(IdentifierToken(TokenInfo { lexeme: "true", span: _ }), _) -> true
    | Cons(IdentifierToken(TokenInfo { lexeme: "false", span: _ }), _) -> true
    | Cons(IdentifierToken(TokenInfo { lexeme: "fn", span: _ }), _) -> false
    | Cons(IdentifierToken(TokenInfo { lexeme: "let", span: _ }), _) -> false
    | Cons(IdentifierToken(TokenInfo { lexeme: "match", span: _ }), _) -> false
    | Cons(IdentifierToken(TokenInfo { lexeme: "use", span: _ }), _) -> false
    | Cons(IdentifierToken(TokenInfo { lexeme: "type", span: _ }), _) -> false
    | Cons(IdentifierToken(_), _) -> true
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), _) -> true
    | Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), _) -> false
    | _ -> false
  }

let parse_prefix_node = fn parse_nested_expression -> fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: "fn", span: _ }), remaining) ->
      parse_function_expression parse_nested_expression remaining
    | Cons(IdentifierToken(TokenInfo { lexeme: "let", span: _ }), remaining) ->
      parse_let_expression parse_nested_expression remaining
    | Cons(IdentifierToken(TokenInfo { lexeme: "match", span: _ }), remaining) ->
      parse_match_expression parse_nested_expression remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), remaining) ->
      parse_block_expression parse_nested_expression remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "-", span: _ }), Cons(IntegerToken(TokenInfo { lexeme: digits, span: literal_span }), remaining)) ->
      parse_negative_integer digits literal_span remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "-", span: _ }), remaining) ->
      match (parse_primary parse_nested_expression remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: operand, remaining_tokens }) -> Ok(ParseStep {
          value: make_expression (Negate(operand)),
          remaining_tokens: remaining_tokens
        })
      }
    | _ -> parse_primary parse_nested_expression tokens
  }

let parse_prefix = fn parse_nested_expression -> fn tokens ->
  match (parse_prefix_node parse_nested_expression tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value, remaining_tokens }) -> Ok(ParseStep {
      value: expression_with_span value (span_of_consumed_tokens tokens remaining_tokens),
      remaining_tokens: remaining_tokens
    })
  }

let parse_primary = fn parse_nested_expression -> fn tokens ->
  match tokens {
    | Cons(IntegerToken(TokenInfo { lexeme: digits, span: _ }), remaining) ->
      parse_integer digits remaining
    | Cons(TextToken(TokenInfo { lexeme, span: _ }), remaining) ->
      Ok(ParseStep {
        value: make_expression (StringLiteral(TextLiteral(decode_string_literal lexeme))),
        remaining_tokens: remaining
      })
    | Cons(IdentifierToken(TokenInfo { lexeme: "true", span: _ }), remaining) ->
      Ok(ParseStep { value: make_expression (Boolean(true)), remaining_tokens: remaining })
    | Cons(IdentifierToken(TokenInfo { lexeme: "false", span: _ }), remaining) ->
      Ok(ParseStep { value: make_expression (Boolean(false)), remaining_tokens: remaining })
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), remaining) ->
      parse_identifier_expression parse_nested_expression name remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), remaining) ->
      parse_parenthesized_expression parse_nested_expression remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), remaining) ->
      parse_block_expression parse_nested_expression remaining
    | _ -> Err("expected an expression")
  }

let parse_integer = fn digits -> fn remaining ->
  match (valid_i32_literal digits) {
    | true -> Ok(ParseStep {
      value: make_expression (Integer(IntegerLiteral(digits))),
      remaining_tokens: remaining
    })
    | false -> Err("integer literal is outside i32 range")
  }

let parse_negative_integer = fn digits -> fn literal_span -> fn remaining ->
  match (equal digits "2147483648") {
    | true -> Ok(ParseStep {
      value: make_expression (Integer(IntegerLiteral("-2147483648"))),
      remaining_tokens: remaining
    })
    | false -> match (valid_i32_literal digits) {
      | true -> Ok(ParseStep {
        value: make_expression (Negate(expression_with_span
          (make_expression (Integer(IntegerLiteral(digits)))) literal_span)),
        remaining_tokens: remaining
      })
      | false -> Err("integer literal is outside i32 range")
    }
  }

let parse_identifier_expression = fn parse_nested_expression -> fn name -> fn tokens ->
  match (is_constructor_name name) {
    | true -> parse_constructor_expression parse_nested_expression name tokens
    | false -> Ok(ParseStep {
      value: make_expression (VariableExpr(ValueName(name))),
      remaining_tokens: tokens
    })
  }

let parse_constructor_expression = fn parse_nested_expression -> fn name -> fn tokens -> {
  let parse_expression = fn nested_tokens ->
    (parse_nested_expression nested_tokens: Result<ParseStep<Expr>, string>);
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), remaining) ->
      parse_record_expression_fields parse_expression (RecordExpressionState {
        constructor_name: ConstructorName(name),
        reversed_fields: Nil
      }) remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), remaining) ->
      parse_constructor_arguments parse_expression name remaining
    | _ -> Ok(ParseStep {
      value: make_expression (ConstructorExpr(ConstructorExpression {
        constructor_name: ConstructorName(name),
        arguments: Nil
      })),
      remaining_tokens: tokens
    })
  }
}

let parse_constructor_arguments = fn parse_nested_expression -> fn name -> fn tokens -> {
  let parse_expression = fn nested_tokens ->
    (parse_nested_expression nested_tokens: Result<ParseStep<Expr>, string>);
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining) ->
      Ok(ParseStep {
        value: make_expression (ConstructorExpr(ConstructorExpression {
          constructor_name: ConstructorName(name),
          arguments: Nil
        })),
        remaining_tokens: remaining
      })
    | _ -> match (parse_expression_list parse_expression tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep {
          value: arguments,
          remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining)
        }) -> Ok(ParseStep {
        value: make_expression (ConstructorExpr(ConstructorExpression {
          constructor_name: ConstructorName(name),
          arguments: arguments
        })),
        remaining_tokens: remaining
      })
      | _ -> Err("expected `)` after constructor arguments")
    }
  }
}

let parse_parenthesized_expression = fn parse_nested_expression -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining) ->
      Ok(ParseStep { value: make_expression Unit, remaining_tokens: remaining })
    | _ -> match (parse_nested_expression tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: first, remaining_tokens }) ->
        parse_parenthesized_tail parse_nested_expression first remaining_tokens
    }
  }

let parse_parenthesized_tail = fn parse_nested_expression -> fn first -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ":", span: _ }), remaining) ->
      match (parse_type remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: annotation, remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), rest) }) ->
          Ok(ParseStep {
            value: make_expression (Ascription(AscriptionExpression {
              expression: first,
              annotation: annotation
            })),
            remaining_tokens: rest
          })
        | _ -> Err("expected `)` after expression annotation")
      }
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), _) ->
      match (parse_expression_list_tail parse_nested_expression (Cons(first, Nil)) tokens) {
        | Err(message) -> Err(message)
        | Ok(ParseStep {
            value: items,
            remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), rest)
          }) -> Ok(ParseStep {
          value: make_expression (TupleExpr(items)),
          remaining_tokens: rest
        })
        | _ -> Err("expected `)` after tuple expression")
      }
    | Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining) ->
      Ok(ParseStep { value: first, remaining_tokens: remaining })
    | _ -> Err("expected `,`, `:`, or `)` after parenthesized expression")
  }

let parse_expression = fn (tokens: List<Token>) ->
  (parse_comparison parse_expression tokens: Result<ParseStep<Expr>, string>)
