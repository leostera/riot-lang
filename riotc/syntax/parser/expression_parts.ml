// Shared list and record expression productions used by the expression parser.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use Syntax.Ast.{
  Expr, ValueName, ConstructorName, RecordExpr, RecordExpression,
  RecordExpressionField, VariableExpr
}
use Syntax.Lexer.{Token, TokenInfo, IdentifierToken, SymbolToken}
use Syntax.Parser.Common.{ParseStep, take_symbol, reverse_list}

type RecordExpressionState = RecordExpressionState {
  constructor_name: ConstructorName,
  reversed_fields: List<RecordExpressionField>
}

let parse_expression_list_tail = fn parse_expression -> fn reversed_items -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), remaining) ->
      match (parse_expression remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: next, remaining_tokens }) ->
          parse_expression_list_tail parse_expression (Cons(next, reversed_items)) remaining_tokens
      }
    | _ -> Ok(ParseStep {
      value: reverse_list reversed_items,
      remaining_tokens: tokens
    })
  }

let parse_expression_list = fn parse_expression -> fn tokens ->
  match (parse_expression tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: first, remaining_tokens }) ->
      parse_expression_list_tail parse_expression (Cons(first, Nil)) remaining_tokens
  }

let finish_record_expression = fn (RecordExpressionState {
  constructor_name,
  reversed_fields
}) -> fn remaining ->
  Ok(ParseStep {
    value: RecordExpr(RecordExpression {
      constructor_name: constructor_name,
      fields: reverse_list reversed_fields
    }),
    remaining_tokens: remaining
  })

let parse_record_expression_fields = fn parse_expression -> fn
  (RecordExpressionState { constructor_name, reversed_fields }) -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) ->
      finish_record_expression (RecordExpressionState {
        constructor_name: constructor_name,
        reversed_fields: reversed_fields
      }) remaining
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), remaining) ->
      match (take_symbol ":" remaining) {
        | Ok(value_tokens) -> match (parse_expression value_tokens) {
          | Err(message) -> Err(message)
          | Ok(ParseStep { value, remaining_tokens }) -> {
            let next_fields = Cons(RecordExpressionField {
              name: ValueName(name),
              value: value
            }, reversed_fields);
            let next_state = RecordExpressionState {
              constructor_name: constructor_name,
              reversed_fields: next_fields
            };
            match remaining_tokens {
              | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), rest) ->
                parse_record_expression_fields parse_expression next_state rest
              | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), rest) ->
                finish_record_expression next_state rest
              | _ -> Err("expected `,` or `}` after record field")
            }
          }
        }
        | Err(_) -> {
          let next_fields = Cons(RecordExpressionField {
            name: ValueName(name),
            value: VariableExpr(ValueName(name))
          }, reversed_fields);
          let next_state = RecordExpressionState {
            constructor_name: constructor_name,
            reversed_fields: next_fields
          };
          match remaining {
            | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), rest) ->
              parse_record_expression_fields parse_expression next_state rest
            | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), rest) ->
              finish_record_expression next_state rest
            | _ -> Err("expected `,` or `}` after record field")
          }
        }
      }
    | _ -> Err("expected a record field name")
  }
