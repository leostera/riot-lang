// Type-expression parsing for RiotML source modules.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use Syntax.Ast.{
  TypeExpr, TypeName, TypeVariableName, NamedType, NamedTypeExpression,
  ArrowType, ArrowTypeExpression, TupleType, TypeVariable
}
use Syntax.Lexer.{Token, TokenInfo, IdentifierToken, SymbolToken}
use Syntax.Parser.Common.{ParseStep, reverse_list}

type TypeListState = TypeListState {
  reversed_types: List<TypeExpr>,
  remaining_tokens: List<Token>
}

let parse_type_list_tail = fn parse_nested_type -> fn (TypeListState {
  reversed_types,
  remaining_tokens
}) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), rest) ->
      match (parse_nested_type rest) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: next_type, remaining_tokens }) ->
          parse_type_list_tail parse_nested_type (TypeListState {
            reversed_types: Cons(next_type, reversed_types),
            remaining_tokens: remaining_tokens
          })
      }
    | _ -> Ok(ParseStep {
      value: reverse_list reversed_types,
      remaining_tokens: remaining_tokens
    })
  }

let parse_type_list = fn parse_nested_type -> fn tokens ->
  match (parse_nested_type tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: first_type, remaining_tokens }) ->
      parse_type_list_tail parse_nested_type (TypeListState {
        reversed_types: Cons(first_type, Nil),
        remaining_tokens: remaining_tokens
      })
  }

let parse_parenthesized_type = fn parse_nested_type -> fn tokens ->
  match (parse_type_list parse_nested_type tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep {
        value: Cons(single_type, Nil),
        remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining)
      }) -> Ok(ParseStep { value: single_type, remaining_tokens: remaining })
    | Ok(ParseStep {
        value: types,
        remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), remaining)
      }) -> Ok(ParseStep { value: TupleType(types), remaining_tokens: remaining })
    | _ -> Err("expected `)` after tuple type")
  }

let parse_named_type_arguments = fn parse_nested_type -> fn name -> fn tokens ->
  match (parse_type_list parse_nested_type tokens) {
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

let parse_type_atom = fn parse_nested_type -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "'", span: _ }), Cons(IdentifierToken(TokenInfo { lexeme: variable, span: _ }), remaining)) ->
      Ok(ParseStep {
        value: TypeVariable(TypeVariableName(variable)),
        remaining_tokens: remaining
      })
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), rest) ->
      parse_parenthesized_type parse_nested_type rest
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), Cons(SymbolToken(TokenInfo { lexeme: "<", span: _ }), rest)) ->
      parse_named_type_arguments parse_nested_type name rest
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), rest) -> Ok(ParseStep {
      value: NamedType(NamedTypeExpression {
        name: TypeName(name),
        arguments: Nil
      }),
      remaining_tokens: rest
    })
    | _ -> Err("expected a type")
  }

let parse_arrow_type = fn parse_nested_type -> fn tokens ->
  match (parse_type_atom parse_nested_type tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep {
        value: left_type,
        remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: "->", span: _ }), rest)
      }) -> match (parse_nested_type rest) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: right_type, remaining_tokens }) -> Ok(ParseStep {
        value: ArrowType(ArrowTypeExpression {
          argument: left_type,
          result: right_type
        }),
        remaining_tokens: remaining_tokens
      })
    }
    | Ok(type_step) -> Ok(type_step)
  }

let parse_type = fn (tokens: List<Token>) ->
  (parse_arrow_type parse_type tokens: Result<ParseStep<TypeExpr>, string>)
