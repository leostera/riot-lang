// Pattern-list parsing shared by constructor arguments and tuple patterns.
use Prelude.{List, Nil, Cons, Result, Ok, Err}
use Syntax.Ast.{Pattern}
use Syntax.Lexer.{Token, TokenInfo, SymbolToken}
use Syntax.Parser.Common.{ParseStep, reverse_list}

let parse_pattern_list_tail = fn parse_pattern -> fn (reversed_patterns: List<Pattern>) -> fn (tokens: List<Token>) ->
  (match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), remaining) ->
      match (parse_pattern remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: next, remaining_tokens }) ->
          parse_pattern_list_tail parse_pattern (Cons(next, reversed_patterns)) remaining_tokens
      }
    | _ -> Ok(ParseStep {
      value: reverse_list reversed_patterns,
      remaining_tokens: tokens
    })
  }: Result<ParseStep<List<Pattern>>, string>)

let parse_pattern_list = fn parse_pattern -> fn (tokens: List<Token>) ->
  match (parse_pattern tokens) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: first, remaining_tokens }) ->
      parse_pattern_list_tail parse_pattern (Cons(first, Nil)) remaining_tokens
  }
