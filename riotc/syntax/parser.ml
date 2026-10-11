// Parser for the initial native RiotML entry-point language.
use Prelude.{List, Nil, Cons, Option, None, Some, Result, Ok, Err}
use Syntax.Ast.{Expr, Program, Integer, Boolean, Add, Subtract, Multiply, Divide, Negate, Main}
use Syntax.Lexer.{lex}
use String.{length, get, slice, equal}

let token_text = fn token -> slice token 2 (length token)

let parse_decimal = fn (digits, index, value) ->
  match (get digits index) {
    | Some(code) -> parse_decimal (digits, index + 1, value * 10 + code - 48)
    | None -> value
  }

let valid_i32_literal = fn digits ->
  match (length digits <= 9) {
    | true -> true
    | false -> match (length digits == 10) {
      | false -> false
      | true -> {
        let prefix = parse_decimal (slice digits 0 9, 0, 0);
        match (prefix < 214748364) {
          | true -> true
          | false -> match (prefix == 214748364) {
            | true -> match (get digits 9) {
              | Some(last) -> last <= 55
              | None -> false
            }
            | false -> false
          }
        }
      }
    }
  }

let parse_primary = fn tokens ->
  match tokens {
    | Cons("-", Cons(token, rest)) -> match (equal (slice token 0 2) "N:") {
      | true -> match (equal (token_text token) "2147483648") {
        | true -> Ok((Integer("-2147483648"), rest))
        | false -> match (valid_i32_literal (token_text token)) {
          | true -> Ok((Negate(Integer(token_text token)), rest))
          | false -> Err("integer literal is outside i32 range")
        }
      }
      | false -> match (parse_primary (Cons(token, rest))) {
        | Ok((expression, remaining)) -> Ok((Negate(expression), remaining))
        | Err(message) -> Err(message)
      }
    }
    | Cons(token, rest) -> match (equal (slice token 0 2) "N:") {
      | true -> match (valid_i32_literal (token_text token)) {
        | true -> Ok((Integer(token_text token), rest))
        | false -> Err("integer literal is outside i32 range")
      }
      | false -> match token {
        | "I:true" -> Ok((Boolean(true), rest))
        | "I:false" -> Ok((Boolean(false), rest))
        | "(" -> match (parse_expr rest) {
          | Ok((expression, Cons(")", remaining))) -> Ok((expression, remaining))
          | _ -> Err("expected `)` after expression")
        }
        | _ -> Err("expected an expression")
      }
    }
    | Nil -> Err("expected an expression before end of input")
  }

let parse_term_tail = fn (left, tokens) ->
  match tokens {
    | Cons("*", rest) -> match (parse_primary rest) {
      | Ok((right, remaining)) -> parse_term_tail (Multiply(left, right), remaining)
      | Err(message) -> Err(message)
    }
    | Cons("/", rest) -> match (parse_primary rest) {
      | Ok((right, remaining)) -> parse_term_tail (Divide(left, right), remaining)
      | Err(message) -> Err(message)
    }
    | _ -> Ok((left, tokens))
  }

let parse_term = fn tokens ->
  match (parse_primary tokens) {
    | Ok((expression, rest)) -> parse_term_tail (expression, rest)
    | Err(message) -> Err(message)
  }

let parse_sum_tail = fn (left, tokens) ->
  match tokens {
    | Cons("+", rest) -> match (parse_term rest) {
      | Ok((right, remaining)) -> parse_sum_tail (Add(left, right), remaining)
      | Err(message) -> Err(message)
    }
    | Cons("-", rest) -> match (parse_term rest) {
      | Ok((right, remaining)) -> parse_sum_tail (Subtract(left, right), remaining)
      | Err(message) -> Err(message)
    }
    | _ -> Ok((left, tokens))
  }

let parse_expr = fn tokens ->
  match (parse_term tokens) {
    | Ok((expression, rest)) -> parse_sum_tail (expression, rest)
    | Err(message) -> Err(message)
  }

let parse_program = fn source ->
  match (lex source) {
    | Cons("I:let", Cons("I:main", Cons("=", Cons("I:fn", Cons("(", Cons(")", Cons("->", rest))))))) ->
      match (parse_expr rest) {
        | Ok((expression, Nil)) -> Ok(Main(expression))
        | Ok(_) -> Err("unexpected tokens after the main expression")
        | Err(message) -> Err(message)
      }
    | _ -> Err("expected `let main = fn () -> <expression>`")
  }
