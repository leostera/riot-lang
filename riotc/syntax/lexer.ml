// Tokenization for the RiotML bootstrap subset.
use Prelude.{List, Nil, Cons, Option, None, Some}
use String.{length, get, slice}

let is_space = fn code ->
  match code {
    | 9 -> true
    | 10 -> true
    | 13 -> true
    | 32 -> true
    | _ -> false
  }

let is_digit = fn code ->
  match (code >= 48) {
    | true -> code <= 57
    | false -> false
  }

let is_lower_or_underscore = fn code ->
  match (code == 95) {
    | true -> true
    | false -> match (code >= 97) {
      | true -> code <= 122
      | false -> false
    }
  }

let is_letter = fn code ->
  match (code >= 65) {
    | true -> match (code <= 90) {
      | true -> true
      | false -> is_lower_or_underscore code
    }
    | false -> is_lower_or_underscore code
  }

let is_word_char = fn code ->
  match (is_letter code) {
    | true -> true
    | false -> is_digit code
  }

let scan_digits = fn (source, index) ->
  match (get source index) {
    | Some(code) -> match (is_digit code) {
      | true -> scan_digits (source, index + 1)
      | false -> index
    }
    | None -> index
  }

let scan_word = fn (source, index) ->
  match (get source index) {
    | Some(code) -> match (is_word_char code) {
      | true -> scan_word (source, index + 1)
      | false -> index
    }
    | None -> index
  }

let scan_string = fn (source, index) ->
  match (get source index) {
    | Some(code) -> match code {
      | 34 -> index + 1
      | 92 -> match (get source (index + 1)) {
        | Some(_) -> scan_string (source, index + 2)
        | None -> index + 1
      }
      | _ -> scan_string (source, index + 1)
    }
    | None -> index
  }

let skip_line = fn (source, index) ->
  match (get source index) {
    | Some(10) -> index + 1
    | Some(_) -> skip_line (source, index + 1)
    | None -> index
  }

let two_char_operator = fn (source, index) ->
  match (get source index) {
    | Some(45) -> match (get source (index + 1)) {
      | Some(62) -> "->"
      | _ -> ""
    }
    | Some(61) -> match (get source (index + 1)) {
      | Some(61) -> "=="
      | _ -> ""
    }
    | Some(33) -> match (get source (index + 1)) {
      | Some(61) -> "!="
      | _ -> ""
    }
    | Some(60) -> match (get source (index + 1)) {
      | Some(61) -> "<="
      | _ -> ""
    }
    | Some(62) -> match (get source (index + 1)) {
      | Some(61) -> ">="
      | _ -> ""
    }
    | _ -> ""
  }

let reverse_acc = fn (remaining, reversed) ->
  match remaining {
    | Nil -> reversed
    | Cons(token, rest) -> reverse_acc (rest, Cons(token, reversed))
  }

let reverse_tokens = fn tokens -> reverse_acc (tokens, Nil)

// Keep recursion shallow on large compiler source files; no tail-call
// optimization is assumed from the Stage 0 JavaScript host.
let scan_chunk = fn (source, index, reversed, fuel) ->
  match (fuel <= 0) {
    | true -> (index, reversed, false)
    | false -> match (get source index) {
      | None -> (index, reversed, true)
      | Some(code) -> match (is_space code) {
        | true -> scan_chunk (source, index + 1, reversed, fuel - 1)
        | false -> match (code == 34) {
          | true -> {
            let end_index = scan_string (source, index + 1);
            let token = "S:" + slice source index end_index;
            scan_chunk (source, end_index, Cons(token, reversed), fuel - 1)
          }
          | false -> match (is_digit code) {
            | true -> {
              let end_index = scan_digits (source, index);
              let token = "N:" + slice source index end_index;
              scan_chunk (source, end_index, Cons(token, reversed), fuel - 1)
            }
            | false -> match (is_letter code) {
              | true -> {
                let end_index = scan_word (source, index);
                let token = "I:" + slice source index end_index;
                scan_chunk (source, end_index, Cons(token, reversed), fuel - 1)
              }
              | false -> match (code == 47) {
                | true -> match (get source (index + 1)) {
                  | Some(47) -> scan_chunk (source, skip_line (source, index + 2), reversed, fuel - 1)
                  | _ -> {
                    let operator = two_char_operator (source, index);
                    match (length operator > 0) {
                      | true -> scan_chunk (source, index + 2, Cons(operator, reversed), fuel - 1)
                      | false -> scan_chunk (source, index + 1, Cons("/", reversed), fuel - 1)
                    }
                  }
                }
                | false -> {
                  let operator = two_char_operator (source, index);
                  match (length operator > 0) {
                    | true -> scan_chunk (source, index + 2, Cons(operator, reversed), fuel - 1)
                    | false -> scan_chunk (source, index + 1, Cons(slice source index (index + 1), reversed), fuel - 1)
                  }
                }
              }
            }
          }
        }
      }
    }
  }

let scan_all = fn (source, index, reversed) ->
  match (scan_chunk (source, index, reversed, 24)) {
    | (next_index, tokens, true) -> reverse_tokens tokens
    | (next_index, tokens, false) -> scan_all (source, next_index, tokens)
  }

let lex = fn source -> scan_all (source, 0, Nil)
