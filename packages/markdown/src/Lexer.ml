open Std

(* Simple cursor for lexing with position tracking *)
type cursor = { input : string; mutable pos : int; length : int }

let create input = { input; pos = 0; length = String.length input }

let peek cursor =
  if cursor.pos >= cursor.length then None
  else Some (String.get cursor.input cursor.pos)

let advance cursor =
  if cursor.pos < cursor.length then cursor.pos <- cursor.pos + 1

let position cursor = cursor.pos

let take_while cursor f =
  let start = cursor.pos in
  while cursor.pos < cursor.length && f (String.get cursor.input cursor.pos) do
    cursor.pos <- cursor.pos + 1
  done;
  let len = cursor.pos - start in
  String.sub cursor.input start len

(* Tokenize the input into a list of tokens with spans *)
let tokenize input =
  let cursor = create input in
  let tokens = Cell.create [] in

  let add_token kind =
    let span =
      Ceibo.Span.make ~start:(position cursor) ~end_:(position cursor + 1)
    in
    let token = Token.make kind span in
    Cell.set tokens (token :: Cell.get tokens);
    advance cursor
  in

  let add_text_token start =
    let end_pos = position cursor in
    if end_pos > start then
      let span = Ceibo.Span.make ~start ~end_:end_pos in
      let token = Token.make Syntax_kind.TEXT_TOKEN span in
      Cell.set tokens (token :: Cell.get tokens)
  in

  while position cursor < cursor.length do
    match peek cursor with
    | None -> ()
    | Some c -> (
        match c with
        | '#' -> add_token Syntax_kind.HASH
        | '*' -> add_token Syntax_kind.STAR
        | '_' -> add_token Syntax_kind.UNDERSCORE
        | '`' -> add_token Syntax_kind.BACKTICK
        | '-' -> add_token Syntax_kind.DASH
        | '+' -> add_token Syntax_kind.PLUS
        | '[' -> add_token Syntax_kind.LEFT_BRACKET
        | ']' -> add_token Syntax_kind.RIGHT_BRACKET
        | '(' -> add_token Syntax_kind.LEFT_PAREN
        | ')' -> add_token Syntax_kind.RIGHT_PAREN
        | '!' -> add_token Syntax_kind.EXCLAMATION
        | '<' -> add_token Syntax_kind.LESS_THAN
        | '>' -> add_token Syntax_kind.GREATER_THAN
        | '=' -> add_token Syntax_kind.EQUAL
        | '.' -> add_token Syntax_kind.DOT
        | ' ' -> add_token Syntax_kind.SPACE
        | '\t' -> add_token Syntax_kind.TAB
        | '\\' -> (
            (* Check for escaped character *)
            let start = position cursor in
            advance cursor;
            match peek cursor with
            | Some c when String.contains "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~" c
              ->
                advance cursor;
                let span = Ceibo.Span.make ~start ~end_:(position cursor) in
                let token = Token.make Syntax_kind.ESCAPED_CHAR span in
                Cell.set tokens (token :: Cell.get tokens)
            | _ ->
                (* Not an escape, just a backslash *)
                let span = Ceibo.Span.make ~start ~end_:(start + 1) in
                let token = Token.make Syntax_kind.BACKSLASH span in
                Cell.set tokens (token :: Cell.get tokens))
        | '&' -> add_token Syntax_kind.AMPERSAND
        | '|' -> add_token Syntax_kind.PIPE
        | '~' -> add_token Syntax_kind.TILDE
        | ':' -> add_token Syntax_kind.COLON
        | '\'' -> add_token Syntax_kind.QUOTE
        | '"' -> add_token Syntax_kind.DOUBLE_QUOTE
        | '\n' | '\r' ->
            (* Handle newlines (both \n and \r\n) *)
            let start = position cursor in
            advance cursor;
            if c = '\r' && peek cursor = Some '\n' then advance cursor;
            let span = Ceibo.Span.make ~start ~end_:(position cursor) in
            let token = Token.make Syntax_kind.NEWLINE span in
            Cell.set tokens (token :: Cell.get tokens)
        | '0' .. '9' ->
            (* Collect digit as text token *)
            let start = position cursor in
            advance cursor;
            let span = Ceibo.Span.make ~start ~end_:(position cursor) in
            let token = Token.make Syntax_kind.DIGIT span in
            Cell.set tokens (token :: Cell.get tokens)
        | _ ->
            (* Collect regular text *)
            let start = position cursor in
            let _text =
              take_while cursor (fun ch ->
                  not
                    (String.contains "#*_`-+[]()!<>=. \t\\\n\r&|~:'\"0123456789"
                       ch))
            in
            add_text_token start)
  done;

  List.rev (Cell.get tokens)
