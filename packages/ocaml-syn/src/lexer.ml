open Std

type t = Cursor.t

let create source = Cursor.create source

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let lex_whitespace cursor =
  Cursor.skip_while cursor is_whitespace;
  Token.Whitespace

let rec lex_block_comment cursor depth =
  match Cursor.peek cursor with
  | None -> Token.Comment { value = ""; terminated = false }
  | Some '(' -> (
      Cursor.advance cursor;
      match Cursor.peek cursor with
      | Some '*' ->
          Cursor.advance cursor;
          lex_block_comment cursor (depth + 1)
      | _ -> lex_block_comment cursor depth)
  | Some '*' -> (
      Cursor.advance cursor;
      match Cursor.peek cursor with
      | Some ')' ->
          Cursor.advance cursor;
          if depth = 1 then Token.Comment { value = ""; terminated = true }
          else lex_block_comment cursor (depth - 1)
      | _ -> lex_block_comment cursor depth)
  | Some _ ->
      Cursor.advance cursor;
      lex_block_comment cursor depth

let lex_comment cursor =
  Cursor.advance cursor;
  Cursor.advance cursor;
  lex_block_comment cursor 1

let lex_ident cursor =
  let start = Cursor.position cursor in
  Cursor.skip_while cursor is_ident_continue;
  let len = Cursor.position cursor - start in
  let ident = Cursor.slice cursor start len in

  if ident = "_" then Token.Underscore
  else
    match Token.keyword_of_string ident with
    | Some kw ->
        if Token.is_opening_keyword ident then
          let delim = Token.delimiter_of_keyword ident |> Option.unwrap in
          Token.OpenDelim delim
        else if Token.is_closing_keyword ident then Token.CloseDelim BeginEnd
        else Token.Keyword kw
    | None -> Token.Ident ident

let lex_number cursor =
  let num_str = Cursor.take_while cursor is_digit in
  match Cursor.peek cursor with
  | Some '.' -> (
      Cursor.advance cursor;
      let frac = Cursor.take_while cursor is_digit in
      let float_str = num_str ^ "." ^ frac in
      match float_of_string_opt float_str with
      | Some f -> Token.Literal (Float f)
      | None -> Token.Unknown '.')
  | _ -> (
      match int_of_string_opt num_str with
      | Some i -> Token.Literal (Int i)
      | None -> Token.Unknown '0')

let lex_string cursor =
  Cursor.advance cursor;
  let start = Cursor.position cursor in
  let rec loop () =
    match Cursor.peek cursor with
    | None -> (Cursor.slice cursor start (Cursor.position cursor - start), false)
    | Some '\\' ->
        Cursor.advance cursor;
        Cursor.advance cursor;
        loop ()
    | Some '"' ->
        let value = Cursor.slice cursor start (Cursor.position cursor - start) in
        Cursor.advance cursor;
        (value, true)
    | Some _ ->
        Cursor.advance cursor;
        loop ()
  in
  let value, terminated = loop () in
  Token.Literal (String { value; terminated })

let lex_char cursor =
  Cursor.advance cursor;
  match Cursor.peek cursor with
  | None -> Token.Unknown '\''
  | Some '\\' ->
      Cursor.advance cursor;
      let escaped = Cursor.peek cursor in
      Cursor.advance cursor;
      (match Cursor.peek cursor with
      | Some '\'' ->
          Cursor.advance cursor;
          Token.Literal (Char (Option.value ~default:' ' escaped))
      | _ -> Token.Unknown '\'')
  | Some c ->
      Cursor.advance cursor;
      (match Cursor.peek cursor with
      | Some '\'' ->
          Cursor.advance cursor;
          Token.Literal (Char c)
      | _ -> Token.Unknown '\'')

let next cursor =
  if Cursor.is_eof cursor then Token.EOF
  else
    match Cursor.peek cursor with
    | None -> Token.EOF
    | Some c when is_whitespace c -> lex_whitespace cursor
    | Some '(' -> (
        match Cursor.peek_n cursor 1 with
        | Some '*' -> lex_comment cursor
        | _ ->
            Cursor.advance cursor;
            Token.OpenDelim Paren)
    | Some ')' ->
        Cursor.advance cursor;
        Token.CloseDelim Paren
    | Some '[' ->
        Cursor.advance cursor;
        Token.OpenDelim Bracket
    | Some ']' ->
        Cursor.advance cursor;
        Token.CloseDelim Bracket
    | Some '{' ->
        Cursor.advance cursor;
        Token.OpenDelim Brace
    | Some '}' ->
        Cursor.advance cursor;
        Token.CloseDelim Brace
    | Some '+' ->
        Cursor.advance cursor;
        Token.Plus
    | Some '-' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '>' ->
            Cursor.advance cursor;
            Token.Arrow
        | _ -> Token.Minus)
    | Some '*' ->
        Cursor.advance cursor;
        Token.Star
    | Some '/' ->
        Cursor.advance cursor;
        Token.Slash
    | Some '%' ->
        Cursor.advance cursor;
        Token.Percent
    | Some '^' ->
        Cursor.advance cursor;
        Token.Caret
    | Some '=' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '>' ->
            Cursor.advance cursor;
            Token.FatArrow
        | _ -> Token.Eq)
    | Some '<' ->
        Cursor.advance cursor;
        Token.Lt
    | Some '>' ->
        Cursor.advance cursor;
        Token.Gt
    | Some '!' ->
        Cursor.advance cursor;
        Token.Bang
    | Some '&' ->
        Cursor.advance cursor;
        Token.Ampersand
    | Some '|' ->
        Cursor.advance cursor;
        Token.Pipe
    | Some ':' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some ':' ->
            Cursor.advance cursor;
            Token.ColonColon
        | Some '=' ->
            Cursor.advance cursor;
            Token.ColonEq
        | _ -> Token.Colon)
    | Some ';' ->
        Cursor.advance cursor;
        Token.Semi
    | Some ',' ->
        Cursor.advance cursor;
        Token.Comma
    | Some '.' ->
        Cursor.advance cursor;
        Token.Dot
    | Some '?' ->
        Cursor.advance cursor;
        Token.Question
    | Some '@' ->
        Cursor.advance cursor;
        Token.At
    | Some '#' ->
        Cursor.advance cursor;
        Token.Hash
    | Some '~' ->
        Cursor.advance cursor;
        Token.Tilde
    | Some '$' ->
        Cursor.advance cursor;
        Token.Dollar
    | Some '"' -> lex_string cursor
    | Some '\'' -> lex_char cursor
    | Some c when is_digit c -> lex_number cursor
    | Some c when is_ident_start c -> lex_ident cursor
    | Some c ->
        Cursor.advance cursor;
        Token.Unknown c

let rec lex_all cursor acc =
  match next cursor with
  | Token.EOF -> List.rev (Token.EOF :: acc)
  | token -> lex_all cursor (token :: acc)

let tokenize source =
  let cursor = create source in
  lex_all cursor []
