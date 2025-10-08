open Std

type t = Cursor.t

let create source = Cursor.create source
let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let lex_whitespace cursor start =
  Cursor.skip_while cursor is_whitespace;
  let end_ = Cursor.position cursor in
  { Token.kind = Token.Whitespace; span = Ceibo.Span.make ~start ~end_ }

let rec lex_block_comment cursor depth content_start token_start =
  match Cursor.peek cursor with
  | None ->
      let value =
        Cursor.slice cursor content_start
          (Cursor.position cursor - content_start)
      in
      let end_ = Cursor.position cursor in
      {
        Token.kind = Token.Comment { value; terminated = false };
        span = { start = token_start; end_ };
      }
  | Some '(' -> (
      Cursor.advance cursor;
      match Cursor.peek cursor with
      | Some '*' ->
          Cursor.advance cursor;
          lex_block_comment cursor (depth + 1) content_start token_start
      | _ -> lex_block_comment cursor depth content_start token_start)
  | Some '*' -> (
      Cursor.advance cursor;
      match Cursor.peek cursor with
      | Some ')' ->
          Cursor.advance cursor;
          if depth = 1 then
            let value =
              Cursor.slice cursor content_start
                (Cursor.position cursor - content_start - 2)
            in
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.Comment { value; terminated = true };
              span = { start = token_start; end_ };
            }
          else lex_block_comment cursor (depth - 1) content_start token_start
      | _ -> lex_block_comment cursor depth content_start token_start)
  | Some _ ->
      Cursor.advance cursor;
      lex_block_comment cursor depth content_start token_start

let lex_comment cursor token_start =
  Cursor.advance cursor;
  (* skip '(' *)
  Cursor.advance cursor;
  (* skip '*' *)
  (* Check if it's a docstring *)
  let is_docstring =
    match Cursor.peek cursor with
    | Some '*'
      when match Cursor.peek_n cursor 1 with Some ')' -> false | _ -> true ->
        Cursor.advance cursor;
        (* skip the second '*' for docstrings *)
        true
    | _ -> false
  in
  let content_start = Cursor.position cursor in

  let rec lex_content depth =
    match Cursor.peek cursor with
    | None ->
        let value =
          Cursor.slice cursor content_start
            (Cursor.position cursor - content_start)
        in
        let end_ = Cursor.position cursor in
        if is_docstring then
          {
            Token.kind = Token.Docstring { value; terminated = false };
            span = { start = token_start; end_ };
          }
        else
          {
            Token.kind = Token.Comment { value; terminated = false };
            span = { start = token_start; end_ };
          }
    | Some '(' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '*' ->
            Cursor.advance cursor;
            lex_content (depth + 1)
        | _ -> lex_content depth)
    | Some '*' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some ')' ->
            Cursor.advance cursor;
            if depth = 1 then
              let value =
                Cursor.slice cursor content_start
                  (Cursor.position cursor - content_start - 2)
              in
              let end_ = Cursor.position cursor in
              if is_docstring then
                {
                  Token.kind = Token.Docstring { value; terminated = true };
                  span = { start = token_start; end_ };
                }
              else
                {
                  Token.kind = Token.Comment { value; terminated = true };
                  span = { start = token_start; end_ };
                }
            else lex_content (depth - 1)
        | _ -> lex_content depth)
    | Some _ ->
        Cursor.advance cursor;
        lex_content depth
  in
  lex_content 1

let lex_ident cursor delim_stack token_start =
  let start = Cursor.position cursor in
  Cursor.skip_while cursor is_ident_continue;
  let len = Cursor.position cursor - start in
  let ident = Cursor.slice cursor start len in
  let end_ = Cursor.position cursor in

  let kind =
    if ident = "_" then Token.Underscore
    else
      match Keyword.of_string ident with
      | Some kw ->
          if Keyword.is_opening kw then
            let delim = Token.delimiter_of_keyword kw |> Option.unwrap in
            Token.OpenDelim delim
          else if Keyword.is_closing kw then
            (* Match 'end' to the correct closing delimiter based on stack *)
            match delim_stack with
            | d :: _ -> Token.CloseDelim d
            | [] -> Token.CloseDelim BeginEnd (* Default fallback *)
          else Token.Keyword kw
      | None -> Token.Ident ident
  in
  { Token.kind; span = { start = token_start; end_ } }

let lex_number cursor token_start =
  let num_str = Cursor.take_while cursor is_digit in
  let end_ = Cursor.position cursor in
  let kind =
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
  in
  {
    Token.kind;
    span = Ceibo.Span.make ~start:token_start ~end_:(Cursor.position cursor);
  }

let lex_string cursor token_start =
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
        let value =
          Cursor.slice cursor start (Cursor.position cursor - start)
        in
        Cursor.advance cursor;
        (value, true)
    | Some _ ->
        Cursor.advance cursor;
        loop ()
  in
  let value, terminated = loop () in
  let end_ = Cursor.position cursor in
  {
    Token.kind = Token.Literal (String { value; terminated });
    span = { start = token_start; end_ };
  }

let lex_char cursor token_start =
  Cursor.advance cursor;
  let kind =
    match Cursor.peek cursor with
    | None -> Token.Unknown '\''
    | Some '\\' -> (
        Cursor.advance cursor;
        let escaped = Cursor.peek cursor in
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '\'' ->
            Cursor.advance cursor;
            Token.Literal (Char (Option.unwrap_or ~default:' ' escaped))
        | _ -> Token.Unknown '\'')
    | Some c -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '\'' ->
            Cursor.advance cursor;
            Token.Literal (Char c)
        | _ -> Token.Unknown '\'')
  in
  let end_ = Cursor.position cursor in
  { Token.kind; span = { start = token_start; end_ } }

let next cursor delim_stack =
  let start = Cursor.position cursor in
  if Cursor.is_eof cursor then
    { Token.kind = Token.EOF; span = { start; end_ = start } }
  else
    match Cursor.peek cursor with
    | None -> { Token.kind = Token.EOF; span = { start; end_ = start } }
    | Some c when is_whitespace c -> lex_whitespace cursor start
    | Some '(' -> (
        match Cursor.peek_n cursor 1 with
        | Some '*' -> lex_comment cursor start
        | _ ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.OpenDelim Paren;
              span = Ceibo.Span.make ~start ~end_;
            })
    | Some ')' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        {
          Token.kind = Token.CloseDelim Paren;
          span = Ceibo.Span.make ~start ~end_;
        }
    | Some '[' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '|' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.OpenDelim Array;
              span = Ceibo.Span.make ~start ~end_;
            }
        | _ ->
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.OpenDelim Bracket;
              span = Ceibo.Span.make ~start ~end_;
            })
    | Some ']' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        {
          Token.kind = Token.CloseDelim Bracket;
          span = Ceibo.Span.make ~start ~end_;
        }
    | Some '{' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        {
          Token.kind = Token.OpenDelim Brace;
          span = Ceibo.Span.make ~start ~end_;
        }
    | Some '}' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        {
          Token.kind = Token.CloseDelim Brace;
          span = Ceibo.Span.make ~start ~end_;
        }
    | Some '+' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Plus; span = Ceibo.Span.make ~start ~end_ }
    | Some '-' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Arrow; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Minus; span = Ceibo.Span.make ~start ~end_ })
    | Some '*' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '*' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.StarStar; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Star; span = Ceibo.Span.make ~start ~end_ })
    | Some '/' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Slash; span = Ceibo.Span.make ~start ~end_ }
    | Some '%' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.PercentGt;
              span = Ceibo.Span.make ~start ~end_;
            }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Percent; span = Ceibo.Span.make ~start ~end_ })
    | Some '^' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Caret; span = Ceibo.Span.make ~start ~end_ }
    | Some '=' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.FatArrow; span = Ceibo.Span.make ~start ~end_ }
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.EqEq; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Eq; span = Ceibo.Span.make ~start ~end_ })
    | Some '<' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.LtEq; span = Ceibo.Span.make ~start ~end_ }
        | Some '-' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.LeftArrow;
              span = Ceibo.Span.make ~start ~end_;
            }
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Ne; span = Ceibo.Span.make ~start ~end_ }
        | Some '%' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.LtPercent;
              span = Ceibo.Span.make ~start ~end_;
            }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Lt; span = Ceibo.Span.make ~start ~end_ })
    | Some '>' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.GtEq; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Gt; span = Ceibo.Span.make ~start ~end_ })
    | Some '!' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.BangEq; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Bang; span = Ceibo.Span.make ~start ~end_ })
    | Some '&' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '&' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.And; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.Ampersand;
              span = Ceibo.Span.make ~start ~end_;
            })
    | Some '|' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '|' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Or; span = Ceibo.Span.make ~start ~end_ }
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.PipeGt; span = Ceibo.Span.make ~start ~end_ }
        | Some ']' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.CloseDelim Array;
              span = Ceibo.Span.make ~start ~end_;
            }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Pipe; span = Ceibo.Span.make ~start ~end_ })
    | Some ':' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some ':' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            {
              Token.kind = Token.ColonColon;
              span = Ceibo.Span.make ~start ~end_;
            }
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.ColonEq; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Colon; span = Ceibo.Span.make ~start ~end_ })
    | Some ';' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Semi; span = Ceibo.Span.make ~start ~end_ }
    | Some ',' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Comma; span = Ceibo.Span.make ~start ~end_ }
    | Some '.' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '.' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.DotDot; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.Dot; span = Ceibo.Span.make ~start ~end_ })
    | Some '?' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Question; span = Ceibo.Span.make ~start ~end_ }
    | Some '@' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '@' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            { Token.kind = Token.AtAt; span = Ceibo.Span.make ~start ~end_ }
        | _ ->
            let end_ = Cursor.position cursor in
            { Token.kind = Token.At; span = Ceibo.Span.make ~start ~end_ })
    | Some '#' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Hash; span = Ceibo.Span.make ~start ~end_ }
    | Some '~' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Tilde; span = Ceibo.Span.make ~start ~end_ }
    | Some '`' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Backtick; span = Ceibo.Span.make ~start ~end_ }
    | Some '$' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Dollar; span = Ceibo.Span.make ~start ~end_ }
    | Some '"' -> lex_string cursor start
    | Some '\'' -> (
        (* Distinguish between char literals and type variable quotes *)
        (* Char literal: 'a', '\\', etc. followed by closing '
           Type variable: 'a, 'foo (no closing ' after ident) *)
        match Cursor.peek_n cursor 2 with
        | Some '\'' -> lex_char cursor start (* It's a char literal like 'a' *)
        | _ -> (
            (* Check if it's an escape like '\\' or '\n' *)
            match Cursor.peek_n cursor 1 with
            | Some '\\' -> lex_char cursor start (* Escaped char *)
            | Some c when is_ident_start c ->
                (* It's a type variable like 'a or 'foo *)
                Cursor.advance cursor;
                let end_ = Cursor.position cursor in
                {
                  Token.kind = Token.Quote;
                  span = Ceibo.Span.make ~start ~end_;
                }
            | _ -> lex_char cursor start (* Try as char anyway *)))
    | Some c when is_digit c -> lex_number cursor start
    | Some c when is_ident_start c -> lex_ident cursor delim_stack start
    | Some c ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        { Token.kind = Token.Unknown c; span = Ceibo.Span.make ~start ~end_ }

let rec lex_all cursor delim_stack acc =
  let token = next cursor delim_stack in
  let new_stack =
    match token.Token.kind with
    | Token.OpenDelim d -> d :: delim_stack
    | Token.CloseDelim _ -> (
        match delim_stack with _ :: rest -> rest | [] -> delim_stack)
    | _ -> delim_stack
  in
  match token.Token.kind with
  | Token.EOF -> List.rev (token :: acc)
  | _ -> lex_all cursor new_stack (token :: acc)

let tokenize source =
  let cursor = create source in
  lex_all cursor [] []
