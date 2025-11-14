open Std

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let lex_whitespace cursor start =
  Cursor.skip_while cursor is_whitespace;
  let end_ = Cursor.position cursor in
  { Token.kind = Whitespace; span = Ceibo.Span.make ~start ~end_ }

let lex_comment cursor start =
  Cursor.advance cursor;
  let rec loop () =
    match Cursor.peek cursor with
    | Some '\n' | None -> ()
    | Some _ ->
        Cursor.advance cursor;
        loop ()
  in
  loop ();
  let text =
    Cursor.slice cursor (start + 1) (Cursor.position cursor - start - 1)
  in
  let end_ = Cursor.position cursor in
  { Token.kind = Comment text; span = Ceibo.Span.make ~start ~end_ }

let lex_string cursor start =
  Cursor.advance cursor;
  let content_start = Cursor.position cursor in
  let rec loop () =
    match Cursor.peek cursor with
    | None ->
        ( Cursor.slice cursor content_start
            (Cursor.position cursor - content_start),
          false )
    | Some '"' ->
        let value =
          Cursor.slice cursor content_start
            (Cursor.position cursor - content_start)
        in
        Cursor.advance cursor;
        (value, true)
    | Some '\\' ->
        Cursor.advance cursor;
        (match Cursor.peek cursor with
        | Some _ -> Cursor.advance cursor
        | None -> ());
        loop ()
    | Some _ ->
        Cursor.advance cursor;
        loop ()
  in
  let value, terminated = loop () in
  let end_ = Cursor.position cursor in
  {
    Token.kind = String { value; terminated };
    span = Ceibo.Span.make ~start ~end_;
  }

let lex_single_quoted_string cursor start =
  Cursor.advance cursor;
  let content_start = Cursor.position cursor in
  let rec loop () =
    match Cursor.peek cursor with
    | None ->
        ( Cursor.slice cursor content_start
            (Cursor.position cursor - content_start),
          false )
    | Some '\'' ->
        let value =
          Cursor.slice cursor content_start
            (Cursor.position cursor - content_start)
        in
        Cursor.advance cursor;
        (value, true)
    | Some '\\' ->
        Cursor.advance cursor;
        (match Cursor.peek cursor with
        | Some _ -> Cursor.advance cursor
        | None -> ());
        loop ()
    | Some _ ->
        Cursor.advance cursor;
        loop ()
  in
  let value, terminated = loop () in
  let end_ = Cursor.position cursor in
  {
    Token.kind = String { value; terminated };
    span = Ceibo.Span.make ~start ~end_;
  }

let lex_number cursor start =
  let rec loop () =
    match Cursor.peek cursor with
    | Some '0' .. '9' ->
        Cursor.advance cursor;
        loop ()
    | _ -> ()
  in
  loop ();
  let text = Cursor.slice cursor start (Cursor.position cursor - start) in
  let n = int_of_string text in
  let end_ = Cursor.position cursor in
  { Token.kind = Integer n; span = Ceibo.Span.make ~start ~end_ }

let lex_ident cursor start =
  Cursor.skip_while cursor is_ident_continue;
  let text = Cursor.slice cursor start (Cursor.position cursor - start) in
  let end_ = Cursor.position cursor in
  let kind =
    match String.get text 0 with
    | 'A' .. 'Z' -> Token.Variable text
    | '_' when String.length text = 1 -> Token.Wildcard
    | _ -> Token.Ident text
  in
  { Token.kind; span = Ceibo.Span.make ~start ~end_ }

let next cursor =
  let start = Cursor.position cursor in

  if Cursor.is_eof cursor then
    { Token.kind = Eof; span = Ceibo.Span.make ~start ~end_:start }
  else
    match Cursor.peek cursor with
    | None -> { Token.kind = Eof; span = Ceibo.Span.make ~start ~end_:start }
    | Some c when is_whitespace c -> lex_whitespace cursor start
    | Some '.' ->
        Cursor.advance cursor;
        {
          kind = Dot;
          span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
        }
    | Some ',' ->
        Cursor.advance cursor;
        {
          kind = Comma;
          span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
        }
    | Some '(' ->
        Cursor.advance cursor;
        {
          kind = LParen;
          span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
        }
    | Some ')' ->
        Cursor.advance cursor;
        {
          kind = RParen;
          span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
        }
    | Some '!' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            {
              kind = NotEq;
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            }
        | _ ->
            {
              kind = Bang;
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            })
    | Some ':' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '-' ->
            Cursor.advance cursor;
            {
              kind = ColonDash;
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            }
        | _ ->
            {
              kind = Unknown ':';
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            })
    | Some '>' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            {
              kind = GtEq;
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            }
        | _ ->
            {
              kind = Gt;
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            })
    | Some '<' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            {
              kind = LtEq;
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            }
        | _ ->
            {
              kind = Lt;
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            })
    | Some '=' ->
        Cursor.advance cursor;
        {
          kind = Eq;
          span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
        }
    | Some '%' -> lex_comment cursor start
    | Some '"' -> lex_string cursor start
    | Some '\'' -> lex_single_quoted_string cursor start
    | Some '-' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '0' .. '9' -> (
            let { Token.kind; span } =
              lex_number cursor (Cursor.position cursor)
            in
            match kind with
            | Integer n ->
                {
                  kind = Integer (-n);
                  span = Ceibo.Span.make ~start ~end_:span.end_;
                }
            | _ -> { kind; span })
        | _ ->
            {
              kind = Unknown '-';
              span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
            })
    | Some c when is_digit c -> lex_number cursor start
    | Some c when is_ident_start c -> lex_ident cursor start
    | Some c ->
        Cursor.advance cursor;
        {
          kind = Unknown c;
          span = Ceibo.Span.make ~start ~end_:(Cursor.position cursor);
        }

let rec tokenize_all cursor acc =
  let token = next cursor in
  match token.Token.kind with
  | Eof -> List.rev (token :: acc)
  | _ -> tokenize_all cursor (token :: acc)

let tokenize source =
  let cursor = Cursor.create source in
  tokenize_all cursor []
