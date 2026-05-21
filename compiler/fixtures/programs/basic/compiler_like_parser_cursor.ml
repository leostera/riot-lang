type token =
  | Fn
  | Ident(String)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Eof

type cursor = { remaining: List<token>, consumed: i64 }

fn advance(cursor: cursor) -> cursor {
  match cursor.remaining {
    [] -> cursor,
    [_token, ..rest] -> cursor { remaining: rest, consumed: cursor.consumed + 1 }
  }
}

fn parse_function(cursor: cursor) -> cursor {
  match cursor.remaining {
    [Fn, Ident(_), LParen, RParen, LBrace, RBrace, ..rest] -> cursor { remaining: rest, consumed: cursor.consumed + 6 },
    [_, .._] -> advance(cursor),
    [] -> cursor
  }
}

fn parse_all(cursor: cursor) -> i64 {
  match cursor.remaining {
    [] -> cursor.consumed,
    [Eof, .._] -> cursor.consumed,
    _ -> parse_all(parse_function(cursor))
  }
}

fn main() {
  let tokens = [Fn, Ident("main"), LParen, RParen, LBrace, RBrace, Eof];
  dbg(parse_all(cursor { remaining: tokens, consumed: 0 }))
}
