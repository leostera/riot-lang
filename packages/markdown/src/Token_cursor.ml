open Std

type t = {
  tokens : Token.t array;
  mutable pos : int;
  length : int;
  source : string; (* Keep source for view function *)
}

let create ~source tokens =
  let tokens = Array.of_list tokens in
  { tokens; pos = 0; length = Array.length tokens; source }

let position t = t.pos
let is_eof t = t.pos >= t.length

let eof_token () =
  { Token.kind = Syntax_kind.EOF; span = Ceibo.Span.make ~start:0 ~end_:0 }

let peek t = if is_eof t then eof_token () else t.tokens.(t.pos)

let peek_n t n =
  if t.pos + n >= t.length then eof_token () else t.tokens.(t.pos + n)

let advance t = if not (is_eof t) then t.pos <- t.pos + 1

let skip_while t f =
  while (not (is_eof t)) && f (peek t) do
    advance t
  done

let take_while t f =
  let start = t.pos in
  skip_while t f;
  let len = t.pos - start in
  Array.sub t.tokens start len |> Array.to_list

let slice t start len = Array.sub t.tokens start len |> Array.to_list

(** Get a substring view of the source at the given span *)
let view t span =
  let start_pos = span.Ceibo.Span.start in
  let end_pos = span.Ceibo.Span.end_ in
  String.sub t.source start_pos (end_pos - start_pos)

(** Get the last consumed non-trivia token. Returns first token if at start. *)
let last_token t =
  let rec find_last_non_trivia idx =
    if idx < 0 then if t.length > 0 then t.tokens.(0) else eof_token ()
    else
      let tok = t.tokens.(idx) in
      match tok.Token.kind with
      | Syntax_kind.SPACE | Syntax_kind.TAB | Syntax_kind.NEWLINE ->
          find_last_non_trivia (idx - 1)
      | _ -> tok
  in
  if t.pos > 0 then find_last_non_trivia (t.pos - 1)
  else if t.length > 0 then t.tokens.(0)
  else eof_token ()
