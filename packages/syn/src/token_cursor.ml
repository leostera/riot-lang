open Std
open Std.Collections

type t = {
  tokens: Token.t array;
  mutable pos: int;
  mutable leading_trivia_consumed: bool;
  length: int;
  source: string;
  (* Keep source for span-based debug views. *)
}

let create = fun ~source tokens ->
  let tokens = Array.from_list tokens in
  {
    tokens;
    pos = 0;
    leading_trivia_consumed = false;
    length = Array.length tokens;
    source;
  }

let position = fun t ->
  (t.pos * 2) + (
    if t.leading_trivia_consumed then
      1
    else
      0
  )

let set_position = fun t pos ->
  t.pos <- pos / 2;
  t.leading_trivia_consumed <- pos mod 2 = 1

let is_eof = fun t -> t.pos >= t.length

let eof_token = fun () ->
  { Token.kind = Token.EOF; span = Ceibo.Span.make ~start:0 ~end_:0; leading_trivia = [] }

let token_at = fun tokens index -> Array.get_unchecked tokens ~at:index

let array_to_list = fun values ->
  Array.fold_right values ~acc:[] ~fn:(fun value acc -> value :: acc)

let peek = fun t ->
  if is_eof t then
    eof_token ()
  else
    token_at t.tokens t.pos

let peek_n = fun t n ->
  if t.pos + n >= t.length then
    eof_token ()
  else
    token_at t.tokens (t.pos + n)

let advance = fun t ->
  if not (is_eof t) then
    (
      t.pos <- t.pos + 1;
      t.leading_trivia_consumed <- false
    )

let skip_while = fun t f ->
  while (not (is_eof t)) && f (peek t) do
    advance t
  done

let take_while = fun t f ->
  let start = t.pos in
  skip_while t f;
  let len = t.pos - start in
  Array.sub t.tokens ~offset:start ~len |> array_to_list

let slice = fun t start len -> Array.sub t.tokens ~offset:start ~len |> array_to_list

let view = fun t span ->
  let start_pos = span.Ceibo.Span.start in
  let end_pos = span.Ceibo.Span.end_ in
  String.sub t.source ~offset:start_pos ~len:(end_pos - start_pos)

let peek_leading_trivia = fun t ->
  if is_eof t || t.leading_trivia_consumed then
    []
  else
    (token_at t.tokens t.pos).Token.leading_trivia

let consume_leading_trivia = fun t ->
  let trivia = peek_leading_trivia t in
  t.leading_trivia_consumed <- true;
  List.map trivia ~fn:Token.trivia_to_token

let last_token = fun t ->
  if t.pos > 0 then
    token_at t.tokens (t.pos - 1)
  else if t.length > 0 then
    token_at t.tokens 0
  else
    eof_token ()
