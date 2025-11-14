open Std
open Std.Collections

type t = { source : string; tokens : Token.located array; mutable pos : int }

let create ~source tokens = { source; tokens = Array.of_list tokens; pos = 0 }
let position t = t.pos
let is_eof t = t.pos >= Array.length t.tokens

let peek t =
  if is_eof t then
    {
      Token.kind = Eof;
      span =
        Ceibo.Span.make ~start:(String.length t.source)
          ~end_:(String.length t.source);
    }
  else Array.get t.tokens t.pos

let peek_n t n =
  let target = t.pos + n in
  if target >= Array.length t.tokens then
    {
      Token.kind = Eof;
      span =
        Ceibo.Span.make ~start:(String.length t.source)
          ~end_:(String.length t.source);
    }
  else Array.get t.tokens target

let advance t = if not (is_eof t) then t.pos <- t.pos + 1

let last_token t =
  if t.pos > 0 then Array.get t.tokens (t.pos - 1) else Array.get t.tokens 0

let view t span =
  let start = span.Ceibo.Span.start in
  let len = span.end_ - start in
  String.sub t.source start len

let set_position t pos = t.pos <- pos
