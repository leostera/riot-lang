open Std

type t = { source : string; mutable pos : int }

let create source = { source; pos = 0 }
let position t = t.pos
let is_eof t = t.pos >= String.length t.source
let peek t = if is_eof t then None else Some (String.get t.source t.pos)

let peek_n t n =
  let target_pos = t.pos + n in
  if target_pos >= String.length t.source then None
  else Some (String.get t.source target_pos)

let advance t = if not (is_eof t) then t.pos <- t.pos + 1

let skip_while t pred =
  while match peek t with Some c -> pred c | None -> false do
    advance t
  done

let take_while t pred =
  let start = t.pos in
  skip_while t pred;
  let len = t.pos - start in
  String.sub t.source start len

let slice t start len =
  if start + len > String.length t.source then
    String.sub t.source start (String.length t.source - start)
  else String.sub t.source start len

let view t span = slice t span.Ceibo.Span.start (span.end_ - span.start)
