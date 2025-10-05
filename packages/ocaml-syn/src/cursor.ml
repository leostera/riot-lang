open Std

type t = { source : string; mutable pos : int; length : int }

let create source = { source; pos = 0; length = String.length source }
let position t = t.pos
let is_eof t = t.pos >= t.length
let peek t = if is_eof t then None else Some (String.get t.source t.pos)

let peek_n t n =
  if t.pos + n >= t.length then None else Some (String.get t.source (t.pos + n))

let advance t = if not (is_eof t) then t.pos <- t.pos + 1

let skip_while t f =
  while (not (is_eof t)) && Option.map f (peek t) = Some true do
    advance t
  done

let take_while t f =
  let start = t.pos in
  skip_while t f;
  let len = t.pos - start in
  String.sub t.source start len

let slice t start len = String.sub t.source start len
