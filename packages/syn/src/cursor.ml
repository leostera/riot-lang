open Std

type t = {
  source: string;
  mutable pos: int;
  length: int;
}

let create = fun source -> {source; pos = 0; length = String.length source}

let position = fun t -> t.pos

let is_eof = fun t -> t.pos >= t.length

let peek = fun t ->
    if is_eof t then
      None
    else
      Some (String.get t.source t.pos)

let peek_n = fun t n ->
    if t.pos + n >= t.length then
      None
    else
      Some (String.get t.source (t.pos + n))

let advance = fun t ->
    if not (is_eof t) then
      t.pos <- t.pos + 1

let skip_while = fun t f ->
    while (not (is_eof t)) && Option.map f (peek t) = Some true do
      advance t
    done

let take_while = fun t f ->
    let start = t.pos in
    skip_while t f;
    let len = t.pos - start in
    String.sub t.source start len

let slice = fun t start len ->
    String.sub t.source start len
