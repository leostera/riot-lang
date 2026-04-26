open Std

module Slice = IO.IoVec.IoSlice

type t = {
  source: Slice.t;
  mutable pos: int;
  length: int;
}

let create = fun source -> { source; pos = 0; length = Slice.length source }

let source = fun t -> t.source

let position = fun t -> t.pos

let is_eof = fun t -> t.pos >= t.length

let peek = fun t ->
  if is_eof t then
    None
  else
    Some (Slice.get_unchecked t.source ~at:t.pos)

let peek_n = fun t n ->
  if t.pos + n >= t.length then
    None
  else
    Some (Slice.get_unchecked t.source ~at:(t.pos + n))

let advance = fun t ->
  if not (is_eof t) then
    t.pos <- t.pos + 1

let skip_while = fun t f ->
  let rec loop () =
    if (not (is_eof t)) && Option.map (peek t) ~fn:f = Some true then (
      advance t;
      loop ()
    )
  in
  loop ()

let take_slice = fun t f ->
  let start = t.pos in
  skip_while t f;
  let len = t.pos - start in
  Slice.sub_unchecked t.source ~off:start ~len

let take_while = fun t f ->
  take_slice t f
  |> Slice.to_string

let slice_view = fun t start len -> Slice.sub_unchecked t.source ~off:start ~len

let slice = fun t start len ->
  slice_view t start len
  |> Slice.to_string
