open Kernel

type t = {
  source: string;
  pos: int;
  length: int;
}

let string_length = Kernel.String.length

let string_get = Kernel.String.get_unchecked

let string_sub = fun source start len ->
  source
  |> Kernel.Bytes.from_string
  |> Kernel.Bytes.sub_unchecked ~offset:start ~len
  |> Kernel.Bytes.to_string

let create = fun source -> { source; pos = 0; length = string_length source }

let source = fun cursor -> cursor.source

let position = fun cursor -> cursor.pos

let length_remaining = fun cursor -> cursor.length - cursor.pos

let is_eof = fun cursor -> cursor.pos >= cursor.length

let peek = fun cursor ->
  if is_eof cursor then
    None
  else
    Some (string_get cursor.source ~at:cursor.pos)

let peek_n = fun cursor count ->
  let target = cursor.pos + count in
  if target >= cursor.length then
    None
  else
    Some (string_get cursor.source ~at:target)

let advance = fun cursor ->
  if is_eof cursor then
    None
  else
    Some { cursor with pos = cursor.pos + 1 }

let advance_by = fun cursor count ->
  let pos = cursor.pos + count in
  if pos > cursor.length then
    None
  else
    Some { cursor with pos }

let take_while = fun cursor predicate ->
  let start = cursor.pos in
  let rec loop pos =
    if pos >= cursor.length then
      pos
    else if predicate (string_get cursor.source ~at:pos) then
      loop (pos + 1)
    else
      pos
  in
  let stop = loop start in
  (string_sub cursor.source start (stop - start), { cursor with pos = stop })

let skip_while = fun cursor predicate ->
  let _, cursor = take_while cursor predicate in
  cursor

let take_until = fun cursor predicate ->
  let start = cursor.pos in
  let rec loop pos =
    if pos >= cursor.length then
      None
    else if predicate (string_get cursor.source ~at:pos) then
      Some pos
    else
      loop (pos + 1)
  in
  match loop start with
  | None -> None
  | Some stop -> Some (string_sub cursor.source start (stop - start), { cursor with pos = stop })

let take_n = fun cursor count ->
  if cursor.pos + count > cursor.length then
    None
  else
    Some (string_sub cursor.source cursor.pos count, { cursor with pos = cursor.pos + count })

let remaining = fun cursor ->
  if is_eof cursor then
    ""
  else
    string_sub cursor.source cursor.pos (cursor.length - cursor.pos)
