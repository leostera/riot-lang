open Kernel

let panic = Kernel.SystemError.panic
module View = Kernel.IO.StringView

type t = {
  source: View.t;
  pos: int;
  length: int;
}

let unwrap_view = fun context ->
  function
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error ->
      panic (Kernel.String.concat "" [ context; ": "; Kernel.IO.Error.message error ])

let from_view = fun source -> { source; pos = 0; length = View.length source }

let from_slice = fun source -> from_view (View.from_slice source)

let from_string = fun source ->
  from_view (unwrap_view "Iter.Cursor.from_string" (View.from_string source))

let create = from_string

let source_view = fun cursor -> cursor.source

let source = fun cursor -> View.to_string cursor.source

let position = fun cursor -> cursor.pos

let length_remaining = fun cursor -> cursor.length - cursor.pos

let is_eof = fun cursor -> cursor.pos >= cursor.length

let peek = fun cursor ->
  if is_eof cursor then
    None
  else
    Some (View.get_unchecked cursor.source ~at:cursor.pos)

let peek_n = fun cursor count ->
  let target = cursor.pos + count in
  if target >= cursor.length then
    None
  else
    Some (View.get_unchecked cursor.source ~at:target)

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

let take_while_view = fun cursor predicate ->
  let start = cursor.pos in
  let rec loop pos =
    if pos >= cursor.length then
      pos
    else if predicate (View.get_unchecked cursor.source ~at:pos) then
      loop (pos + 1)
    else
      pos
  in
  let stop = loop start in
  (
    unwrap_view "Iter.Cursor.take_while_view" (View.sub cursor.source ~off:start ~len:(stop - start)),
    { cursor with pos = stop }
  )

let take_while = fun cursor predicate ->
  let taken, cursor = take_while_view cursor predicate in
  (View.to_string taken, cursor)

let skip_while = fun cursor predicate ->
  let _, cursor = take_while_view cursor predicate in
  cursor

let take_until_view = fun cursor predicate ->
  let start = cursor.pos in
  let rec loop pos =
    if pos >= cursor.length then
      None
    else if predicate (View.get_unchecked cursor.source ~at:pos) then
      Some pos
    else
      loop (pos + 1)
  in
  match loop start with
  | None -> None
  | Some stop ->
      Some (
        unwrap_view "Iter.Cursor.take_until_view" (View.sub cursor.source ~off:start ~len:(stop - start)),
        { cursor with pos = stop }
      )

let take_until = fun cursor predicate ->
  match take_until_view cursor predicate with
  | None -> None
  | Some (taken, cursor) -> Some (View.to_string taken, cursor)

let take_n_view = fun cursor count ->
  if cursor.pos + count > cursor.length then
    None
  else
    Some (
      unwrap_view "Iter.Cursor.take_n_view" (View.sub cursor.source ~off:cursor.pos ~len:count),
      { cursor with pos = cursor.pos + count }
    )

let take_n = fun cursor count ->
  match take_n_view cursor count with
  | None -> None
  | Some (taken, cursor) -> Some (View.to_string taken, cursor)

let remaining_view = fun cursor ->
  if is_eof cursor then
    View.empty
  else
    unwrap_view
      "Iter.Cursor.remaining_view"
      (View.sub cursor.source ~off:cursor.pos ~len:(cursor.length - cursor.pos))

let remaining = fun cursor -> View.to_string (remaining_view cursor)
