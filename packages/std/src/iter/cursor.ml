open Kernel

let panic = Kernel.SystemError.panic

module IoSlice = Kernel.IO.IoVec.IoSlice

type t = {
  source: IoSlice.t;
  pos: int;
  length: int;
}

let unwrap_slice = fun context ->
  function
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> panic
    (Kernel.String.concat "" [ context; ": "; Kernel.IO.Error.message error ])

let from_slice = fun source -> { source; pos = 0; length = IoSlice.length source }

let from_string = fun source ->
  from_slice (unwrap_slice "Iter.Cursor.from_string" (IoSlice.from_string source))

let create = from_string

let source = fun cursor -> cursor.source

let source_string = fun cursor -> IoSlice.to_string cursor.source

let position = fun cursor -> cursor.pos

let length_remaining = fun cursor -> cursor.length - cursor.pos

let is_eof = fun cursor -> cursor.pos >= cursor.length

let peek = fun cursor ->
  if is_eof cursor then
    None
  else
    Some (IoSlice.get_unchecked cursor.source ~at:cursor.pos)

let peek_n = fun cursor count ->
  let target = cursor.pos + count in
  if target >= cursor.length then
    None
  else
    Some (IoSlice.get_unchecked cursor.source ~at:target)

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
    else if predicate (IoSlice.get_unchecked cursor.source ~at:pos) then
      loop (pos + 1)
    else
      pos
  in
  let stop = loop start in
  (IoSlice.sub_unchecked cursor.source ~off:start ~len:(stop - start), { cursor with pos = stop })

let take_while_string = fun cursor predicate ->
  let (taken, cursor) = take_while cursor predicate in
  (IoSlice.to_string taken, cursor)

let skip_while = fun cursor predicate ->
  let (_, cursor) = take_while cursor predicate in
  cursor

let take_until = fun cursor predicate ->
  let start = cursor.pos in
  let rec loop pos =
    if pos >= cursor.length then
      None
    else if predicate (IoSlice.get_unchecked cursor.source ~at:pos) then
      Some pos
    else
      loop (pos + 1)
  in
  match loop start with
  | None -> None
  | Some stop -> Some (
    IoSlice.sub_unchecked cursor.source ~off:start ~len:(stop - start),
    { cursor with pos = stop }
  )

let take_until_string = fun cursor predicate ->
  match take_until cursor predicate with
  | None -> None
  | Some (taken, cursor) -> Some (IoSlice.to_string taken, cursor)

let take_until_char = fun cursor needle ->
  let start = cursor.pos in
  let rec loop pos =
    if pos >= cursor.length then
      None
    else if IoSlice.get_unchecked cursor.source ~at:pos = needle then
      Some pos
    else
      loop (pos + 1)
  in
  match loop start with
  | None -> None
  | Some stop -> Some (
    IoSlice.sub_unchecked cursor.source ~off:start ~len:(stop - start),
    { cursor with pos = stop }
  )

let take_until_char_string = fun cursor needle ->
  match take_until_char cursor needle with
  | None -> None
  | Some (taken, cursor) -> Some (IoSlice.to_string taken, cursor)

let take_n = fun cursor count ->
  if cursor.pos + count > cursor.length then
    None
  else
    Some (
      IoSlice.sub_unchecked cursor.source ~off:cursor.pos ~len:count,
      { cursor with pos = cursor.pos + count }
    )

let take_n_string = fun cursor count ->
  match take_n cursor count with
  | None -> None
  | Some (taken, cursor) -> Some (IoSlice.to_string taken, cursor)

let remaining = fun cursor ->
  if is_eof cursor then
    IoSlice.empty
  else
    IoSlice.sub_unchecked cursor.source ~off:cursor.pos ~len:(cursor.length - cursor.pos)

let remaining_string = fun cursor -> IoSlice.to_string (remaining cursor)
