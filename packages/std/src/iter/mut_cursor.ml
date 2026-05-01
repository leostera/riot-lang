open Kernel

module IoSlice = Kernel.IO.IoVec.IoSlice

type t = {
  source: IoSlice.t;
  mutable pos: int;
  length: int;
}

let panic = Kernel.SystemError.panic

let unwrap_slice = fun context ->
  fun __tmp1 ->
    match __tmp1 with
    | Kernel.Result.Ok value -> value
    | Kernel.Result.Error error ->
        panic (Kernel.String.concat "" [ context; ": "; Kernel.IO.Error.message error ])

let from_slice = fun source -> { source; pos = 0; length = IoSlice.length source }

let from_string = fun source ->
  from_slice
    (unwrap_slice "Iter.MutCursor.from_string" (IoSlice.from_string source))

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
  if not (is_eof cursor) then
    cursor.pos <- cursor.pos + 1

let advance_by = fun cursor count ->
  let pos = cursor.pos + count in
  if pos <= cursor.length then
    cursor.pos <- pos

let take_while = fun cursor predicate ->
  let start = cursor.pos in
  let rec loop () =
    match peek cursor with
    | Some value when predicate value ->
        cursor.pos <- cursor.pos + 1;
        loop ()
    | _ -> ()
  in
  loop ();
  IoSlice.sub_unchecked cursor.source ~off:start ~len:(cursor.pos - start)

let take_while_string = fun cursor predicate ->
  take_while cursor predicate
  |> IoSlice.to_string

let skip_while = fun cursor predicate ->
  let rec loop () =
    match peek cursor with
    | Some value when predicate value ->
        cursor.pos <- cursor.pos + 1;
        loop ()
    | _ -> ()
  in
  loop ()

let take_until = fun cursor predicate ->
  let start = cursor.pos in
  let rec loop () =
    if cursor.pos >= cursor.length then
      None
    else if predicate (IoSlice.get_unchecked cursor.source ~at:cursor.pos) then
      Some cursor.pos
    else (
      cursor.pos <- cursor.pos + 1;
      loop ()
    )
  in
  match loop () with
  | None ->
      cursor.pos <- start;
      None
  | Some stop -> Some (IoSlice.sub_unchecked cursor.source ~off:start ~len:(stop - start))

let take_until_string = fun cursor predicate ->
  match take_until cursor predicate with
  | None -> None
  | Some slice -> Some (IoSlice.to_string slice)

let take_until_char = fun cursor needle ->
  let start = cursor.pos in
  let rec loop () =
    if cursor.pos >= cursor.length then
      None
    else if IoSlice.get_unchecked cursor.source ~at:cursor.pos = needle then
      Some cursor.pos
    else (
      cursor.pos <- cursor.pos + 1;
      loop ()
    )
  in
  match loop () with
  | None ->
      cursor.pos <- start;
      None
  | Some stop -> Some (IoSlice.sub_unchecked cursor.source ~off:start ~len:(stop - start))

let take_until_char_string = fun cursor needle ->
  match take_until_char cursor needle with
  | None -> None
  | Some slice -> Some (IoSlice.to_string slice)

let take_n = fun cursor count ->
  if cursor.pos + count > cursor.length then
    None
  else
    (
      let taken = IoSlice.sub_unchecked cursor.source ~off:cursor.pos ~len:count in
      cursor.pos <- cursor.pos + count;
      Some taken
    )

let take_n_string = fun cursor count ->
  match take_n cursor count with
  | None -> None
  | Some slice -> Some (IoSlice.to_string slice)

let remaining = fun cursor ->
  if is_eof cursor then
    IoSlice.empty
  else
    IoSlice.sub_unchecked cursor.source ~off:cursor.pos ~len:(cursor.length - cursor.pos)

let remaining_string = fun cursor -> IoSlice.to_string (remaining cursor)
