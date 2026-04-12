open Kernel

type t = {
  source: string;
  mutable pos: int;
  length: int;
}

let string_length = Kernel.String.length

let string_get = Kernel.String.get

let string_sub = fun source start len ->
  let bytes = Kernel.Bytes.of_string source in
  Kernel.Bytes.sub_string bytes start len

let create = fun source -> { source; pos = 0; length = string_length source }

let source = fun cursor -> cursor.source

let position = fun cursor -> cursor.pos

let length_remaining = fun cursor -> cursor.length - cursor.pos

let is_eof = fun cursor -> cursor.pos >= cursor.length

let peek = fun cursor ->
  if is_eof cursor then
    None
  else
    Some (string_get cursor.source cursor.pos)

let peek_n = fun cursor count ->
  let target = cursor.pos + count in
  if target >= cursor.length then
    None
  else
    Some (string_get cursor.source target)

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
  string_sub cursor.source start (cursor.pos - start)

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
    else if predicate (string_get cursor.source cursor.pos) then
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
  | Some stop -> Some (string_sub cursor.source start (stop - start))

let take_n = fun cursor count ->
  if cursor.pos + count > cursor.length then
    None
  else
    (
      let taken = string_sub cursor.source cursor.pos count in
      cursor.pos <- cursor.pos + count;
      Some taken
    )

let remaining = fun cursor ->
  if is_eof cursor then
    ""
  else
    string_sub cursor.source cursor.pos (cursor.length - cursor.pos)
