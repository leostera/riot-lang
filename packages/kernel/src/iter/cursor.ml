module String = Stdlib.String

type t = { source : string; pos : int; length : int }

let create source = { source; pos = 0; length = String.length source }
let source t = t.source
let position t = t.pos
let length_remaining t = t.length - t.pos
let is_eof t = t.pos >= t.length
let peek t = if is_eof t then None else Some (String.get t.source t.pos)

let peek_n t n =
  let target = t.pos + n in
  if target >= t.length then None else Some (String.get t.source target)

let advance t = if is_eof t then None else Some { t with pos = t.pos + 1 }

let advance_by t n =
  let new_pos = t.pos + n in
  if new_pos > t.length then None else Some { t with pos = new_pos }

let take_while t f =
  let start = t.pos in
  let rec loop pos =
    if pos >= t.length then pos
    else if f (String.get t.source pos) then loop (pos + 1)
    else pos
  in
  let end_pos = loop start in
  let taken = String.sub t.source start (end_pos - start) in
  (taken, { t with pos = end_pos })

let skip_while t f =
  let _, new_cursor = take_while t f in
  new_cursor

let take_until t f =
  let start = t.pos in
  let rec loop pos =
    if pos >= t.length then None
    else if f (String.get t.source pos) then Some pos
    else loop (pos + 1)
  in
  match loop start with
  | None -> None
  | Some end_pos ->
      let taken = String.sub t.source start (end_pos - start) in
      Some (taken, { t with pos = end_pos })

let take_n t n =
  if t.pos + n > t.length then None
  else
    let taken = String.sub t.source t.pos n in
    Some (taken, { t with pos = t.pos + n })

let remaining t =
  if is_eof t then "" else String.sub t.source t.pos (t.length - t.pos)
