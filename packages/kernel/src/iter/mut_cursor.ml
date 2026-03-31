open Global0

type t = {
  source: string;
  mutable pos: int;
  length: int;
}

let create = fun source -> {source; pos = 0; length = String.length source}

let source = fun t -> t.source

let position = fun t -> t.pos

let length_remaining = fun t -> t.length - t.pos

let is_eof = fun t -> t.pos >= t.length

let peek = fun t ->
    if is_eof t then
      None
    else
      Some (String.get t.source t.pos)

let peek_n = fun t n ->
    let target = t.pos + n in
    if target >= t.length then
      None
    else
      Some (String.get t.source target)

let advance = fun t ->
    if not (is_eof t) then
      t.pos <- t.pos + 1

let advance_by = fun t n ->
    let new_pos = t.pos + n in
    if new_pos <= t.length then
      t.pos <- new_pos

let take_while = fun t f ->
    let start = t.pos in
    let rec loop () =
      if t.pos < String.length t.source then
        if f (String.get t.source t.pos) then
          (
            t.pos <- t.pos + 1;
            loop ()
          )
    in
    loop ();
    let len = t.pos - start in
    String.sub t.source start len

let skip_while = fun t f ->
    let rec loop () =
      if t.pos < String.length t.source then
        if f (String.get t.source t.pos) then
          (
            t.pos <- t.pos + 1;
            loop ()
          )
    in
    loop ()

let take_until = fun t f ->
    let start = t.pos in
    let rec loop () =
      if t.pos >= t.length then
        None
      else if f (String.get t.source t.pos) then
        Some t.pos
      else (
        t.pos <- t.pos + 1;
        loop ()
      )
    in
    match loop () with
    | None ->
        t.pos <- start;
        None
    | Some end_pos ->
        let taken = String.sub t.source start (end_pos - start) in
        Some taken

let take_n = fun t n ->
    if t.pos + n > t.length then
      None
    else
      let taken = String.sub t.source t.pos n in
      t.pos <- t.pos + n;
      Some taken

let remaining = fun t ->
    if is_eof t then
      ""
    else
      String.sub t.source t.pos (t.length - t.pos)
