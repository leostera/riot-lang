open Std

type t = { bytes : bytes; mutable offset : int }

let create bytes = { bytes; offset = 0 }
let remaining reader = Bytes.length reader.bytes - reader.offset
let is_eof reader = reader.offset >= Bytes.length reader.bytes

let read_byte reader =
  if is_eof reader then None
  else
    let b = Char.code (Bytes.get reader.bytes reader.offset) in
    reader.offset <- reader.offset + 1;
    Some b

let read_int32 reader =
  if remaining reader < 4 then None
  else
    let b1 = Char.code (Bytes.get reader.bytes reader.offset) in
    let b2 = Char.code (Bytes.get reader.bytes (reader.offset + 1)) in
    let b3 = Char.code (Bytes.get reader.bytes (reader.offset + 2)) in
    let b4 = Char.code (Bytes.get reader.bytes (reader.offset + 3)) in
    reader.offset <- reader.offset + 4;
    Some ((b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4)

let read_int16 reader =
  if remaining reader < 2 then None
  else
    let b1 = Char.code (Bytes.get reader.bytes reader.offset) in
    let b2 = Char.code (Bytes.get reader.bytes (reader.offset + 1)) in
    reader.offset <- reader.offset + 2;
    Some ((b1 lsl 8) lor b2)

let read_string reader =
  let buf = Buffer.create 64 in
  let rec loop () =
    if is_eof reader then None
    else
      let c = Bytes.get reader.bytes reader.offset in
      reader.offset <- reader.offset + 1;
      if c = '\x00' then Some (Buffer.contents buf)
      else (
        Buffer.add_char buf c;
        loop ())
  in
  loop ()

let read_bytes reader len =
  if remaining reader < len then None
  else
    let result = Bytes.sub reader.bytes reader.offset len in
    reader.offset <- reader.offset + len;
    Some result

let read_cstring reader len =
  match read_bytes reader len with
  | None -> None
  | Some bytes -> Some (Bytes.to_string bytes)

let position reader = reader.offset
let set_position reader pos = reader.offset <- pos
