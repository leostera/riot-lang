open Std
open Std.IO

type t = {
  bytes: bytes;
  mutable offset: int;
}

let create = fun bytes -> { bytes; offset = 0 }

let remaining = fun reader -> Bytes.length reader.bytes - reader.offset

let is_eof = fun reader -> reader.offset >= Bytes.length reader.bytes

let read_byte = fun reader ->
  if is_eof reader then
    None
  else
    let b = Char.code (Option.unwrap (Bytes.get reader.bytes ~at:reader.offset)) in
    reader.offset <- reader.offset + 1;
  Some b

let read_int32 = fun reader ->
  if remaining reader < 4 then
    None
  else
    let b1 = Char.code (Option.unwrap (Bytes.get reader.bytes ~at:reader.offset)) in
    let b2 = Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 1))) in
    let b3 = Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 2))) in
    let b4 = Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 3))) in
    reader.offset <- reader.offset + 4;
  (* Construct as unsigned, then convert to signed *)
  let unsigned = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
  (* Convert to signed: if high bit is set, it's negative *)
  let signed =
    if unsigned >= 0x8000_0000 then
      unsigned - 0x1_0000_0000
      (* Convert to negative *)
    else
      unsigned
  in
  Some signed

let read_int16 = fun reader ->
  if remaining reader < 2 then
    None
  else
    let b1 = Char.code (Option.unwrap (Bytes.get reader.bytes ~at:reader.offset)) in
    let b2 = Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 1))) in
    reader.offset <- reader.offset + 2;
  Some ((b1 lsl 8) lor b2)

let read_int64 = fun reader ->
  if remaining reader < 8 then
    None
  else
    let b1 =
      Int64.from_int (Char.code (Option.unwrap (Bytes.get reader.bytes ~at:reader.offset)))
    in
    let b2 =
      Int64.from_int (Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 1))))
    in
    let b3 =
      Int64.from_int (Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 2))))
    in
    let b4 =
      Int64.from_int (Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 3))))
    in
    let b5 =
      Int64.from_int (Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 4))))
    in
    let b6 =
      Int64.from_int (Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 5))))
    in
    let b7 =
      Int64.from_int (Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 6))))
    in
    let b8 =
      Int64.from_int (Char.code (Option.unwrap (Bytes.get reader.bytes ~at:(reader.offset + 7))))
    in
    reader.offset <- reader.offset + 8;
  let result =
    Int64.(logor
      (logor
        (logor (shift_left b1 56) (shift_left b2 48))
        (logor (shift_left b3 40) (shift_left b4 32)))
      (logor (logor (shift_left b5 24) (shift_left b6 16)) (logor (shift_left b7 8) b8)))
  in
  Some result

let read_float64 = fun reader ->
  match read_int64 reader with
  | None -> None
  | Some bits -> Some (Int64.float_of_bits bits)

let read_float32 = fun reader ->
  match read_int32 reader with
  | None -> None
  | Some bits -> Some (Int32.float_of_bits (Int32.from_int bits))

let read_string = fun reader ->
  let buf = Buffer.create ~size:64 in
  let rec loop () =
    if is_eof reader then
      None
    else
      let c = Option.unwrap (Bytes.get reader.bytes ~at:reader.offset) in
      reader.offset <- reader.offset + 1;
    if c = '\x00' then
      Some (Buffer.contents buf)
    else (
      Buffer.add_char buf c;
      loop ()
    )
  in
  loop ()

let read_bytes = fun reader len ->
  if remaining reader < len then
    None
  else
    match Bytes.sub reader.bytes ~offset:reader.offset ~len with
    | Error _ -> None
    | Ok result ->
        reader.offset <- reader.offset + len;
        Some result

let read_cstring = fun reader len ->
  match read_bytes reader len with
  | None -> None
  | Some bytes -> Some (Bytes.to_string bytes)

let position = fun reader -> reader.offset

let set_position = fun reader pos -> reader.offset <- pos
