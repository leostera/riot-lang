open Std

type t = Base64 | QuotedPrintable | SevenBit | EightBit | Binary

let of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "base64" -> Ok Base64
  | "quoted-printable" -> Ok QuotedPrintable
  | "7bit" -> Ok SevenBit
  | "8bit" -> Ok EightBit
  | "binary" -> Ok Binary
  | _ -> Error (format "Unknown encoding: %s" s)

let to_string = function
  | Base64 -> "base64"
  | QuotedPrintable -> "quoted-printable"
  | SevenBit -> "7bit"
  | EightBit -> "8bit"
  | Binary -> "binary"

let base64_encode s =
  let b64_chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let len = String.length s in
  let buf = Buffer.create ((len * 4 / 3) + 4) in

  let rec encode_chunk i =
    if i >= len then ()
    else if i + 2 < len then (
      let c1 = Char.code (String.get s i) in
      let c2 = Char.code (String.get s (i + 1)) in
      let c3 = Char.code (String.get s (i + 2)) in

      Buffer.add_char buf (String.get b64_chars ((c1 lsr 2) land 0x3F));
      Buffer.add_char buf
        (String.get b64_chars ((c1 lsl 4) lor (c2 lsr 4) land 0x3F));
      Buffer.add_char buf
        (String.get b64_chars ((c2 lsl 2) lor (c3 lsr 6) land 0x3F));
      Buffer.add_char buf (String.get b64_chars (c3 land 0x3F));
      encode_chunk (i + 3))
    else if i + 1 < len then (
      let c1 = Char.code (String.get s i) in
      let c2 = Char.code (String.get s (i + 1)) in

      Buffer.add_char buf (String.get b64_chars ((c1 lsr 2) land 0x3F));
      Buffer.add_char buf
        (String.get b64_chars ((c1 lsl 4) lor (c2 lsr 4) land 0x3F));
      Buffer.add_char buf (String.get b64_chars ((c2 lsl 2) land 0x3F));
      Buffer.add_char buf '=';
      encode_chunk (i + 2))
    else
      let c1 = Char.code (String.get s i) in

      Buffer.add_char buf (String.get b64_chars ((c1 lsr 2) land 0x3F));
      Buffer.add_char buf (String.get b64_chars ((c1 lsl 4) land 0x3F));
      Buffer.add_char buf '=';
      Buffer.add_char buf '=';
      encode_chunk (i + 1)
  in

  encode_chunk 0;
  Ok (Buffer.contents buf)

let base64_decode s =
  let s = String.trim s in
  let len = String.length s in
  let buf = Buffer.create ((len * 3 / 4) + 3) in

  let char_to_value c =
    match c with
    | 'A' .. 'Z' -> Char.code c - Char.code 'A'
    | 'a' .. 'z' -> Char.code c - Char.code 'a' + 26
    | '0' .. '9' -> Char.code c - Char.code '0' + 52
    | '+' -> 62
    | '/' -> 63
    | '=' -> -1
    | _ -> -2
  in

  let rec decode_chunk i =
    if i >= len then Ok ()
    else if i + 3 < len then
      let v1 = char_to_value (String.get s i) in
      let v2 = char_to_value (String.get s (i + 1)) in
      let v3 = char_to_value (String.get s (i + 2)) in
      let v4 = char_to_value (String.get s (i + 3)) in

      if v1 = -2 || v2 = -2 || v3 = -2 || v4 = -2 then
        Error "Invalid base64 character"
      else (
        if v1 >= 0 && v2 >= 0 then
          Buffer.add_char buf (Char.chr ((v1 lsl 2) lor (v2 lsr 4) land 0xFF));
        if v2 >= 0 && v3 >= 0 then
          Buffer.add_char buf (Char.chr ((v2 lsl 4) lor (v3 lsr 2) land 0xFF));
        if v3 >= 0 && v4 >= 0 then
          Buffer.add_char buf (Char.chr ((v3 lsl 6) lor v4 land 0xFF));
        decode_chunk (i + 4))
    else Ok ()
  in

  match decode_chunk 0 with
  | Ok () -> Ok (Buffer.contents buf)
  | Error e -> Error e

let quoted_printable_decode s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in

  let hex_to_int c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | _ -> -1
  in

  let rec decode i =
    if i >= len then Ok ()
    else
      let c = String.get s i in
      if c = '=' then
        if i + 2 < len then
          let h1 = hex_to_int (String.get s (i + 1)) in
          let h2 = hex_to_int (String.get s (i + 2)) in
          if h1 >= 0 && h2 >= 0 then (
            Buffer.add_char buf (Char.chr ((h1 lsl 4) lor h2));
            decode (i + 3))
          else if String.get s (i + 1) = '\n' || String.get s (i + 1) = '\r'
          then decode (i + 2) (* Soft line break *)
          else decode (i + 1)
        else Ok () (* Trailing = *)
      else (
        Buffer.add_char buf c;
        decode (i + 1))
  in

  match decode 0 with Ok () -> Ok (Buffer.contents buf) | Error e -> Error e

let decode enc s =
  match enc with
  | Base64 -> base64_decode s
  | QuotedPrintable -> quoted_printable_decode s
  | SevenBit | EightBit | Binary -> Ok s

let encode enc s =
  match enc with
  | Base64 -> base64_encode s
  | QuotedPrintable -> Error "Quoted-printable encoding not yet implemented"
  | SevenBit | EightBit | Binary -> Ok s
