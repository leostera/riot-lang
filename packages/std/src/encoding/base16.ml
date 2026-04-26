open Global
open IO

let hex_upper = "0123456789ABCDEF"

let hex_lower = "0123456789abcdef"

let encode_bytes_with = fun table bytes ->
  let len = Bytes.length bytes in
  let result = Buffer.create ~size:(len * 2) in
  for i = 0 to len - 1 do
    let byte = Char.code (Bytes.get_unchecked bytes ~at:i) in
    Buffer.add_char result (String.get_unchecked table ~at:(byte lsr 4));
    Buffer.add_char result (String.get_unchecked table ~at:(byte land 0x0f))
  done;
  Buffer.contents result

let encode_bytes = fun bytes -> encode_bytes_with hex_upper bytes

let encode_bytes_lower = fun bytes -> encode_bytes_with hex_lower bytes

let encode = fun str -> encode_bytes (Bytes.from_string str)

let encode_lower = fun str -> encode_bytes_lower (Bytes.from_string str)

let decode_char = fun c ->
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | _ -> None

let decode_bytes = fun str ->
  let len = String.length str in
  if len mod 2 != 0 then
    Error `Invalid_base16
  else
    let result = Bytes.create ~size:(len / 2) in
    let rec decode_pair i =
      if i >= len then
        Ok result
      else
        match (
          decode_char (String.get_unchecked str ~at:i),
          decode_char (String.get_unchecked str ~at:(i + 1))
        ) with
        | (Some hi, Some lo) ->
            Bytes.set_unchecked result ~at:(i / 2) ~char:(Char.from_int_unchecked ((hi lsl 4) lor lo));
            decode_pair (i + 2)
        | _ -> Error `Invalid_base16
    in
    decode_pair 0

let decode = fun str ->
  match decode_bytes str with
  | Ok bytes -> Ok (Bytes.to_string bytes)
  | Error e -> Error e
