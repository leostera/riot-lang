open Global
open IO
open Sync

let table = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

let encode_bytes = fun bytes ->
  let len = Bytes.length bytes in
  let output_len = (len + 4) / 5 * 8 in
  let result = Buffer.create ~size:output_len in
  let rec encode_block i =
    if i >= len then
      ()
    else
      let remaining = len - i in
      let b0 = Char.code (Bytes.get_unchecked bytes ~at:i) in
      let b1 =
        if remaining > 1 then
          Char.code (Bytes.get_unchecked bytes ~at:(i + 1))
        else
          0
      in
      let b2 =
        if remaining > 2 then
          Char.code (Bytes.get_unchecked bytes ~at:(i + 2))
        else
          0
      in
      let b3 =
        if remaining > 3 then
          Char.code (Bytes.get_unchecked bytes ~at:(i + 3))
        else
          0
      in
      let b4 =
        if remaining > 4 then
          Char.code (Bytes.get_unchecked bytes ~at:(i + 4))
        else
          0
      in
      Buffer.add_char result (String.get_unchecked table ~at:(b0 lsr 3));
      Buffer.add_char result (String.get_unchecked table ~at:(((b0 land 0x07) lsl 2) lor (b1 lsr 6)));
      if remaining > 1 then
        Buffer.add_char result (String.get_unchecked table ~at:((b1 lsr 1) land 0x1f))
      else
        Buffer.add_char result '=';
      if remaining > 1 then
        Buffer.add_char
          result
          (String.get_unchecked table ~at:(((b1 land 0x01) lsl 4) lor (b2 lsr 4)))
      else
        Buffer.add_char result '=';
      if remaining > 2 then
        Buffer.add_char
          result
          (String.get_unchecked table ~at:(((b2 land 0x0f) lsl 1) lor (b3 lsr 7)))
      else
        Buffer.add_char result '=';
      if remaining > 3 then
        Buffer.add_char result (String.get_unchecked table ~at:((b3 lsr 2) land 0x1f))
      else
        Buffer.add_char result '=';
      if remaining > 3 then
        Buffer.add_char
          result
          (String.get_unchecked table ~at:(((b3 land 0x03) lsl 3) lor (b4 lsr 5)))
      else
        Buffer.add_char result '=';
      if remaining > 4 then
        Buffer.add_char result (String.get_unchecked table ~at:(b4 land 0x1f))
      else
        Buffer.add_char result '=';
      encode_block (i + 5)
  in
  encode_block 0;
  Buffer.contents result

let encode = fun str -> encode_bytes (String.to_bytes str)

let decode_char = fun c ->
  match c with
  | 'A' .. 'Z' -> Some (Char.code c - Char.code 'A')
  | 'a' .. 'z' -> Some (Char.code c - Char.code 'a')
  | '2' .. '7' -> Some (Char.code c - Char.code '2' + 26)
  | '=' -> Some 0
  | _ -> None

let set_result = fun result ->
  let _ = Result.unwrap result in
  ()

let decode_bytes: string -> (bytes, [
    `Invalid_base32
  ]) result = fun str ->
  let len = String.length str in
  if len mod 8 != 0 then
    Error `Invalid_base32
  else
    let output_len = len / 8 * 5 in
    let result = Bytes.create ~size:output_len in
    let output_pos = cell 0 in
    let rec decode_block i =
      if i >= len then
        (
          match Bytes.sub result ~offset:0 ~len:!output_pos with
          | Ok bytes -> Ok bytes
          | Error _ -> Error `Invalid_base32
        )
      else
        match (
          decode_char (String.get_unchecked str ~at:i),
          decode_char (String.get_unchecked str ~at:(i + 1)),
          decode_char (String.get_unchecked str ~at:(i + 2)),
          decode_char (String.get_unchecked str ~at:(i + 3)),
          decode_char (String.get_unchecked str ~at:(i + 4)),
          decode_char (String.get_unchecked str ~at:(i + 5)),
          decode_char (String.get_unchecked str ~at:(i + 6)),
          decode_char (String.get_unchecked str ~at:(i + 7))
        ) with
        | Some c0, Some c1, Some c2, Some c3, Some c4, Some c5, Some c6, Some c7 ->
            let b0 = (c0 lsl 3) lor (c1 lsr 2) in
            set_result (Bytes.set result ~at:!output_pos ~char:(Char.from_int_unchecked b0));
            Cell.incr output_pos;
            if String.get_unchecked str ~at:(i + 2) != '=' then
              (
                let b1 = ((c1 land 0x03) lsl 6) lor (c2 lsl 1) lor (c3 lsr 4) in
                set_result (Bytes.set result ~at:!output_pos ~char:(Char.from_int_unchecked b1));
                Cell.incr output_pos
              );
            if String.get_unchecked str ~at:(i + 4) != '=' then
              (
                let b2 = ((c3 land 0x0f) lsl 4) lor (c4 lsr 1) in
                set_result (Bytes.set result ~at:!output_pos ~char:(Char.from_int_unchecked b2));
                Cell.incr output_pos
              );
            if String.get_unchecked str ~at:(i + 5) != '=' then
              (
                let b3 = ((c4 land 0x01) lsl 7) lor (c5 lsl 2) lor (c6 lsr 3) in
                set_result (Bytes.set result ~at:!output_pos ~char:(Char.from_int_unchecked b3));
                Cell.incr output_pos
              );
            if String.get_unchecked str ~at:(i + 7) != '=' then
              (
                let b4 = ((c6 land 0x07) lsl 5) lor c7 in
                set_result (Bytes.set result ~at:!output_pos ~char:(Char.from_int_unchecked b4));
                Cell.incr output_pos
              );
            decode_block (i + 8)
        | _ -> Error `Invalid_base32
    in
    decode_block 0

let decode = fun str ->
  match decode_bytes str with
  | Ok bytes -> Ok (Bytes.to_string bytes)
  | Error e -> Error e
