open Global
open Sync
open IO
open Collections

type decode_error =
  | InvalidBase85

let encode_bytes = fun bytes ->
  let len = Bytes.length bytes in
  let result = Buffer.create ~size:(((len * 5) + 3) / 4) in
  let rec encode_block i =
    if i >= len then
      ()
    else
      let remaining = len - i in
      let b0 = Char.to_int (Bytes.get_unchecked bytes ~at:i) in
      let b1 =
        if remaining > 1 then
          Char.to_int (Bytes.get_unchecked bytes ~at:(i + 1))
        else
          0
      in
      let b2 =
        if remaining > 2 then
          Char.to_int (Bytes.get_unchecked bytes ~at:(i + 2))
        else
          0
      in
      let b3 =
        if remaining > 3 then
          Char.to_int (Bytes.get_unchecked bytes ~at:(i + 3))
        else
          0
      in
      let value = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
      if remaining >= 4 && value = 0 then
        Buffer.add_char result 'z'
      else
        let chars_to_output =
          if remaining = 1 then
            2
          else if remaining = 2 then
            3
          else if remaining = 3 then
            4
          else
            5
        in
        let c0 = value / 52_200_625 in
        let c1 = value / 614_125 mod 85 in
        let c2 = value / 7_225 mod 85 in
        let c3 = value / 85 mod 85 in
        let c4 = value mod 85 in
        if chars_to_output >= 1 then
          Buffer.add_char result (Char.from_int_unchecked (c0 + 33));
    if chars_to_output >= 2 then
      Buffer.add_char result (Char.from_int_unchecked (c1 + 33));
    if chars_to_output >= 3 then
      Buffer.add_char result (Char.from_int_unchecked (c2 + 33));
    if chars_to_output >= 4 then
      Buffer.add_char result (Char.from_int_unchecked (c3 + 33));
    if chars_to_output >= 5 then
      Buffer.add_char result (Char.from_int_unchecked (c4 + 33));
    encode_block (i + 4)
  in
  encode_block 0;
  Buffer.contents result

let encode = fun str -> encode_bytes (Bytes.from_string str)

let strip_delimiters = fun str ->
  let str =
    let len = String.length str in
    if len >= 2 then
      if String.sub str ~offset:0 ~len:2 = "<~" then
        String.sub str ~offset:2 ~len:(len - 2)
      else
        str
    else
      str
  in
  let len = String.length str in
  if len >= 2 then
    if String.sub str ~offset:(len - 2) ~len:2 = "~>" then
      String.sub str ~offset:0 ~len:(len - 2)
    else
      str
  else
    str

let decode_char = fun c ->
  let code = Char.to_int c in
  if code >= 33 && code <= 117 then
    Some (code - 33)
  else
    None

let decode_bytes = fun str ->
  let exception Invalid_base85_character in
  let str = strip_delimiters str in
  let len = String.length str in
  let result = Buffer.create ~size:(len * 4 / 5) in
  let cursor = cell 0 in
  let rec decode_group () =
    if !cursor >= len then
      Ok (
        Buffer.contents result
        |> Bytes.from_string
      )
    else
      let c = String.get_unchecked str ~at:!cursor in
      if c = 'z' then (
        Buffer.add_char result '\x00';
        Buffer.add_char result '\x00';
        Buffer.add_char result '\x00';
        Buffer.add_char result '\x00';
        Cell.incr cursor;
        decode_group ()
      ) else if c = ' ' || c = '\n' || c = '\r' || c = '\t' then (
        Cell.incr cursor;
        decode_group ()
      ) else
        let chars = cell [] in
        let count = cell 0 in
        while !cursor < len && !count < 5 do
          let c = String.get_unchecked str ~at:!cursor in
          if c != ' ' && c != '\n' && c != '\r' && c != '\t' then
            match decode_char c with
            | Some v ->
                chars := v :: !chars;
                Cell.incr count;
                Cell.incr cursor
            | None -> raise Invalid_base85_character
          else
            Cell.incr cursor
        done;
    if !count = 0 then
      Ok (
        Buffer.contents result
        |> Bytes.from_string
      )
    else if !count = 1 then
      Error InvalidBase85
    else
      let chars = List.reverse !chars in
      let values =
        match chars with
        | [ c0; c1 ] -> [ c0; c1; 84; 84; 84; ]
        | [ c0; c1; c2 ] -> [ c0; c1; c2; 84; 84; ]
        | [ c0; c1; c2; c3 ] -> [ c0; c1; c2; c3; 84; ]
        | c0 :: c1 :: c2 :: c3 :: c4 :: _ -> [ c0; c1; c2; c3; c4; ]
        | _ -> []
      in
      let value =
        match values with
        | [ c0; c1; c2; c3; c4 ] -> (c0 * 52_200_625) + (c1 * 614_125) + (c2 * 7_225) + (c3 * 85) + c4
        | _ -> 0
      in
      let bytes_to_output =
        if !count = 2 then
          1
        else if !count = 3 then
          2
        else if !count = 4 then
          3
        else
          4
      in
      if bytes_to_output >= 1 then
        Buffer.add_char result (Char.from_int_unchecked ((value lsr 24) land 0xff));
    if bytes_to_output >= 2 then
      Buffer.add_char result (Char.from_int_unchecked ((value lsr 16) land 0xff));
    if bytes_to_output >= 3 then
      Buffer.add_char result (Char.from_int_unchecked ((value lsr 8) land 0xff));
    if bytes_to_output >= 4 then
      Buffer.add_char result (Char.from_int_unchecked (value land 0xff));
    decode_group ()
  in
  try decode_group () with
  | Invalid_base85_character -> Error InvalidBase85

let decode = fun str ->
  match decode_bytes str with
  | Ok bytes -> Ok (Bytes.to_string bytes)
  | Error e -> Error e
