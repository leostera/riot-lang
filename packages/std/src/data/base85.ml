open Global
open Sync

let encode_bytes bytes =
  let len = Bytes.length bytes in
  let result = Buffer.create (((len * 5) + 3) / 4) in

  let rec encode_block i =
    if i >= len then ()
    else
      let remaining = len - i in
      let b0 = Char.code (Bytes.get bytes i) in
      let b1 =
        if remaining > 1 then Char.code (Bytes.get bytes (i + 1)) else 0
      in
      let b2 =
        if remaining > 2 then Char.code (Bytes.get bytes (i + 2)) else 0
      in
      let b3 =
        if remaining > 3 then Char.code (Bytes.get bytes (i + 3)) else 0
      in

      let value = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in

      if remaining >= 4 && value = 0 then Buffer.add_char result 'z'
      else
        let chars_to_output =
          if remaining = 1 then 2
          else if remaining = 2 then 3
          else if remaining = 3 then 4
          else 5
        in

        let c0 = value / 52200625 in
        let c1 = value / 614125 mod 85 in
        let c2 = value / 7225 mod 85 in
        let c3 = value / 85 mod 85 in
        let c4 = value mod 85 in

        if chars_to_output >= 1 then Buffer.add_char result (Char.chr (c0 + 33));
        if chars_to_output >= 2 then Buffer.add_char result (Char.chr (c1 + 33));
        if chars_to_output >= 3 then Buffer.add_char result (Char.chr (c2 + 33));
        if chars_to_output >= 4 then Buffer.add_char result (Char.chr (c3 + 33));
        if chars_to_output >= 5 then Buffer.add_char result (Char.chr (c4 + 33));

        encode_block (i + 4)
  in
  encode_block 0;
  Buffer.contents result

let encode str = encode_bytes (Bytes.unsafe_of_string str)

let strip_delimiters str =
  let str =
    if String.length str >= 2 && String.sub str 0 2 = "<~" then
      String.sub str 2 (String.length str - 2)
    else str
  in
  if String.length str >= 2 && String.sub str (String.length str - 2) 2 = "~>"
  then String.sub str 0 (String.length str - 2)
  else str

let decode_char c =
  let code = Char.code c in
  if code >= 33 && code <= 117 then Some (code - 33) else None

let decode_bytes str =
  let str = strip_delimiters str in
  let len = String.length str in
  let result = Buffer.create (len * 4 / 5) in
  let cursor = cell 0 in

  let rec decode_group () =
    if !cursor >= len then Ok (Buffer.contents result |> Bytes.unsafe_of_string)
    else
      let c = str.[!cursor] in
      if c = 'z' then (
        Buffer.add_char result '\x00';
        Buffer.add_char result '\x00';
        Buffer.add_char result '\x00';
        Buffer.add_char result '\x00';
        Cell.incr cursor;
        decode_group ())
      else if c = ' ' || c = '\n' || c = '\r' || c = '\t' then (
        Cell.incr cursor;
        decode_group ())
      else
        let start = !cursor in
        let chars = cell [] in
        let count = cell 0 in
        while !cursor < len && !count < 5 do
          let c = str.[!cursor] in
          if c <> ' ' && c <> '\n' && c <> '\r' && c <> '\t' then
            match decode_char c with
            | Some v ->
                chars := v :: !chars;
                Cell.incr count;
                Cell.incr cursor
            | None -> cursor := len
          else Cell.incr cursor
        done;

        if !count = 0 then Ok (Buffer.contents result |> Bytes.unsafe_of_string)
        else if !count = 1 then Error `Invalid_base85
        else
          let chars = List.rev !chars in
          let values =
            match chars with
            | [ c0; c1 ] -> [ c0; c1; 84; 84; 84 ]
            | [ c0; c1; c2 ] -> [ c0; c1; c2; 84; 84 ]
            | [ c0; c1; c2; c3 ] -> [ c0; c1; c2; c3; 84 ]
            | c0 :: c1 :: c2 :: c3 :: c4 :: _ -> [ c0; c1; c2; c3; c4 ]
            | _ -> []
          in

          let value =
            match values with
            | [ c0; c1; c2; c3; c4 ] ->
                (c0 * 52200625) + (c1 * 614125) + (c2 * 7225) + (c3 * 85) + c4
            | _ -> 0
          in

          let bytes_to_output =
            if !count = 2 then 1
            else if !count = 3 then 2
            else if !count = 4 then 3
            else 4
          in

          if bytes_to_output >= 1 then
            Buffer.add_char result (Char.chr ((value lsr 24) land 0xFF));
          if bytes_to_output >= 2 then
            Buffer.add_char result (Char.chr ((value lsr 16) land 0xFF));
          if bytes_to_output >= 3 then
            Buffer.add_char result (Char.chr ((value lsr 8) land 0xFF));
          if bytes_to_output >= 4 then
            Buffer.add_char result (Char.chr (value land 0xFF));

          decode_group ()
  in
  decode_group ()

let decode str =
  match decode_bytes str with
  | Ok bytes -> Ok (Bytes.unsafe_to_string bytes)
  | Error e -> Error e
