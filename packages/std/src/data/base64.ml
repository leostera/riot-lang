open Global
open IO

let table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let encode_bytes = fun bytes ->
  let len = Bytes.length bytes in
  let result = Buffer.create ((len + 2) / 3 * 4) in
  let rec encode_block = fun i ->
    if i >= len then
      ()
    else if i + 2 < len then
      (
        let b1 = Char.code (Bytes.get bytes i) in
        let b2 = Char.code (Bytes.get bytes (i + 1)) in
        let b3 = Char.code (Bytes.get bytes (i + 2)) in
        Buffer.add_char result table.[b1 lsr 2];
        Buffer.add_char result table.[((b1 land 0x03) lsl 4) lor (b2 lsr 4)];
        Buffer.add_char result table.[((b2 land 0x0f) lsl 2) lor (b3 lsr 6)];
        Buffer.add_char result table.[b3 land 0x3f];
        encode_block (i + 3)
      )
    else if i + 1 < len then
      (
        let b1 = Char.code (Bytes.get bytes i) in
        let b2 = Char.code (Bytes.get bytes (i + 1)) in
        Buffer.add_char result table.[b1 lsr 2];
        Buffer.add_char result table.[((b1 land 0x03) lsl 4) lor (b2 lsr 4)];
        Buffer.add_char result table.[(b2 land 0x0f) lsl 2];
        Buffer.add_char result '=';
        encode_block (i + 3)
      )
    else
      let b1 = Char.code (Bytes.get bytes i) in
      Buffer.add_char result table.[b1 lsr 2];
      Buffer.add_char result table.[(b1 land 0x03) lsl 4];
      Buffer.add_char result '=';
      Buffer.add_char result '=';
      encode_block (i + 3)
  in
  encode_block 0;
  Buffer.contents result

let encode = fun str -> encode_bytes (Bytes.unsafe_of_string str)

let decode_char = fun c ->
  match c with
  | 'A' .. 'Z' -> Some (Char.code c - Char.code 'A')
  | 'a' .. 'z' -> Some (Char.code c - Char.code 'a' + 26)
  | '0' .. '9' -> Some (Char.code c - Char.code '0' + 52)
  | '+' -> Some 62
  | '/' -> Some 63
  | '=' -> Some 0
  | _ -> None

let decode = fun str ->
  let len = String.length str in
  if len mod 4 != 0 then
    Error `Invalid_base64
  else
    let result = Buffer.create (len / 4 * 3) in
    let rec decode_block = fun i ->
      if i >= len then
        Ok (Buffer.contents result)
      else
        match (
          decode_char str.[i],
          decode_char str.[i + 1],
          decode_char str.[i + 2],
          decode_char str.[i + 3]
        ) with
        | Some c1, Some c2, Some c3, Some c4 ->
            let b1 = (c1 lsl 2) lor (c2 lsr 4) in
            let b2 = ((c2 land 0x0f) lsl 4) lor (c3 lsr 2) in
            let b3 = ((c3 land 0x03) lsl 6) lor c4 in
            Buffer.add_char result (Char.chr b1);
            if str.[i + 2] != '=' then
              Buffer.add_char result (Char.chr b2);
            if str.[i + 3] != '=' then
              Buffer.add_char result (Char.chr b3);
            decode_block (i + 4)
        | _ -> Error `Invalid_base64
    in
    decode_block 0
