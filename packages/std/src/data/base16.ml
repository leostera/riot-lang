let hex_upper = "0123456789ABCDEF"
let hex_lower = "0123456789abcdef"

let encode_bytes_with table bytes =
  let len = Bytes.length bytes in
  let result = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    let byte = Char.code (Bytes.get bytes i) in
    Buffer.add_char result table.[byte lsr 4];
    Buffer.add_char result table.[byte land 0x0F]
  done;
  Buffer.contents result

let encode_bytes bytes = encode_bytes_with hex_upper bytes
let encode_bytes_lower bytes = encode_bytes_with hex_lower bytes

let encode str = encode_bytes (Bytes.unsafe_of_string str)
let encode_lower str = encode_bytes_lower (Bytes.unsafe_of_string str)

let decode_char c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | _ -> None

let decode_bytes str =
  let len = String.length str in
  if len mod 2 <> 0 then Error `Invalid_base16
  else
    let result = Bytes.create (len / 2) in
    let rec decode_pair i =
      if i >= len then Ok result
      else
        match (decode_char str.[i], decode_char str.[i + 1]) with
        | Some hi, Some lo ->
            Bytes.set result (i / 2) (Char.chr ((hi lsl 4) lor lo));
            decode_pair (i + 2)
        | _ -> Error `Invalid_base16
    in
    decode_pair 0

let decode str =
  match decode_bytes str with
  | Ok bytes -> Ok (Bytes.unsafe_to_string bytes)
  | Error e -> Error e
