open Std

type read_result =
[
  `Ok of int
  | `Would_block
  | `Error
]

type result =
[
  `Retry
  | `End
  | `Malformed of string
  | `Read of string
]

let utf8_char_length = fun first_byte ->
  if first_byte land 0x80 = 0 then
    1
  else if first_byte land 0xe0 = 0xc0 then
    2
  else if first_byte land 0xf0 = 0xe0 then
    3
  else if first_byte land 0xf8 = 0xf0 then
    4
  else
    0

let read = fun ~read ->
  let bytes = IO.Bytes.create ~size:4 in
  match read bytes ~offset:0 ~len:1 with
  | `Ok 0 ->
      `End
  | `Ok 1 ->
      let first_byte = Char.code (IO.Bytes.get_unchecked bytes ~at:0) in
      let len = utf8_char_length first_byte in
      if len = 0 then
        `Malformed "Invalid UTF-8 start byte"
      else if len = 1 then
        `Read (IO.Bytes.sub_unchecked bytes ~offset:0 ~len:1 |> IO.Bytes.to_string)
      else
        (
          match read bytes ~offset:1 ~len:(len - 1) with
          | `Ok count when count = len - 1 ->
              `Read (IO.Bytes.sub_unchecked bytes ~offset:0 ~len |> IO.Bytes.to_string)
          | `Ok _ ->
              `Malformed "Incomplete UTF-8 sequence"
          | `Would_block ->
              `Retry
          | `Error ->
              `Malformed "Read error"
        )
  | `Ok _ ->
      `Malformed "Unexpected read length"
  | `Would_block ->
      `Retry
  | `Error ->
      `End
