open Std

type read_result = [`Ok of int | `Would_block | `Error]

type t = { data: bytes; mutable pos: int; mutable len: int }

type result = [`Retry | `End | `Malformed of string | `Read of string]

let create = fun () -> { data = IO.Bytes.create ~size:4; pos = 0; len = 0 }

let clear = fun reader ->
  reader.pos <- 0;
  reader.len <- 0

let utf8_char_length = fun first_byte ->
  if first_byte land 0x80 = 0 then
    1
  else
    if first_byte land 0xe0 = 0xc0 then
      2
    else
      if first_byte land 0xf0 = 0xe0 then
        3
      else
        if first_byte land 0xf8 = 0xf0 then
          4
        else 0

let is_valid_buffer = fun reader ->
  let value = IO.Bytes.sub_unchecked reader.data ~offset:0 ~len:reader.len |> IO.Bytes.to_string in
  match Unicode.Utf8.decode_rune value 0 with
  | Some (_rune, next_pos) -> Int.equal next_pos reader.len
  | None -> false

let read_more = fun reader ~read ->
  let remaining = reader.len - reader.pos in
  match read reader.data ~offset:reader.pos ~len:remaining with
  | `Ok 0 ->
      clear reader;
      `Malformed "Incomplete UTF-8 sequence"
  | `Ok count ->
      reader.pos <- reader.pos + count;
      `Retry
  | `Would_block -> `Retry
  | `Error ->
      clear reader;
      `Malformed "Read error"

let read = fun reader ~read ->
  if reader.pos = 0 then
    match read reader.data ~offset:0 ~len:1 with
    | `Ok 0 -> `End
    | `Ok 1 ->
        let first_byte = Char.code (IO.Bytes.get_unchecked reader.data ~at:0) in
        let len = utf8_char_length first_byte in
        if len = 0 then
          `Malformed "Invalid UTF-8 start byte"
        else
          if len = 1 then
            `Read (IO.Bytes.sub_unchecked reader.data ~offset:0 ~len:1 |> IO.Bytes.to_string)
          else
            (
              reader.pos <- 1;
              reader.len <- len;
              match read_more reader ~read with
              | `Retry when Int.equal reader.pos reader.len ->
                  if is_valid_buffer reader then
                    let value = IO.Bytes.sub_unchecked reader.data ~offset:0 ~len:reader.len |> IO.Bytes.to_string in clear reader;
                    `Read value
                  else
                    (
                      clear reader;
                      `Malformed "Invalid UTF-8 sequence"
                    )
              | result -> result
            )
    | `Ok _ -> `Malformed "Unexpected read length"
    | `Would_block -> `Retry
    | `Error -> `End
  else
    match read_more reader ~read with
    | `Retry when Int.equal reader.pos reader.len ->
        if is_valid_buffer reader then
          let value = IO.Bytes.sub_unchecked reader.data ~offset:0 ~len:reader.len |> IO.Bytes.to_string in clear reader;
          `Read value
        else
          (
            clear reader;
            `Malformed "Invalid UTF-8 sequence"
          )
    | result -> result
