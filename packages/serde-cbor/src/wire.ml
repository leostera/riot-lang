open Std
open Cbor_value
open Std.Result.Syntax

let major_positive = 0

let major_negative = 1

let major_text = 3

let major_array = 4

let major_map = 5

let major_tag = 6

let major_other = 7

let error = fun message -> Error (`Msg message)

let add_byte = fun buffer value -> IO.Buffer.add_char buffer (Char.from_int_unchecked value)

let add_uint16_be = fun buffer value ->
  add_byte buffer ((value lsr 8) land 0xff);
  add_byte buffer (value land 0xff)

let add_uint32_be = fun buffer value ->
  add_byte buffer (Int32.to_int (Int32.logand (Int32.shift_right_logical value 24) 0xffl));
  add_byte buffer (Int32.to_int (Int32.logand (Int32.shift_right_logical value 16) 0xffl));
  add_byte buffer (Int32.to_int (Int32.logand (Int32.shift_right_logical value 8) 0xffl));
  add_byte buffer (Int32.to_int (Int32.logand value 0xffl))

let add_uint64_be = fun buffer value ->
  let open Int64 in
  let add shift = add_byte buffer (to_int (logand (shift_right_logical value shift) 0xffL)) in
  add 56;
  add 48;
  add 40;
  add 32;
  add 24;
  add 16;
  add 8;
  add 0

let add_header = fun buffer major value ->
  if (
    match Int64.compare value 23L with
    | Order.LT
    | Order.EQ -> true
    | Order.GT -> false
  ) then
    add_byte buffer ((major lsl 5) lor Int64.to_int value)
  else if (
    match Int64.compare value 0xffL with
    | Order.LT
    | Order.EQ -> true
    | Order.GT -> false
  ) then (
    add_byte buffer ((major lsl 5) lor 24);
    add_byte buffer (Int64.to_int value)
  ) else if (
    match Int64.compare value 0xffffL with
    | Order.LT
    | Order.EQ -> true
    | Order.GT -> false
  ) then (
    add_byte buffer ((major lsl 5) lor 25);
    add_uint16_be buffer (Int64.to_int value)
  ) else if (
    match Int64.compare value 0xffff_ffffL with
    | Order.LT
    | Order.EQ -> true
    | Order.GT -> false
  ) then (
    add_byte buffer ((major lsl 5) lor 26);
    add_uint32_be buffer (Int64.to_int32 value)
  ) else (
    add_byte buffer ((major lsl 5) lor 27);
    add_uint64_be buffer value
  )

let add_float64 = fun buffer value ->
  add_byte buffer 0xfb;
  add_uint64_be buffer (Int64.bits_of_float value)

let add_text = fun buffer value ->
  add_header buffer major_text (Int64.of_int (String.length value));
  IO.Buffer.add_string buffer value

let rec encode_value = fun buffer value ->
  match value with
  | Null -> add_byte buffer 0xf6
  | Bool false -> add_byte buffer 0xf4
  | Bool true -> add_byte buffer 0xf5
  | Int value ->
      if (
        match Int64.compare value 0L with
        | Order.LT -> false
        | Order.EQ
        | Order.GT -> true
      ) then
        add_header buffer major_positive value
      else
        add_header buffer major_negative (Int64.lognot value)
  | Float value -> add_float64 buffer value
  | Text value -> add_text buffer value
  | Array values ->
      add_header buffer major_array (Int64.of_int (List.length values));
      List.for_each values ~fn:(encode_value buffer)
  | Map items ->
      add_header buffer major_map (Int64.of_int (List.length items));
      List.for_each
        items
        ~fn:(fun (key, value) ->
          add_text buffer key;
          encode_value buffer value)

let to_string = fun value ->
  let buffer = IO.Buffer.create ~size:128 in
  encode_value buffer value;
  Ok (IO.Buffer.contents buffer)

let to_writer = fun writer value ->
  let* encoded = to_string value in
  let buffer = IO.Buffer.from_string encoded in
  match IO.write_all writer ~from:buffer with
  | Ok () -> Ok ()
  | Error err -> Error (`Io_error err)

type input = {
  source: string;
  length: int;
  mutable pos: int;
}

let ensure = fun input needed ->
  if input.pos + needed > input.length then
    Error `no_more_data
  else
    Ok ()

let read_byte = fun input ->
  let* () = ensure input 1 in
  let value = Char.code (String.unsafe_get input.source input.pos) in
  input.pos <- input.pos + 1;
  Ok value

let read_uint16_be = fun input ->
  let* b0 = read_byte input in
  let* b1 = read_byte input in
  Ok ((b0 lsl 8) lor b1)

let read_uint32_be = fun input ->
  let* b0 = read_byte input in
  let* b1 = read_byte input in
  let* b2 = read_byte input in
  let* b3 = read_byte input in
  Ok Int64.(logor
    (shift_left (of_int b0) 24)
    (logor (shift_left (of_int b1) 16) (logor (shift_left (of_int b2) 8) (of_int b3))))

let read_uint64_be = fun input ->
  let open Int64 in
  let* b0 = read_byte input in
  let* b1 = read_byte input in
  let* b2 = read_byte input in
  let* b3 = read_byte input in
  let* b4 = read_byte input in
  let* b5 = read_byte input in
  let* b6 = read_byte input in
  let* b7 = read_byte input in
  Ok (logor
    (shift_left (of_int b0) 56)
    (logor
      (shift_left (of_int b1) 48)
      (logor
        (shift_left (of_int b2) 40)
        (logor
          (shift_left (of_int b3) 32)
          (logor
            (shift_left (of_int b4) 24)
            (logor (shift_left (of_int b5) 16) (logor (shift_left (of_int b6) 8) (of_int b7))))))))

let decode_half = fun bits ->
  let sign =
    if Int.equal (bits land 0x8000) 0 then
      1.0
    else
      (-1.0)
  in
  let exponent = (bits lsr 10) land 0x1f in
  let fraction = bits land 0x03ff in
  if Int.equal exponent 0 then
    if Int.equal fraction 0 then
      sign *. 0.0
    else
      sign *. (2. ** (-14.0)) *. (Float.of_int fraction /. 1_024.0)
  else if Int.equal exponent 0x1f then
    if Int.equal fraction 0 then
      sign *. Float.infinity
    else
      Float.nan
  else
    sign *. (2. ** Float.of_int (exponent - 15)) *. (1.0 +. (Float.of_int fraction /. 1_024.0))

let read_argument = fun input minor ->
  match minor with
  | n when n < 24 -> Ok (Int64.of_int n)
  | 24 ->
      read_byte input
      |> Result.map ~fn:Int64.of_int
  | 25 ->
      read_uint16_be input
      |> Result.map ~fn:Int64.of_int
  | 26 -> read_uint32_be input
  | 27 -> read_uint64_be input
  | 31 -> error "serde-cbor does not support indefinite-length items"
  | _ -> error "serde-cbor encountered an invalid additional-information value"

let read_count = fun input minor kind ->
  let* value = read_argument input minor in
  if (
    match Int64.compare value (Int64.of_int Int.max_int) with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) then
    error ("serde-cbor " ^ kind ^ " length does not fit in OCaml int")
  else
    Ok (Int64.to_int value)

let read_text = fun input minor ->
  let* length = read_count input minor "text" in
  let* () = ensure input length in
  let value = String.sub input.source ~offset:input.pos ~len:length in
  input.pos <- input.pos + length;
  Ok value

let int_of_positive = fun value ->
  if (
    match Int64.compare value Int64.max_int with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) then
    error "serde-cbor integer exceeds the supported signed range"
  else
    Ok value

let int_of_negative = fun raw ->
  if (
    match Int64.compare raw Int64.max_int with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) then
    error "serde-cbor negative integer exceeds the supported signed range"
  else
    Ok (Int64.lognot raw)

let rec read_value = fun input ->
  let* lead = read_byte input in
  let major = lead lsr 5 in
  let minor = lead land 0x1f in
  match major with
  | 0 ->
      let* value = read_argument input minor in
      let* value = int_of_positive value in
      Ok (Int value)
  | 1 ->
      let* raw = read_argument input minor in
      let* value = int_of_negative raw in
      Ok (Int value)
  | 3 ->
      read_text input minor
      |> Result.map ~fn:(fun value -> Text value)
  | 4 ->
      let* length = read_count input minor "array" in
      let rec loop remaining acc =
        if Int.equal remaining 0 then
          Ok (Array (List.rev acc))
        else
          let* value = read_value input in
          loop (remaining - 1) (value :: acc)
      in
      loop length []
  | 5 ->
      let* length = read_count input minor "map" in
      let rec loop remaining acc =
        if Int.equal remaining 0 then
          Ok (Map (List.rev acc))
        else
          let* key = read_value input in
          let* key =
            match key with
            | Text value -> Ok value
            | _ -> error "serde-cbor map keys must be text strings"
          in
          let* value = read_value input in
          loop (remaining - 1) ((key, value) :: acc)
      in
      loop length []
  | 6 ->
      let* _tag = read_argument input minor in
      read_value input
  | 7 -> (
      match minor with
      | 20 -> Ok (Bool false)
      | 21 -> Ok (Bool true)
      | 22 -> Ok Null
      | 25 ->
          read_uint16_be input
          |> Result.map ~fn:(fun bits -> Float (decode_half bits))
      | 26 ->
          let* bits = read_uint32_be input in
          Ok (Float (Int32.float_of_bits (Int64.to_int32 bits)))
      | 27 ->
          let* bits = read_uint64_be input in
          Ok (Float (Int64.float_of_bits bits))
      | _ -> error "serde-cbor encountered an unsupported simple value"
    )
  | _ -> error "serde-cbor encountered an invalid major type"

let from_string = fun input ->
  let state = { source = input; length = String.length input; pos = 0 } in
  let* value = read_value state in
  if Int.equal state.pos state.length then
    Ok value
  else
    error "serde-cbor input has trailing bytes after the top-level value"

let from_reader = fun reader ->
  let buffer = IO.Buffer.create ~size:256 in
  match IO.read_to_end reader ~into:buffer with
  | Ok _ -> from_string (IO.Buffer.contents buffer)
  | Error err -> Error (`Io_error err)
