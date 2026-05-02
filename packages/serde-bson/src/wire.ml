open Std
open Bson_value
open Std.Result.Syntax

let type_double = Char.from_int_unchecked 0x01

let type_string = Char.from_int_unchecked 0x02

let type_document = Char.from_int_unchecked 0x03

let type_array = Char.from_int_unchecked 0x04

let type_bool = Char.from_int_unchecked 0x08

let type_null = Char.from_int_unchecked 0x0a

let type_int32 = Char.from_int_unchecked 0x10

let type_int64 = Char.from_int_unchecked 0x12

let error = fun message -> Error (`Msg message)

let hex_char = fun __tmp1 ->
  match __tmp1 with
  | value when value < 10 -> Char.from_int_unchecked (Char.code '0' + value)
  | value -> Char.from_int_unchecked (Char.code 'A' + value - 10)

let hex_byte = fun value ->
  String.init
    ~len:2
    ~fn:(fun index ->
      if Int.equal index 0 then
        hex_char ((value lsr 4) land 0x0f)
      else
        hex_char (value land 0x0f))

let length_error = fun kind -> error ("serde-bson " ^ kind ^ " length exceeds BSON's int32 limit")

let int_of_length_int32 = fun value ->
  if (
    match Int32.compare value 5l with
    | Order.LT -> true
    | Order.EQ
    | Order.GT -> false
  ) then
    Error (`Msg "serde-bson document length is too small")
  else
    try Ok (Int32.to_int value) with
    | _ -> Error (`Msg "serde-bson document length does not fit in OCaml int")

let int_of_positive_int32 = fun kind value minimum ->
  if (
    match Int32.compare value minimum with
    | Order.LT -> true
    | Order.EQ
    | Order.GT -> false
  ) then
    Error (`Msg ("serde-bson " ^ kind ^ " length is too small"))
  else
    try Ok (Int32.to_int value) with
    | _ -> Error (`Msg ("serde-bson " ^ kind ^ " length does not fit in OCaml int"))

let int32_of_length = fun kind value ->
  if value > Int32.to_int Int32.max_int then
    length_error kind
  else
    Ok (Int32.of_int value)

let add_int32_le = fun buffer value ->
  let open Int32 in
  let add_byte shift =
    IO.Buffer.add_char
      buffer
      (Char.from_int_unchecked (to_int (logand (shift_right_logical value shift) 0xffl)))
  in
  add_byte 0;
  add_byte 8;
  add_byte 16;
  add_byte 24

let add_int64_le = fun buffer value ->
  let open Int64 in
  let add_byte shift =
    IO.Buffer.add_char
      buffer
      (Char.from_int_unchecked (to_int (logand (shift_right_logical value shift) 0xffL)))
  in
  add_byte 0;
  add_byte 8;
  add_byte 16;
  add_byte 24;
  add_byte 32;
  add_byte 40;
  add_byte 48;
  add_byte 56

let add_double_le = fun buffer value -> add_int64_le buffer (Int64.bits_of_float value)

let add_cstring = fun buffer value ->
  let has_nul = ref false in
  String.iter
    (fun char_ ->
      if Char.equal char_ '\x00' then
        has_nul := true)
    value;
  if !has_nul then
    Error (`Msg "serde-bson field names cannot contain NUL bytes")
  else (
    IO.Buffer.add_string buffer value;
    IO.Buffer.add_char buffer '\x00';
    Ok ()
  )

let rec encode_value = fun __tmp1 ->
  match __tmp1 with
  | Null -> Ok (type_null, "")
  | Bool value -> Ok (type_bool, if value then
    "\x01"
  else
    "\x00")
  | Int32 value ->
      let buffer = IO.Buffer.create ~size:4 in
      add_int32_le buffer value;
      Ok (type_int32, IO.Buffer.contents buffer)
  | Int64 value ->
      let buffer = IO.Buffer.create ~size:8 in
      add_int64_le buffer value;
      Ok (type_int64, IO.Buffer.contents buffer)
  | Double value ->
      let buffer = IO.Buffer.create ~size:8 in
      add_double_le buffer value;
      Ok (type_double, IO.Buffer.contents buffer)
  | String value ->
      let* encoded_length = int32_of_length "string" (String.length value + 1) in
      let buffer = IO.Buffer.create ~size:(String.length value + 5) in
      add_int32_le buffer encoded_length;
      IO.Buffer.add_string buffer value;
      IO.Buffer.add_char buffer '\x00';
      Ok (type_string, IO.Buffer.contents buffer)
  | Document fields ->
      let* encoded = encode_document fields in
      Ok (type_document, encoded)
  | Array values ->
      let rec fields_of_values index values acc =
        match values with
        | [] -> List.rev acc
        | value :: rest -> fields_of_values (index + 1) rest ((Int.to_string index, value) :: acc)
      in
      let* encoded = encode_document (fields_of_values 0 values []) in
      Ok (type_array, encoded)

and encode_element = fun (key, value) ->
  let* (kind, payload) = encode_value value in
  let buffer = IO.Buffer.create ~size:(String.length key + String.length payload + 8) in
  IO.Buffer.add_char buffer kind;
  let* () = add_cstring buffer key in
  IO.Buffer.add_string buffer payload;
  Ok (IO.Buffer.contents buffer)

and encode_document = fun fields ->
  let* encoded_fields =
    List.fold_left
      fields
      ~init:(Ok [])
      ~fn:(fun acc field ->
        let* acc = acc in
        let* encoded = encode_element field in
        Ok (encoded :: acc))
  in
  let encoded_fields = List.rev encoded_fields in
  let payload_len =
    List.fold_left encoded_fields ~init:0 ~fn:(fun total field -> total + String.length field)
  in
  let* encoded_length = int32_of_length "document" (payload_len + 5) in
  let buffer = IO.Buffer.create ~size:(payload_len + 5) in
  add_int32_le buffer encoded_length;
  List.for_each encoded_fields ~fn:(IO.Buffer.add_string buffer);
  IO.Buffer.add_char buffer '\x00';
  Ok (IO.Buffer.contents buffer)

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

let read_int32_le = fun input ->
  let* b0 = read_byte input in
  let* b1 = read_byte input in
  let* b2 = read_byte input in
  let* b3 = read_byte input in
  Ok Int32.(logor
    (of_int b0)
    (logor
      (shift_left (of_int b1) 8)
      (logor (shift_left (of_int b2) 16) (shift_left (of_int b3) 24))))

let read_int64_le = fun input ->
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
    (of_int b0)
    (logor
      (shift_left (of_int b1) 8)
      (logor
        (shift_left (of_int b2) 16)
        (logor
          (shift_left (of_int b3) 24)
          (logor
            (shift_left (of_int b4) 32)
            (logor
              (shift_left (of_int b5) 40)
              (logor (shift_left (of_int b6) 48) (shift_left (of_int b7) 56))))))))

let read_double_le = fun input ->
  let* bits = read_int64_le input in
  Ok (Int64.float_of_bits bits)

let read_cstring = fun input ->
  let start = input.pos in
  let rec loop index =
    if index >= input.length then
      Error `no_more_data
    else if Char.equal (String.unsafe_get input.source index) '\x00' then (
      let value = String.sub input.source ~offset:start ~len:(index - start) in
      input.pos <- index + 1;
      Ok value
    ) else
      loop (index + 1)
  in
  loop input.pos

let read_length_prefixed_string = fun input ->
  let* length = read_int32_le input in
  let* length = int_of_positive_int32 "string" length 1l in
  let* () = ensure input length in
  let text_len = length - 1 in
  let terminator_index = input.pos + text_len in
  if not (Char.equal (String.unsafe_get input.source terminator_index) '\x00') then
    error "serde-bson string payload is missing its terminating NUL"
  else
    let value = String.sub input.source ~offset:input.pos ~len:text_len in
    input.pos <- input.pos + length;
  Ok value

let array_of_document = fun fields ->
  let rec loop index fields acc =
    match fields with
    | [] -> Ok (List.rev acc)
    | (key, value) :: rest ->
        if String.equal key (Int.to_string index) then
          loop (index + 1) rest (value :: acc)
        else
          error ("serde-bson array keys must be sequential numeric strings, got '" ^ key ^ "'")
  in
  loop 0 fields []

let rec read_value = fun input kind end_pos ->
  match kind with
  | 0x01 ->
      read_double_le input
      |> Result.map ~fn:(fun value -> Double value)
  | 0x02 ->
      read_length_prefixed_string input
      |> Result.map ~fn:(fun value -> String value)
  | 0x03 ->
      read_document_body input end_pos
      |> Result.map ~fn:(fun value -> Document value)
  | 0x04 ->
      let* fields = read_document_body input end_pos in
      let* values = array_of_document fields in
      Ok (Array values)
  | 0x08 ->
      let* value = read_byte input in
      if Int.equal value 0 then
        Ok (Bool false)
      else if Int.equal value 1 then
        Ok (Bool true)
      else
        error "serde-bson bool payload must be 0 or 1"
  | 0x0a -> Ok Null
  | 0x10 ->
      read_int32_le input
      |> Result.map ~fn:(fun value -> Int32 value)
  | 0x12 ->
      read_int64_le input
      |> Result.map ~fn:(fun value -> Int64 value)
  | _ -> error ("serde-bson encountered unsupported BSON element type 0x" ^ hex_byte kind)

and read_document_body = fun input _parent_end ->
  let start = input.pos in
  let* declared_length = read_int32_le input in
  let* declared_length = int_of_length_int32 declared_length in
  let end_pos = start + declared_length in
  if end_pos > input.length then
    Error `no_more_data
  else
    let rec loop acc =
      if Int.equal input.pos (end_pos - 1) then
        let* terminator = read_byte input in
        if Int.equal terminator 0 then
          Ok (List.rev acc)
        else
          error "serde-bson document terminator must be 0"
      else if input.pos > end_pos - 1 then
        error "serde-bson document overran its declared length"
      else
        let* kind = read_byte input in
        if Int.equal kind 0 then
          error "serde-bson document ended before its declared length"
        else
          let* key = read_cstring input in
          let* value = read_value input kind end_pos in
          loop ((key, value) :: acc)
    in
    loop []

let to_string = fun __tmp1 ->
  match __tmp1 with
  | Document fields -> encode_document fields
  | _ -> error "serde-bson top-level value must be a document"

let to_writer = fun writer value ->
  let* encoded = to_string value in
  let buffer = IO.Buffer.from_string encoded in
  match IO.write_all writer ~from:buffer with
  | Ok () -> Ok ()
  | Error err -> Error (`Io_error err)

let from_string = fun input ->
  let state = { source = input; length = String.length input; pos = 0 } in
  let* fields = read_document_body state state.length in
  if Int.equal state.pos state.length then
    Ok (Document fields)
  else
    error "serde-bson input has trailing bytes after the top-level document"

let from_reader = fun reader ->
  let buffer = IO.Buffer.create ~size:256 in
  match IO.read_to_end reader ~into:buffer with
  | Ok _ -> from_string (IO.Buffer.contents buffer)
  | Error err -> Error (`Io_error err)
