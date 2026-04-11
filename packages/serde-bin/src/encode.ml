open Std
module Vector = Collections.Vector

type encode_target =
  | Buffer_target
  | Bytes_target of { dst: bytes; mutable pos: int }
  | Writer_target: ('dst, IO.error) IO.Writer.t -> encode_target

type state = {
  target: encode_target;
  output: IO.Buffer.t;
  scratch: bytes;
}

type size_state = {
  mutable bytes_written: int;
}

let flush_threshold = 4_096

let raise_io_error = fun err -> raise (Serde.Encode_error (`Io_error err))

let raise_no_space = fun () ->
  raise (Serde.Encode_error (`Msg "serde-bin destination buffer is too small"))

let raise_length_out_of_range = fun kind value ->
  raise
    (Serde.Encode_error (`Msg ("serde-bin " ^ kind ^ " length is out of range: " ^ Int.to_string value)))

let int_is_nonzero = fun value -> not (Int.equal value 0)

let int_lt = fun left right ->
  let ordering = Int.compare left right in
  int_is_nonzero ordering && Int.equal (Int.min ordering 0) ordering

let int_gt = fun left right ->
  let ordering = Int.compare left right in
  int_is_nonzero ordering && Int.equal (Int.max ordering 0) ordering

let int_le = fun left right -> not (int_gt left right)

let flush_output = fun state ->
  match state.target with
  | Buffer_target
  | Bytes_target _ -> ()
  | Writer_target writer ->
      if int_is_nonzero (IO.Buffer.length state.output) then
        match IO.write_all writer ~buf:(IO.Buffer.contents state.output) with
        | Ok () -> IO.Buffer.clear state.output
        | Error err -> raise_io_error err

let maybe_flush_output = fun state ->
  match state.target with
  | Writer_target _ when IO.Buffer.length state.output >= flush_threshold -> flush_output state
  | _ -> ()

let write_subbytes = fun state source ~off ~len ->
  if int_is_nonzero len then
    match state.target with
    | Buffer_target
    | Writer_target _ ->
        IO.Buffer.add_subbytes state.output source off len;
        maybe_flush_output state
    | Bytes_target target ->
        if int_gt (target.pos + len) (IO.Bytes.length target.dst) then
          raise_no_space ();
        IO.Bytes.blit source off target.dst target.pos len;
        target.pos <- target.pos + len

let write_char = fun state value ->
  match state.target with
  | Bytes_target target ->
      if int_gt (target.pos + 1) (IO.Bytes.length target.dst) then
        raise_no_space ();
      IO.Bytes.set target.dst target.pos value;
      target.pos <- target.pos + 1
  | Buffer_target
  | Writer_target _ ->
      IO.Bytes.set state.scratch 0 value;
      write_subbytes state state.scratch ~off:0 ~len:1

let write_string = fun state value ->
  let len = String.length value in
  if int_is_nonzero len then
    match state.target with
    | Buffer_target
    | Writer_target _ ->
        IO.Buffer.add_string state.output value;
        maybe_flush_output state
    | Bytes_target target ->
        if int_gt (target.pos + len) (IO.Bytes.length target.dst) then
          raise_no_space ();
        IO.Bytes.blit_string value 0 target.dst target.pos len;
        target.pos <- target.pos + len

let write_uint32_le = fun state value ->
  match state.target with
  | Bytes_target target ->
      if int_gt (target.pos + 4) (IO.Bytes.length target.dst) then
        raise_no_space ();
      Stubs.write_uint32_le target.dst target.pos value;
      target.pos <- target.pos + 4
  | Buffer_target
  | Writer_target _ ->
      Stubs.write_uint32_le state.scratch 0 value;
      write_subbytes state state.scratch ~off:0 ~len:4

let write_int32_le = fun state value ->
  match state.target with
  | Bytes_target target ->
      if int_gt (target.pos + 4) (IO.Bytes.length target.dst) then
        raise_no_space ();
      Stubs.write_int32_le target.dst target.pos value;
      target.pos <- target.pos + 4
  | Buffer_target
  | Writer_target _ ->
      Stubs.write_int32_le state.scratch 0 value;
      write_subbytes state state.scratch ~off:0 ~len:4

let write_int64_le = fun state value ->
  match state.target with
  | Bytes_target target ->
      if int_gt (target.pos + 8) (IO.Bytes.length target.dst) then
        raise_no_space ();
      Stubs.write_int64_le target.dst target.pos value;
      target.pos <- target.pos + 8
  | Buffer_target
  | Writer_target _ ->
      Stubs.write_int64_le state.scratch 0 value;
      write_subbytes state state.scratch ~off:0 ~len:8

let write_int_le = fun state value ->
  match state.target with
  | Bytes_target target ->
      if int_gt (target.pos + 8) (IO.Bytes.length target.dst) then
        raise_no_space ();
      Stubs.write_int_le target.dst target.pos value;
      target.pos <- target.pos + 8
  | Buffer_target
  | Writer_target _ ->
      Stubs.write_int_le state.scratch 0 value;
      write_subbytes state state.scratch ~off:0 ~len:8

let encode_u32 = fun kind value ->
  if int_lt value 0 then
    raise_length_out_of_range kind value
  else
    let value64 = Int64.of_int value in
    if int_gt (Int64.unsigned_compare value64 0xffff_ffffL) 0 then
      raise_length_out_of_range kind value
    else
      Int64.to_int value64

let variant_uses_u8 = fun cases -> int_le (array__length cases) 0x100

let rec list_backend: 'value. state -> 'value Serde.Ser.t -> 'value vec -> unit = fun state encode values ->
  write_uint32_le state (encode_u32 "list" (Vector.len values));
  Vector.iter
    (fun value ->
      encode.run backend state value)
    values

and array_backend: 'value. state -> 'value Serde.Ser.t -> 'value array -> unit = fun state encode values ->
  let len = array__length values in
  write_uint32_le state (encode_u32 "array" len);
  for index = 0 to len - 1 do
    encode.run backend state (array__get values index)
  done

and record_backend: 'value. state -> 'value Serde.Ser.fields -> 'value -> unit = fun state fields value ->
  for index = 0 to array__length fields - 1 do
    match array__get fields index with
    | Serde.Ser.Field (_name, encode, get) -> encode.run backend state (get value)
  done

and variant_backend: 'value. state -> 'value Serde.Ser.variant_cases -> 'value -> unit = fun state cases value ->
  let compact_tag = variant_uses_u8 cases in
  let rec loop index =
    if Int.equal index (array__length cases) then
      raise (Serde.Encode_error `invalid_tag)
    else
      match array__get cases index with
      | Serde.Ser.Unit (_tag, matches) ->
          if matches value then
            if compact_tag then
              write_char state (Char.chr index)
            else
              write_uint32_le state (encode_u32 "variant" index)
          else
            loop (index + 1)
      | Serde.Ser.Newtype (_tag, encode, unwrap) -> (
          match unwrap value with
          | Some payload ->
              if compact_tag then
                write_char state (Char.chr index)
              else
                write_uint32_le state (encode_u32 "variant" index);
              encode.run backend state payload
          | None -> loop (index + 1)
        )
  in
  loop 0

and backend: state Serde.Ser.backend = {
  bool =
    (fun state value ->
      if value then
        write_char state '\001'
      else
        write_char state '\000');
  string =
    (fun state value ->
      write_uint32_le state (encode_u32 "string" (String.length value));
      write_string state value);
  int = write_int_le;
  int32 = write_int32_le;
  int64 = write_int64_le;
  float = (fun state value -> write_int64_le state (Int64.bits_of_float value));
  null = (fun _state -> ());
  option =
    (fun state encode value ->
      match value with
      | None -> write_char state '\000'
      | Some payload ->
          write_char state '\001';
          encode.run backend state payload);
  list = list_backend;
  array = array_backend;
  record = record_backend;
  variant = variant_backend;
}

let rec size_list_backend: 'value. size_state -> 'value Serde.Ser.t -> 'value vec -> unit = fun state encode values ->
  state.bytes_written <- state.bytes_written + 4;
  Vector.iter
    (fun value ->
      encode.run size_backend state value)
    values

and size_array_backend: 'value. size_state -> 'value Serde.Ser.t -> 'value array -> unit = fun state encode values ->
  state.bytes_written <- state.bytes_written + 4;
  for index = 0 to array__length values - 1 do
    encode.run size_backend state (array__get values index)
  done

and size_record_backend: 'value. size_state -> 'value Serde.Ser.fields -> 'value -> unit = fun state fields value ->
  for index = 0 to array__length fields - 1 do
    match array__get fields index with
    | Serde.Ser.Field (_name, encode, get) -> encode.run size_backend state (get value)
  done

and size_variant_backend: 'value. size_state -> 'value Serde.Ser.variant_cases -> 'value -> unit = fun state cases value ->
  let tag_size =
    if variant_uses_u8 cases then
      1
    else
      4
  in
  let rec loop index =
    if Int.equal index (array__length cases) then
      raise (Serde.Encode_error `invalid_tag)
    else
      match array__get cases index with
      | Serde.Ser.Unit (_tag, matches) ->
          if matches value then
            state.bytes_written <- state.bytes_written + tag_size
          else
            loop (index + 1)
      | Serde.Ser.Newtype (_tag, encode, unwrap) -> (
          match unwrap value with
          | Some payload ->
              state.bytes_written <- state.bytes_written + tag_size;
              encode.run size_backend state payload
          | None -> loop (index + 1)
        )
  in
  loop 0

and size_backend: size_state Serde.Ser.backend = {
  bool = (fun state _value -> state.bytes_written <- state.bytes_written + 1);
  string = (fun state value -> state.bytes_written <- state.bytes_written + 4 + String.length value);
  int = (fun state _value -> state.bytes_written <- state.bytes_written + 8);
  int32 = (fun state _value -> state.bytes_written <- state.bytes_written + 4);
  int64 = (fun state _value -> state.bytes_written <- state.bytes_written + 8);
  float = (fun state _value -> state.bytes_written <- state.bytes_written + 8);
  null = (fun _state -> ());
  option =
    (fun state encode value ->
      state.bytes_written <- state.bytes_written + 1;
      match value with
      | None -> ()
      | Some payload -> encode.run size_backend state payload);
  list = size_list_backend;
  array = size_array_backend;
  record = size_record_backend;
  variant = size_variant_backend;
}

let size_of = fun encode value ->
  let state = { bytes_written = 0 } in
  match Serde.Ser.run encode size_backend state value with
  | Ok () -> Ok state.bytes_written
  | Error err -> Error err

let encode_into_bytes = fun encode dst value ->
  let state = {
    target = Bytes_target { dst; pos = 0 };
    output = IO.Buffer.create 0;
    scratch = IO.Bytes.create 8
  } in
  match Serde.Ser.run encode backend state value with
  | Error err -> Error err
  | Ok () -> (
      match state.target with
      | Bytes_target target -> Ok target.pos
      | Buffer_target
      | Writer_target _ -> panic "serde-bin encode_into_bytes: expected bytes target"
    )

let to_string = fun encode value ->
  match size_of encode value with
  | Error err -> Error err
  | Ok len ->
      let dst = IO.Bytes.create len in
      match encode_into_bytes encode dst value with
      | Ok written when Int.equal written len -> Ok (IO.Bytes.unsafe_to_string dst)
      | Ok written -> Error (`Msg ("serde-bin wrote "
      ^ Int.to_string written
      ^ " bytes but expected "
      ^ Int.to_string len))
      | Error err -> Error err

let to_writer = fun encode writer value ->
  let state = {
    target = Writer_target writer;
    output = IO.Buffer.create 4_096;
    scratch = IO.Bytes.create 8
  } in
  match Serde.Ser.run encode backend state value with
  | Error err -> Error err
  | Ok () -> (
      try
        flush_output state;
        match IO.flush writer with
        | Ok () -> Ok ()
        | Error err -> Error (`Io_error err)
      with
      | Serde.Encode_error err -> Error err
    )
