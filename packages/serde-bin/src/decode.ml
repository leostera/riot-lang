open Std

module Array = Collections.Array
module Vector = Collections.Vector
module De = Serde.De

type reader_state = {
  reader: IO.Reader.t;
  buf: bytes;
  view: string;
  mutable base: int;
  mutable pos: int;
  mutable limit: int;
  mutable eof: bool;
}

type input =
  | String_input of {
      input: string;
      mutable pos: int;
    }
  | Reader_input of reader_state

type state = {
  input: input;
  scratch: IO.Buffer.t;
  bytes: bytes;
}

let buffer_capacity = 4_096

let max_container_length = 1_000_000

let error_at = fun pos message ->
  raise
    (Serde.Decode_error (`Msg (message ^ " at byte " ^ Int.to_string pos)))

let position = fun __tmp1 ->
  match __tmp1 with
  | String_input state -> state.pos
  | Reader_input state -> state.base + state.pos

let unexpected_end = fun state expected ->
  error_at
    (position state.input)
    ("unexpected end of input while decoding " ^ expected)

let compact = fun state ->
  if state.pos > 0 then
    let unread = state.limit - state.pos in
    if unread > 0 then
      IO.Bytes.blit_unchecked
        state.buf
        ~src_offset:state.pos
        ~dst:state.buf
        ~dst_offset:0
        ~len:unread;
  state.base <- state.base + state.pos;
  state.limit <- unread;
  state.pos <- 0

let refill = fun state ->
  if state.eof then
    false
  else (
    compact state;
    if Int.equal state.limit (IO.Bytes.length state.buf) then
      false
    else
      let free = IO.Bytes.length state.buf - state.limit in
      let chunk = IO.Buffer.create ~size:free in
      match IO.Reader.read state.reader ~into:chunk with
      | Ok 0 ->
          state.eof <- true;
          false
      | Ok read_len ->
          IO.IoSlice.blit_to_bytes_unchecked
            (IO.Buffer.readable chunk)
            ~src_off:0
            state.buf
            ~dst_off:state.limit
            ~len:read_len;
          state.limit <- state.limit + read_len;
          true
      | Error err -> raise (Serde.Decode_error (`Io_error err))
  )

let peek_byte = fun __tmp1 ->
  match __tmp1 with
  | String_input state ->
      if state.pos < String.length state.input then
        Some (String.unsafe_get state.input state.pos)
      else
        None
  | Reader_input state ->
      if state.pos < state.limit then
        Some (String.unsafe_get state.view state.pos)
      else if refill state then
        Some (String.unsafe_get state.view state.pos)
      else
        None

let advance = fun __tmp1 ->
  match __tmp1 with
  | String_input state -> state.pos <- state.pos + 1
  | Reader_input state -> state.pos <- state.pos + 1

let read_byte = fun state expected ->
  match peek_byte state.input with
  | Some byte ->
      advance state.input;
      byte
  | None -> unexpected_end state expected

let read_exact_into = fun state dst ~off ~len expected ->
  match state.input with
  | String_input input ->
      if input.pos + len > String.length input.input then
        unexpected_end state expected
      else (
        IO.Bytes.blit_string input.input ~src_offset:input.pos ~dst ~dst_offset:off ~len;
        input.pos <- input.pos + len
      )
  | Reader_input reader ->
      let rec loop dst_off remaining =
        if Int.equal remaining 0 then
          ()
        else
          let available = reader.limit - reader.pos in
          if Int.equal available 0 then
            if refill reader then
              loop dst_off remaining
            else
              unexpected_end state expected
          else
            let chunk = min remaining available in
            IO.Bytes.blit_unchecked
              reader.buf
              ~src_offset:reader.pos
              ~dst
              ~dst_offset:dst_off
              ~len:chunk;
        reader.pos <- reader.pos + chunk;
        loop (dst_off + chunk) (remaining - chunk)
      in
      loop off len

let read_uint32_le = fun state ->
  match state.input with
  | String_input input when input.pos + 4 <= String.length input.input ->
      let value = Stubs.read_uint32_le_from_string input.input input.pos in
      input.pos <- input.pos + 4;
      value
  | Reader_input reader when reader.pos + 4 <= reader.limit ->
      let value = Stubs.read_uint32_le_from_bytes reader.buf reader.pos in
      reader.pos <- reader.pos + 4;
      value
  | _ ->
      read_exact_into state state.bytes ~off:0 ~len:4 "u32";
      Stubs.read_uint32_le_from_bytes state.bytes 0

let read_int32_le = fun state ->
  match state.input with
  | String_input input when input.pos + 4 <= String.length input.input ->
      let value = Stubs.read_int32_le_from_string input.input input.pos in
      input.pos <- input.pos + 4;
      value
  | Reader_input reader when reader.pos + 4 <= reader.limit ->
      let value = Stubs.read_int32_le_from_bytes reader.buf reader.pos in
      reader.pos <- reader.pos + 4;
      value
  | _ ->
      read_exact_into state state.bytes ~off:0 ~len:4 "i32";
      Stubs.read_int32_le_from_bytes state.bytes 0

let read_int64_le = fun state ->
  match state.input with
  | String_input input when input.pos + 8 <= String.length input.input ->
      let value = Stubs.read_int64_le_from_string input.input input.pos in
      input.pos <- input.pos + 8;
      value
  | Reader_input reader when reader.pos + 8 <= reader.limit ->
      let value = Stubs.read_int64_le_from_bytes reader.buf reader.pos in
      reader.pos <- reader.pos + 8;
      value
  | _ ->
      read_exact_into state state.bytes ~off:0 ~len:8 "i64";
      Stubs.read_int64_le_from_bytes state.bytes 0

let decode_length = fun state kind ->
  let value = read_uint32_le state in
  if value < 0 then
    error_at (position state.input) ("decoded " ^ kind ^ " length is negative")
  else
    value

let decode_container_length = fun state kind ->
  let value = decode_length state kind in
  if value > max_container_length then
    error_at
      (position state.input)
      ("decoded " ^ kind ^ " length exceeds maximum supported elements")
  else
    value

let raise_int_out_of_range = fun pos -> error_at pos "decoded int does not fit in an OCaml int"

let variant_uses_u8 = fun cases -> Array.length cases <= 0x100

let read_string = fun state ->
  let len = decode_length state "string" in
  match state.input with
  | String_input input ->
      if input.pos + len > String.length input.input then
        unexpected_end state "string"
      else
        let value = String.sub input.input ~offset:input.pos ~len in
        input.pos <- input.pos + len;
      value
  | Reader_input reader ->
      let available = reader.limit - reader.pos in
      if len <= available then
        let value = String.sub reader.view ~offset:reader.pos ~len in
        reader.pos <- reader.pos + len;
        value
      else (
        IO.Buffer.clear state.scratch;
        let rec loop remaining =
          if Int.equal remaining 0 then
            IO.Buffer.contents state.scratch
          else
            let available = reader.limit - reader.pos in
            if Int.equal available 0 then
              if refill reader then
                loop remaining
              else
                unexpected_end state "string"
            else
              let chunk = min remaining available in
              IO.Buffer.add_subbytes state.scratch reader.buf reader.pos chunk;
          reader.pos <- reader.pos + chunk;
          loop (remaining - chunk)
        in
        loop len
      )

let read_int = fun state ->
  match state.input with
  | String_input input when input.pos + 8 <= String.length input.input ->
      let pos = input.pos in
      let value =
        try Stubs.read_int_le_from_string input.input pos with
        | Invalid_argument _ -> raise_int_out_of_range (pos + 8)
      in
      input.pos <- pos + 8;
      value
  | Reader_input reader when reader.pos + 8 <= reader.limit ->
      let pos = reader.pos in
      let value =
        try Stubs.read_int_le_from_bytes reader.buf pos with
        | Invalid_argument _ -> raise_int_out_of_range (reader.base + pos + 8)
      in
      reader.pos <- pos + 8;
      value
  | _ ->
      read_exact_into state state.bytes ~off:0 ~len:8 "i64";
      (
        try Stubs.read_int_le_from_bytes state.bytes 0 with
        | Invalid_argument _ -> raise_int_out_of_range (position state.input)
      )

let rec list_backend: 'value. state -> 'value De.t -> 'value vec = fun state decode ->
  let len = decode_container_length state "list" in
  let values = Vector.with_capacity ~size:(min len 64) in
  for _index = 0 to len - 1 do
    Vector.push values ~value:(decode.run backend state)
  done;
  values

and array_backend: 'value. state -> 'value De.t -> 'value array = fun state decode ->
  let len = decode_container_length state "array" in
  Array.init ~count:len ~fn:(fun _index -> decode.run backend state)

and dict_backend: 'value. state -> 'value De.t -> (string * 'value) vec = fun state decode ->
  let len = decode_container_length state "dict" in
  let values = Vector.with_capacity ~size:(min len 64) in
  for _index = 0 to len - 1 do
    let key = read_string state in
    Vector.push values ~value:(key, decode.run backend state)
  done;
  values

and record_backend:
  'field 'acc 'value. state ->
  fields:'field De.Fields.t ->
  init:'acc ->
  step:('acc -> 'field option -> 'acc) ->
  finish:('acc -> 'value) ->
  'value = fun _state ~fields ~init ~step ~finish ->
  let rec loop index acc =
    if Int.equal index (De.Fields.length fields) then
      finish acc
    else
      let next = step acc (Some (De.Fields.tag_at_unchecked fields index)) in
      loop (index + 1) next
  in
  loop 0 init

and record_mut_backend:
  'field 'builder 'value. state ->
  fields:'field De.Fields.t ->
  create:(unit -> 'builder) ->
  step:('builder -> 'field option -> unit) ->
  finish:('builder -> 'value) ->
  'value = fun _state ~fields ~create ~step ~finish ->
  let builder = create () in
  for index = 0 to De.Fields.length fields - 1 do
    step builder (Some (De.Fields.tag_at_unchecked fields index))
  done;
  finish builder

and variant_backend: 'value. state -> 'value De.compiled_variant_cases -> 'value = fun
  state cases ->
  let index =
    if variant_uses_u8 cases then
      Char.code (read_byte state "variant")
    else
      decode_length state "variant"
  in
  if index < 0 || index >= Array.length cases then
    raise (Serde.Decode_error `invalid_tag)
  else
    match Array.get_unchecked cases ~at:index with
    | De.Unit (_tag, value) -> value
    | De.Newtype (_tag, decode, wrap) -> wrap (decode.run backend state)

and backend: state De.backend = {
  bool =
    (fun state ->
      match read_byte state "bool" with
      | '\000' -> false
      | '\001' -> true
      | _ -> error_at (position state.input - 1) "invalid bool value");
  string = read_string;
  int = read_int;
  int32 = read_int32_le;
  int64 = read_int64_le;
  float =
    (fun state ->
      read_int64_le state
      |> Int64.float_of_bits);
  skip_any = (fun _state -> raise (Serde.Decode_error `unimplemented));
  option =
    (fun state decode ->
      match read_byte state "option tag" with
      | '\000' -> None
      | '\001' -> Some (decode.run backend state)
      | _ -> error_at (position state.input - 1) "invalid option tag");
  list = list_backend;
  array = array_backend;
  dict = dict_backend;
  record = record_backend;
  record_mut = record_mut_backend;
  variant = variant_backend;
}

let finish = fun state value ->
  match peek_byte state.input with
  | None -> Ok value
  | Some _ ->
      Error (`Msg ("extra input after binary value at byte " ^ Int.to_string (position state.input)))

let from_input = fun decode input ->
  let state = { input; scratch = IO.Buffer.create ~size:64; bytes = IO.Bytes.create ~size:8 } in
  match De.run decode backend state with
  | Error err -> Error err
  | Ok value -> finish state value

let decode_prefix = fun decode input ->
  let state = {
    input = String_input { input; pos = 0 };
    scratch = IO.Buffer.create ~size:64;
    bytes = IO.Bytes.create ~size:8;
  }
  in
  match De.run decode backend state with
  | Error err -> Error err
  | Ok value -> Ok (value, position state.input)

let from_string = fun decode input -> from_input decode (String_input { input; pos = 0 })

let from_reader = fun decode reader ->
  let buf = IO.Bytes.create ~size:buffer_capacity in
  let input = Reader_input {
    reader;
    buf;
    view = IO.Bytes.unsafe_to_string buf;
    base = 0;
    pos = 0;
    limit = 0;
    eof = false;
  }
  in
  from_input decode input
