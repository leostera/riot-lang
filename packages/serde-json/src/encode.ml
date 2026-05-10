open Std

module Array = Collections.Array
module Vector = Collections.Vector

open Std.Result.Syntax

type encode_target =
  | Buffer_target
  | Writer_target of IO.Writer.t

type state = {
  target: encode_target;
  output: IO.Buffer.t;
  scratch: IO.Buffer.t;
  mutable escaped_literals: (string * string) list;
}

external format_float: string -> float -> string = "caml_format_float"

let flush_threshold = 4_096

let raise_io_error = fun err -> raise (Serde.Encode_error (`Io_error err))

let flush_output = fun state ->
  match state.target with
  | Buffer_target -> ()
  | Writer_target writer ->
      if IO.Buffer.length state.output > 0 then (
        match IO.write_all writer ~from:state.output with
        | Ok () -> IO.Buffer.clear state.output
        | Error err -> raise_io_error err
      )

let maybe_flush_output = fun state ->
  match state.target with
  | Buffer_target -> ()
  | Writer_target _ ->
      if IO.Buffer.length state.output >= flush_threshold then
        flush_output state

let write_char = fun state value ->
  IO.Buffer.add_char state.output value;
  maybe_flush_output state

let write_string = fun state value ->
  IO.Buffer.add_string state.output value;
  maybe_flush_output state

let scratch_write_char = fun state value -> IO.Buffer.add_char state.scratch value

let scratch_write_string = fun state value -> IO.Buffer.add_string state.scratch value

let hex_digit = fun value ->
  match value with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'A'
  | 11 -> 'B'
  | 12 -> 'C'
  | 13 -> 'D'
  | 14 -> 'E'
  | 15 -> 'F'
  | _ -> panic "Encode.hex_digit: invalid hex digit"

let add_unicode_escape = fun write_str write_chr code ->
  write_str "\\u";
  write_chr (hex_digit ((code lsr 12) land 0xf));
  write_chr (hex_digit ((code lsr 8) land 0xf));
  write_chr (hex_digit ((code lsr 4) land 0xf));
  write_chr (hex_digit (code land 0xf))

let append_escaped_string = fun write_str write_chr value ->
  write_chr '"';
  String.iter
    (fun __tmp1 ->
      match __tmp1 with
      | '"' -> write_str "\\\""
      | '\\' -> write_str "\\\\"
      | '\b' -> write_str "\\b"
      | '\012' -> write_str "\\f"
      | '\n' -> write_str "\\n"
      | '\r' -> write_str "\\r"
      | '\t' -> write_str "\\t"
      | c when Char.code c < 0x20 -> add_unicode_escape write_str write_chr (Char.code c)
      | c -> write_chr c)
    value;
  write_chr '"'

let write_escaped_string = fun state value ->
  append_escaped_string
    (write_string state)
    (write_char state)
    value

let write_cached_escaped_literal = fun state value ->
  let rec lookup = fun __tmp1 ->
    match __tmp1 with
    | [] ->
        IO.Buffer.clear state.scratch;
        append_escaped_string (scratch_write_string state) (scratch_write_char state) value;
        let escaped = IO.Buffer.contents state.scratch in
        state.escaped_literals <- (value, escaped) :: state.escaped_literals;
        write_string state escaped
    | (key, escaped) :: _ when String.equal key value -> write_string state escaped
    | _ :: rest -> lookup rest
  in
  lookup state.escaped_literals

let float_to_roundtrip_string = fun value ->
  let text12 = format_float "%.12g" value in
  match Float.parse text12 with
  | Some roundtrip when Float.equal value roundtrip -> text12
  | _ ->
      let text15 = format_float "%.15g" value in
      match Float.parse text15 with
      | Some roundtrip when Float.equal value roundtrip -> text15
      | _ -> format_float "%.18g" value

let float_to_json = fun value ->
  if Float.is_nan value || Float.is_infinite value then
    "null"
  else
    let text = float_to_roundtrip_string value in
    if String.ends_with ~suffix:"." text then
      text ^ "0"
    else
      text

let rec list_backend: 'value. state -> 'value Serde.Ser.t -> 'value vec -> unit = fun
  state encode values ->
  write_char state '[';
  let first = ref true in
  Vector.for_each
    values
    ~fn:(fun value ->
      if !first then
        first := false
      else
        write_char state ',';
      encode.run backend state value);
  write_char state ']'

and array_backend: 'value. state -> 'value Serde.Ser.t -> 'value array -> unit = fun
  state encode values ->
  write_char state '[';
  for index = 0 to Array.length values - 1 do
    if not (Int.equal index 0) then
      write_char state ',';
    encode.run
      backend
      state
      (Array.get_unchecked values ~at:index)
  done;
  write_char state ']'

and dict_backend: 'value. state -> 'value Serde.Ser.t -> (string * 'value) vec -> unit = fun
  state encode values ->
  write_char state '{';
  let first = ref true in
  Vector.for_each
    values
    ~fn:(fun (name, value) ->
      if !first then
        first := false
      else
        write_char state ',';
      write_cached_escaped_literal state name;
      write_char state ':';
      encode.run backend state value);
  write_char state '}'

and record_backend: 'value. state -> 'value Serde.Ser.fields -> 'value -> unit = fun
  state fields value ->
  write_char state '{';
  for index = 0 to Array.length fields - 1 do
    if not (Int.equal index 0) then
      write_char state ',';
    match Array.get_unchecked fields ~at:index with
    | Serde.Ser.Field (name, encode, get) ->
        write_cached_escaped_literal state name;
        write_char state ':';
        encode.run backend state (get value)
  done;
  write_char state '}'

and variant_backend: 'value. state -> 'value Serde.Ser.variant_cases -> 'value -> unit = fun
  state cases value ->
  let rec loop index =
    if Int.equal index (Array.length cases) then
      raise (Serde.Encode_error `invalid_tag)
    else
      match Array.get_unchecked cases ~at:index with
      | Serde.Ser.Unit (tag, matches) ->
          if matches value then
            write_cached_escaped_literal state tag
          else
            loop (index + 1)
      | Serde.Ser.Newtype (tag, encode, unwrap) -> (
          match unwrap value with
          | Some payload ->
              write_char state '{';
              write_cached_escaped_literal state tag;
              write_char state ':';
              encode.run backend state payload;
              write_char state '}'
          | None -> loop (index + 1)
        )
  in
  loop 0

and backend: state Serde.Ser.backend = {
  bool =
    (fun state value ->
      if value then
        write_string state "true"
      else
        write_string state "false");
  string = write_escaped_string;
  int = (fun state value -> write_string state (Int.to_string value));
  int32 = (fun state value -> write_string state (Int32.to_string value));
  int64 = (fun state value -> write_string state (Int64.to_string value));
  float = (fun state value -> write_string state (float_to_json value));
  null = (fun state -> write_string state "null");
  option =
    (fun state encode value ->
      match value with
      | None -> write_string state "null"
      | Some payload -> encode.run backend state payload);
  list = list_backend;
  array = array_backend;
  dict = dict_backend;
  record = record_backend;
  variant = variant_backend;
}

let to_string = fun encode value ->
  let state = {
    target = Buffer_target;
    output = IO.Buffer.create ~size:256;
    scratch = IO.Buffer.create ~size:64;
    escaped_literals = [];
  }
  in
  let* () = Serde.Ser.run encode backend state value in
  Ok (IO.Buffer.contents state.output)

let to_writer = fun encode writer value ->
  let state = {
    target = Writer_target writer;
    output = IO.Buffer.create ~size:4_096;
    scratch = IO.Buffer.create ~size:64;
    escaped_literals = [];
  }
  in
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
