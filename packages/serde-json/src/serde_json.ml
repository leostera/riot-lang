open Std

let ( let* ) = Result.and_then

module De = Serde.De

type encode_state = {
  buffer: IO.Buffer.t;
  scratch: IO.Buffer.t;
  mutable escaped_literals: (string * string) list;
}

let write_char = fun state value ->
  IO.Buffer.add_char state.buffer value

let write_string = fun state value ->
  IO.Buffer.add_string state.buffer value

let scratch_write_char = fun state value ->
  IO.Buffer.add_char state.scratch value

let scratch_write_string = fun state value ->
  IO.Buffer.add_string state.scratch value

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
  | _ -> panic "Serde_json.hex_digit: invalid hex digit"

let add_unicode_escape = fun write_str write_chr code ->
  write_str "\\u";
  write_chr (hex_digit ((code lsr 12) land 0xf));
  write_chr (hex_digit ((code lsr 8) land 0xf));
  write_chr (hex_digit ((code lsr 4) land 0xf));
  write_chr (hex_digit (code land 0xf))

let append_escaped_string = fun write_str write_chr value ->
  write_chr '"';
  String.iter
    (
      function
      | '"' -> write_str "\\\""
      | '\\' -> write_str "\\\\"
      | '\b' -> write_str "\\b"
      | '\012' -> write_str "\\f"
      | '\n' -> write_str "\\n"
      | '\r' -> write_str "\\r"
      | '\t' -> write_str "\\t"
      | c when Char.code c < 0x20 ->
          add_unicode_escape write_str write_chr (Char.code c)
      | c -> write_chr c
    )
    value;
  write_chr '"'

let write_escaped_string = fun state value ->
  append_escaped_string (write_string state) (write_char state) value

let write_cached_escaped_literal = fun state value ->
  let rec lookup = function
    | [] ->
        IO.Buffer.clear state.scratch;
        append_escaped_string (scratch_write_string state) (scratch_write_char state) value;
        let escaped = IO.Buffer.contents state.scratch in
        state.escaped_literals <- (value, escaped) :: state.escaped_literals;
        write_string state escaped
    | (key, escaped) :: _ when String.equal key value ->
        write_string state escaped
    | _ :: rest ->
        lookup rest
  in
  lookup state.escaped_literals

let float_to_json = fun value ->
  if Float.is_nan value || Float.is_infinite value then
    "null"
  else
    let text = Float.to_string value in
    if String.ends_with ~suffix:"." text then
      text ^ "0"
    else
      text

let rec ser_list_backend: 'value. encode_state -> 'value Serde.Ser.t -> 'value list -> unit =
 fun state encode values ->
  write_char state '[';
  let first = ref true in
  List.iter
    (fun value ->
      if !first then
        first := false
      else
        write_char state ',';
      encode.run ser_backend state value)
    values;
  write_char state ']'

and ser_array_backend:
  'value. encode_state -> 'value Serde.Ser.t -> 'value array -> unit =
 fun state encode values ->
  write_char state '[';
  for index = 0 to array__length values - 1 do
    if not (Int.equal index 0) then
      write_char state ',';
    encode.run ser_backend state (array__get values index)
  done;
  write_char state ']'

and ser_record_backend:
  'value. encode_state -> 'value Serde.Ser.fields -> 'value -> unit =
 fun state fields value ->
  write_char state '{';
  for index = 0 to array__length fields - 1 do
    if not (Int.equal index 0) then
      write_char state ',';
    match array__get fields index with
    | Serde.Ser.Field (name, encode, get) ->
        write_cached_escaped_literal state name;
        write_char state ':';
        encode.run ser_backend state (get value)
  done;
  write_char state '}'

and ser_variant_backend:
  'value. encode_state -> 'value Serde.Ser.variant_cases -> 'value -> unit =
 fun state cases value ->
  let rec loop index =
    if Int.equal index (array__length cases) then
      raise (Serde.Encode_error `invalid_tag)
    else
      match array__get cases index with
      | Serde.Ser.Unit (tag, matches) ->
          if matches value then
            write_cached_escaped_literal state tag
          else
            loop (index + 1)
      | Serde.Ser.Newtype (tag, encode, unwrap) ->
          (match unwrap value with
          | Some payload ->
              write_char state '{';
              write_cached_escaped_literal state tag;
              write_char state ':';
              encode.run ser_backend state payload;
              write_char state '}';
          | None ->
              loop (index + 1))
  in
  loop 0

and ser_backend: encode_state Serde.Ser.backend = {
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
      | None ->
          write_string state "null"
      | Some payload ->
          encode.run ser_backend state payload);
  list = ser_list_backend;
  array = ser_array_backend;
  record = ser_record_backend;
  variant = ser_variant_backend;
}

let to_string = fun encode value ->
  let state = {
    buffer = IO.Buffer.create 256;
    scratch = IO.Buffer.create 64;
    escaped_literals = [];
  } in
  let* () = Serde.Ser.run encode ser_backend state value in
  Ok (IO.Buffer.contents state.buffer)

type state = {
  input: string;
  len: int;
  mutable pos: int;
  scratch: IO.Buffer.t;
}

let error_at = fun pos message ->
  raise (Serde.Decode_error (`Msg (message ^ " at position " ^ Int.to_string pos)))

let unexpected_end = fun state expected ->
  error_at state.pos ("unexpected end of input while parsing " ^ expected)

let unexpected_character = fun state actual expected ->
  error_at
    state.pos
    ("unexpected character '" ^ String.make 1 actual ^ "' (expected " ^ expected ^ ")")

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_value_delimiter = function
  | None
  | Some (' ' | '\t' | '\n' | '\r' | ',' | ']' | '}') ->
      true
  | _ ->
      false

let current_char = fun state ->
  if state.pos < state.len then
    Some state.input.[state.pos]
  else
    None

let advance = fun state ->
  if state.pos < state.len then
    state.pos <- state.pos + 1

let skip_whitespace = fun state ->
  let input = state.input in
  let length = state.len in
  let rec loop pos =
    if pos >= length then
      pos
    else
      match String.unsafe_get input pos with
      | ' ' | '\t' | '\n' | '\r' ->
          loop (pos + 1)
      | _ ->
          pos
  in
  state.pos <- loop state.pos

let expect_char = fun state expected expected_name ->
  if state.pos >= state.len then
    unexpected_end state expected_name
  else
    let actual = state.input.[state.pos] in
    if Char.equal actual expected then
      state.pos <- state.pos + 1
    else
      unexpected_character state actual expected_name

let skip_then_expect_char = fun state expected expected_name ->
  skip_whitespace state;
  expect_char state expected expected_name

let expect_literal = fun state literal ->
  let length = String.length literal in
  let start = state.pos in
  if start + length > state.len then
    unexpected_end state literal
  else
    let rec loop index =
      if Int.equal index length then (
        state.pos <- start + length;
        if is_value_delimiter (current_char state) then
          ()
        else
          match current_char state with
          | Some actual ->
              unexpected_character state actual (literal ^ " delimiter")
          | None ->
              ()
      ) else if Char.equal state.input.[start + index] literal.[index] then
        loop (index + 1)
      else
        error_at start ("expected '" ^ literal ^ "'")
    in
    loop 0

let hex_value = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let read_hex_quad = fun state ->
  if state.pos + 4 >= state.len then
    unexpected_end state "unicode escape"
  else
    let decode index =
      match hex_value state.input.[index] with
      | Some value -> value
      | None ->
          error_at index "expected hex digit in unicode escape"
    in
    let value0 = decode (state.pos + 1) in
    let value1 = decode (state.pos + 2) in
    let value2 = decode (state.pos + 3) in
    let value3 = decode (state.pos + 4) in
    state.pos <- state.pos + 5;
    (value0 lsl 12)
    lor (value1 lsl 8)
    lor (value2 lsl 4)
    lor value3

let read_unicode_scalar = fun state ->
  let code = read_hex_quad state in
  if code >= 0xD800 && code <= 0xDBFF then (
    if state.pos + 1 >= state.len then
      unexpected_end state "unicode surrogate pair"
    else if not (Char.equal state.input.[state.pos] '\\' && Char.equal state.input.[state.pos + 1] 'u') then
      error_at state.pos "expected low surrogate after high surrogate"
    else (
      state.pos <- state.pos + 1;
      let low = read_hex_quad state in
      if low < 0xDC00 || low > 0xDFFF then
        error_at state.pos "expected low surrogate after high surrogate"
      else
        0x10000 + (((code - 0xD800) lsl 10) lor (low - 0xDC00))
    )
  ) else if code >= 0xDC00 && code <= 0xDFFF then
    error_at state.pos "unexpected low surrogate without preceding high surrogate"
  else
    code

let append_unicode_escape = fun state ->
  let code = read_unicode_scalar state in
  IO.Buffer.add_utf_8_uchar state.scratch (Kernel.Uchar.of_int code)

let skip_unicode_escape = fun state ->
  ignore (read_unicode_scalar state)

let skip_string = fun state ->
  expect_char state '"' "string";
  let rec loop () =
    match current_char state with
    | None ->
        unexpected_end state "string"
    | Some '"' ->
        advance state
    | Some '\\' ->
        advance state;
        (match current_char state with
        | None ->
            unexpected_end state "string escape"
        | Some ('"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't') ->
            advance state;
            loop ()
        | Some 'u' ->
            skip_unicode_escape state;
            loop ()
        | Some actual ->
            unexpected_character state actual "valid string escape")
    | Some actual when Char.code actual < 0x20 ->
        error_at state.pos "unescaped control character in string"
    | Some _ ->
        advance state;
        loop ()
  in
  loop ()

let parse_string = fun state ->
  skip_whitespace state;
  match current_char state with
  | Some '"' ->
      advance state;
      let input = state.input in
      let length = state.len in
      let start = state.pos in
      let rec scan pos =
        if pos >= length then
          pos
        else
          let c = String.unsafe_get input pos in
          if Char.equal c '"' || Char.equal c '\\' || Char.code c < 0x20 then
            pos
          else
            scan (pos + 1)
      in
      state.pos <- scan state.pos;
      if state.pos >= state.len then
        unexpected_end state "string"
      else
        let current = String.unsafe_get state.input state.pos in
        if Char.equal current '"' then (
          let value = String.sub state.input start (state.pos - start) in
          advance state;
          value
        ) else if Char.equal current '\\' then (
          IO.Buffer.clear state.scratch;
          if state.pos > start then
            IO.Buffer.add_substring state.scratch state.input start (state.pos - start);
          let rec loop () =
            match current_char state with
            | None ->
                unexpected_end state "string"
            | Some '"' ->
                advance state;
                IO.Buffer.contents state.scratch
            | Some '\\' ->
                advance state;
                (match current_char state with
                | None ->
                    unexpected_end state "string escape"
                | Some '"' ->
                    IO.Buffer.add_char state.scratch '"';
                    advance state;
                    loop ()
                | Some '\\' ->
                    IO.Buffer.add_char state.scratch '\\';
                    advance state;
                    loop ()
                | Some '/' ->
                    IO.Buffer.add_char state.scratch '/';
                    advance state;
                    loop ()
                | Some 'b' ->
                    IO.Buffer.add_char state.scratch '\b';
                    advance state;
                    loop ()
                | Some 'f' ->
                    IO.Buffer.add_char state.scratch '\012';
                    advance state;
                    loop ()
                | Some 'n' ->
                    IO.Buffer.add_char state.scratch '\n';
                    advance state;
                    loop ()
                | Some 'r' ->
                    IO.Buffer.add_char state.scratch '\r';
                    advance state;
                    loop ()
                | Some 't' ->
                    IO.Buffer.add_char state.scratch '\t';
                    advance state;
                    loop ()
                | Some 'u' ->
                    append_unicode_escape state;
                    loop ()
                | Some actual ->
                    unexpected_character state actual "valid string escape")
            | Some actual when Char.code actual < 0x20 ->
                error_at state.pos "unescaped control character in string"
            | Some actual ->
                IO.Buffer.add_char state.scratch actual;
                advance state;
                loop ()
          in
          loop ()
        ) else
          error_at state.pos "unescaped control character in string"
  | Some actual ->
      unexpected_character state actual "string"
  | None ->
      unexpected_end state "string"

let read_field_tag = fun state cases ->
  skip_whitespace state;
  match current_char state with
  | Some '"' ->
      advance state;
      let input = state.input in
      let length = state.len in
      let start = state.pos in
      let rec scan pos =
        if pos >= length then
          pos
        else
          let c = String.unsafe_get input pos in
          if Char.equal c '"' || Char.equal c '\\' || Char.code c < 0x20 then
            pos
          else
            scan (pos + 1)
      in
      state.pos <- scan state.pos;
      if state.pos >= state.len then
        unexpected_end state "object key"
      else
        let current = String.unsafe_get state.input state.pos in
        if Char.equal current '"' then (
          let tag =
            De.Fields.match_slice
              cases
              state.input
              ~offset:start
              ~length:(state.pos - start) in
          advance state;
          tag
        ) else if Char.equal current '\\' then (
          IO.Buffer.clear state.scratch;
          if state.pos > start then
            IO.Buffer.add_substring state.scratch state.input start (state.pos - start);
          let rec loop () =
            match current_char state with
            | None ->
                unexpected_end state "object key"
            | Some '"' ->
                advance state;
                De.Fields.match_buffer cases state.scratch
            | Some '\\' ->
                advance state;
                (match current_char state with
                | None ->
                    unexpected_end state "object key escape"
                | Some '"' ->
                    IO.Buffer.add_char state.scratch '"';
                    advance state;
                    loop ()
                | Some '\\' ->
                    IO.Buffer.add_char state.scratch '\\';
                    advance state;
                    loop ()
                | Some '/' ->
                    IO.Buffer.add_char state.scratch '/';
                    advance state;
                    loop ()
                | Some 'b' ->
                    IO.Buffer.add_char state.scratch '\b';
                    advance state;
                    loop ()
                | Some 'f' ->
                    IO.Buffer.add_char state.scratch '\012';
                    advance state;
                    loop ()
                | Some 'n' ->
                    IO.Buffer.add_char state.scratch '\n';
                    advance state;
                    loop ()
                | Some 'r' ->
                    IO.Buffer.add_char state.scratch '\r';
                    advance state;
                    loop ()
                | Some 't' ->
                    IO.Buffer.add_char state.scratch '\t';
                    advance state;
                    loop ()
                | Some 'u' ->
                    append_unicode_escape state;
                    loop ()
                | Some actual ->
                    unexpected_character state actual "valid object key escape")
            | Some actual when Char.code actual < 0x20 ->
                error_at state.pos "unescaped control character in object key"
            | Some actual ->
                IO.Buffer.add_char state.scratch actual;
                advance state;
                loop ()
          in
          loop ()
        ) else
          error_at state.pos "unescaped control character in object key"
  | Some actual ->
      unexpected_character state actual "object key"
  | None ->
      unexpected_end state "object key"

let parse_bool = fun state ->
  skip_whitespace state;
  match current_char state with
  | Some 't' ->
      expect_literal state "true";
      true
  | Some 'f' ->
      expect_literal state "false";
      false
  | Some actual ->
      unexpected_character state actual "bool"
  | None ->
      unexpected_end state "bool"

let parse_null = fun state ->
  skip_whitespace state;
  match current_char state with
  | Some 'n' ->
      expect_literal state "null"
  | Some actual ->
      unexpected_character state actual "null"
  | None ->
      unexpected_end state "null"

type scanned_number = {
  start: int;
  length: int;
  is_float: bool;
}

let scan_number = fun state ->
  skip_whitespace state;
  let input = state.input in
  let length = state.len in
  let start = state.pos in
  let current pos =
    if pos < length then
      Some (String.unsafe_get input pos)
    else
      None in
  let rec consume_digits pos =
    match current pos with
    | Some digit when is_digit digit ->
        consume_digits (pos + 1)
    | _ ->
        pos
  in
  let pos =
    match current state.pos with
    | Some '-' ->
        state.pos + 1
    | _ ->
        state.pos in
  let pos =
    match current pos with
    | Some '0' ->
        let next = pos + 1 in
        (match current next with
        | Some digit when is_digit digit ->
            error_at next "leading zeros are not allowed in JSON numbers"
        | _ ->
            ());
        next
    | Some ('1' .. '9') ->
        consume_digits (pos + 1)
    | Some actual ->
        error_at pos ("unexpected '" ^ String.make 1 actual ^ "' while parsing number")
    | None ->
        unexpected_end state "number" in
  let (pos, is_float) =
    match current pos with
    | Some '.' ->
        let after_dot = pos + 1 in
        (match current after_dot with
        | Some digit when is_digit digit ->
            (consume_digits (after_dot + 1), true)
        | Some actual ->
            error_at after_dot ("unexpected '" ^ String.make 1 actual ^ "' after decimal point")
        | None ->
            unexpected_end state "digit after decimal point")
    | _ ->
        (pos, false) in
  let (pos, is_float) =
    match current pos with
    | Some ('e' | 'E') ->
        let exponent_start = pos + 1 in
        let exponent_pos =
          match current exponent_start with
          | Some ('+' | '-') ->
              exponent_start + 1
          | _ ->
              exponent_start in
        (match current exponent_pos with
        | Some digit when is_digit digit ->
            (consume_digits (exponent_pos + 1), true)
        | Some actual ->
            error_at exponent_pos ("unexpected '" ^ String.make 1 actual ^ "' after exponent")
        | None ->
            unexpected_end state "digit after exponent")
    | _ ->
        (pos, is_float) in
  state.pos <- pos;
  if not (is_value_delimiter (current pos)) then (
    match current pos with
    | Some actual ->
        unexpected_character state actual "number delimiter"
    | None ->
        ()
  );
  {
    start;
    length = pos - start;
    is_float;
  }

let parse_number_text = fun state ->
  let number = scan_number state in
  (number, String.sub state.input number.start number.length)

let parse_int64 = fun state ->
  let (number, text) = parse_number_text state in
  if number.is_float then
    raise (Serde.Decode_error `invalid_field_type)
  else
    try Int64.of_string text with
    | _ ->
        raise (Serde.Decode_error `invalid_field_type)

let parse_int = fun state ->
  let (number, text) = parse_number_text state in
  if number.is_float then
    raise (Serde.Decode_error `invalid_field_type)
  else
    try Int.of_string text with
    | _ ->
        raise (Serde.Decode_error `invalid_field_type)

let parse_int32 = fun state ->
  let (number, text) = parse_number_text state in
  if number.is_float then
    raise (Serde.Decode_error `invalid_field_type)
  else
    try Int32.of_string text with
    | _ ->
        raise (Serde.Decode_error `invalid_field_type)

let parse_float = fun state ->
  let (_number, text) = parse_number_text state in
  try Float.of_string text with
  | _ ->
      raise (Serde.Decode_error `invalid_field_type)

let rec skip_value = fun state ->
  skip_whitespace state;
  match current_char state with
  | Some '{' ->
      advance state;
      skip_whitespace state;
      (match current_char state with
      | Some '}' ->
          advance state
      | _ ->
        let rec loop first =
          if first then
            ()
          else
            expect_char state ',' "object delimiter";
          skip_string state;
          expect_char state ':' "':' after object key";
          skip_value state;
          skip_whitespace state;
          match current_char state with
          | Some '}' ->
              advance state
          | Some _ ->
              loop false
          | None ->
              unexpected_end state "object"
        in
        loop true)
  | Some '[' ->
      advance state;
      skip_whitespace state;
      (match current_char state with
      | Some ']' ->
          advance state
      | _ ->
        let rec loop first =
          if first then
            ()
          else
            expect_char state ',' "array delimiter";
          skip_value state;
          skip_whitespace state;
          match current_char state with
          | Some ']' ->
              advance state
          | Some _ ->
              loop false
          | None ->
              unexpected_end state "array"
        in
        loop true)
  | Some '"' ->
      skip_string state
  | Some ('-' | '0' .. '9') ->
      ignore (parse_float state)
  | Some 't' ->
      expect_literal state "true"
  | Some 'f' ->
      expect_literal state "false"
  | Some 'n' ->
      expect_literal state "null"
  | Some actual ->
      unexpected_character state actual "JSON value"
  | None ->
      unexpected_end state "JSON value"

let list_nth = fun values index ->
  let rec loop values index =
    match (values, index) with
    | (value :: _, 0) -> value
    | (_ :: rest, _) -> loop rest (index - 1)
    | ([], _) -> panic "Serde_json.Fast.list_nth: index out of bounds"
  in
  loop values index

let array_of_list = fun values ->
  array__init (List.length values) (fun index -> list_nth values index)

let rec option_backend: 'value. state -> 'value De.t -> 'value option =
 fun state (decode : 'value De.t) ->
  skip_whitespace state;
  match current_char state with
  | Some 'n' ->
      parse_null state;
      None
  | _ ->
      Some (decode.run backend state)

and list_backend: 'value. state -> 'value De.t -> 'value list =
 fun state (decode : 'value De.t) ->
  skip_then_expect_char state '[' "array";
  skip_whitespace state;
  match current_char state with
  | Some ']' ->
      advance state;
      []
  | _ ->
      let acc = ref [] in
      let finished = ref false in
      while not !finished do
        let value = decode.run backend state in
        skip_whitespace state;
        match current_char state with
        | Some ',' ->
            advance state;
            acc := value :: !acc
        | Some ']' ->
            advance state;
            acc := value :: !acc;
            finished := true
        | Some actual ->
            unexpected_character state actual "array delimiter"
        | None ->
            unexpected_end state "array"
      done;
      List.rev !acc

and record_backend:
  'field 'acc 'value.
  state ->
  fields:'field De.Fields.t ->
  init:'acc ->
  step:('acc -> 'field option -> 'acc) ->
  finish:('acc -> 'value) ->
  'value =
 fun state ~fields ~init ~step ~finish ->
  skip_then_expect_char state '{' "object";
  skip_whitespace state;
  match current_char state with
  | Some '}' ->
      advance state;
      finish init
  | _ ->
      let acc = ref init in
      let first = ref true in
      let finished = ref false in
      while not !finished do
        if !first then
          first := false
        else (
          skip_whitespace state;
          expect_char state ',' "object delimiter"
        );
        let field = read_field_tag state fields in
        skip_then_expect_char state ':' "':' after object key";
        acc := step !acc field;
        skip_whitespace state;
        match current_char state with
        | Some '}' ->
            advance state;
            finished := true
        | Some _ ->
            ()
        | None ->
            unexpected_end state "object"
      done;
      finish !acc

and variant_backend: 'value. state -> 'value De.variant_cases -> 'value =
 fun state (cases : 'value De.variant_cases) ->
  let rec find_unit tag = function
    | [] ->
        raise (Serde.Decode_error `invalid_tag)
    | De.Unit (expected, value) :: _ when String.equal expected tag ->
        value
    | _ :: rest ->
        find_unit tag rest
  in
  let rec find_object tag = function
    | [] ->
        raise (Serde.Decode_error `invalid_tag)
    | De.Unit (expected, value) :: _ when String.equal expected tag ->
        parse_null state;
        value
    | De.Newtype (expected, decode, wrap) :: _ when String.equal expected tag ->
        wrap (decode.run backend state)
    | _ :: rest ->
        find_object tag rest
  in
  skip_whitespace state;
  match current_char state with
  | Some '"' ->
      let tag = parse_string state in
      find_unit tag cases
  | Some '{' ->
      expect_char state '{' "variant object";
      let tag = parse_string state in
      skip_then_expect_char state ':' "':' after variant tag";
      let value = find_object tag cases in
      skip_then_expect_char state '}' "closing '}' for variant";
      value
  | Some actual ->
      unexpected_character state actual "variant"
  | None ->
      unexpected_end state "variant"

and backend: state De.backend = {
  bool = parse_bool;
  string = parse_string;
  int = parse_int;
  int32 = parse_int32;
  int64 = parse_int64;
  float = parse_float;
  skip_any = skip_value;
  option = option_backend;
  list = list_backend;
  record = record_backend;
  variant = variant_backend;
}

let of_string = fun decode input ->
  let state = { input; len = String.length input; pos = 0; scratch = IO.Buffer.create 64 } in
  let* value = De.run decode backend state in
  skip_whitespace state;
  if Int.equal state.pos state.len then
    Ok value
  else
    Error (`Msg ("extra input after JSON value at position " ^ Int.to_string state.pos))
