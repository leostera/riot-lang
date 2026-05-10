open Std

module Array = Collections.Array
module Vector = Collections.Vector

open Std.Result.Syntax

module De = Serde.De
module Input = Buffered_input

type state = {
  input: Input.t;
  scratch: IO.Buffer.t;
}

let error_at = fun pos message ->
  raise
    (Serde.Decode_error (`Msg (message ^ " at position " ^ Int.to_string pos)))

let unexpected_end = fun state expected ->
  error_at
    (Input.position state.input)
    ("unexpected end of input while parsing " ^ expected)

let unexpected_character = fun state actual expected ->
  error_at
    (Input.position state.input)
    ("unexpected character '" ^ String.make ~len:1 ~char:actual ^ "' (expected " ^ expected ^ ")")

let is_digit = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9' -> true
  | _ -> false

let is_value_delimiter = fun __tmp1 ->
  match __tmp1 with
  | None
  | Some (' ' | '\t' | '\n' | '\r' | ',' | ']' | '}') -> true
  | _ -> false

let expect_char = fun state expected expected_name ->
  match Input.current_char state.input with
  | None -> unexpected_end state expected_name
  | Some actual ->
      if Char.equal actual expected then
        Input.advance state.input
      else
        unexpected_character state actual expected_name

let skip_then_expect_char = fun state expected expected_name ->
  Input.skip_whitespace state.input;
  expect_char state expected expected_name

let expect_literal = fun state literal ->
  let length = String.length literal in
  let start = Input.position state.input in
  let rec loop index =
    if Int.equal index length then (
      Input.advance_by state.input length;
      if is_value_delimiter (Input.current_char state.input) then
        ()
      else
        match Input.current_char state.input with
        | Some actual -> unexpected_character state actual (literal ^ " delimiter")
        | None -> ()
    ) else
      match Input.peek_char state.input ~offset:index with
      | None -> unexpected_end state literal
      | Some actual when Char.equal actual (String.get_unchecked literal ~at:index) ->
          loop (index + 1)
      | Some _ -> error_at start ("expected '" ^ literal ^ "'")
  in
  loop 0

let hex_value = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let read_hex_quad = fun state ->
  let start = Input.position state.input in
  let decode offset =
    match Input.peek_char state.input ~offset with
    | Some actual -> (
        match hex_value actual with
        | Some value -> value
        | None -> error_at (start + offset) "expected hex digit in unicode escape"
      )
    | None -> unexpected_end state "unicode escape"
  in
  let value0 = decode 1 in
  let value1 = decode 2 in
  let value2 = decode 3 in
  let value3 = decode 4 in
  Input.advance_by state.input 5;
  (value0 lsl 12) lor (value1 lsl 8) lor (value2 lsl 4) lor value3

let rec read_unicode_scalar = fun state ->
  let code = read_hex_quad state in
  if code >= 0xd800 && code <= 0xdbff then
    match (Input.current_char state.input, Input.peek_char state.input ~offset:1) with
    | (Some '\\', Some 'u') ->
        Input.advance state.input;
        let low = read_hex_quad state in
        if low < 0xdc00 || low > 0xdfff then
          error_at (Input.position state.input) "expected low surrogate after high surrogate"
        else
          0x1_0000 + (((code - 0xd800) lsl 10) lor (low - 0xdc00))
    | (None, _)
    | (_, None) -> unexpected_end state "unicode surrogate pair"
    | _ -> error_at (Input.position state.input) "expected low surrogate after high surrogate"
  else if code >= 0xdc00 && code <= 0xdfff then
    error_at
      (Input.position state.input)
      "unexpected low surrogate without preceding high surrogate"
  else
    code

let append_unicode_escape = fun state ->
  let code = read_unicode_scalar state in
  match Unicode.Rune.from_int code with
  | Some rune -> IO.Buffer.add_utf_8_uchar state.scratch rune
  | None -> error_at (Input.position state.input) "invalid unicode scalar value"

let skip_unicode_escape = fun state -> ignore (read_unicode_scalar state)

let string_fast_continue = fun c ->
  not
    (Char.equal c '"' || Char.equal c '\\' || Char.code c < 0x20)

let append_current_range = fun state start stop ->
  Input.copy_range_to_buffer
    state.scratch
    state.input
    ~start
    ~stop

let reader_current_char: Input.reader_state -> char option = fun reader ->
  if Int.(reader.Input.pos < IO.Buffer.readable_bytes reader.Input.buffer) then
    Some (IO.Buffer.get_unchecked reader.Input.buffer ~at:reader.Input.pos)
  else if Input.refill reader then
    Some (IO.Buffer.get_unchecked reader.Input.buffer ~at:reader.Input.pos)
  else
    None

let reader_advance: Input.reader_state -> unit = fun reader ->
  reader.Input.pos <- reader.Input.pos + 1

let reader_advance_by: Input.reader_state -> int -> unit = fun reader count ->
  reader.Input.pos <- reader.Input.pos + count

let reader_append_range: IO.Buffer.t -> Input.reader_state -> int -> int -> unit = fun
  scratch reader start stop -> Input.reader_append_range scratch reader ~start ~stop

let rec reader_scan_while:
  Input.reader_state ->
  continue:(char -> bool) ->
  [`Stop of int * char | `Boundary of int | `Eof] = fun reader ~continue ->
  if Int.(reader.Input.pos >= IO.Buffer.readable_bytes reader.Input.buffer) then
    if Input.refill reader then
      reader_scan_while reader ~continue
    else
      `Eof
  else
    let rec loop pos =
      if Int.(pos >= IO.Buffer.readable_bytes reader.Input.buffer) then
        `Boundary pos
      else
        let current = IO.Buffer.get_unchecked reader.Input.buffer ~at:pos in
        if continue current then
          loop (pos + 1)
        else
          `Stop (pos, current)
    in
    loop reader.Input.pos

let reader_expect_char: state -> Input.reader_state -> char -> string -> unit = fun
  state reader expected expected_name ->
  match reader_current_char reader with
  | None -> unexpected_end state expected_name
  | Some actual ->
      if Char.equal actual expected then
        reader_advance reader
      else
        unexpected_character state actual expected_name

let reader_token_start = fun reader ->
  if Int.(reader.Input.pos >= IO.Buffer.readable_bytes reader.Input.buffer) then
    ignore (Input.refill reader);
  reader.Input.pos

let reader_skip_whitespace: Input.reader_state -> unit = fun reader ->
  let rec loop () =
    if Int.(reader.Input.pos >= IO.Buffer.readable_bytes reader.Input.buffer) then
      if Input.refill reader then
        loop ()
      else
        ()
    else
      let rec skip pos =
        if Int.(pos >= IO.Buffer.readable_bytes reader.Input.buffer) then
          pos
        else
          match IO.Buffer.get_unchecked reader.Input.buffer ~at:pos with
          | ' '
          | '\t'
          | '\n'
          | '\r' -> skip (pos + 1)
          | _ -> pos
      in
      reader.Input.pos <- skip reader.Input.pos;
    if
      Int.(reader.Input.pos >= IO.Buffer.readable_bytes reader.Input.buffer) && Input.refill reader
    then
      loop ()
  in
  loop ()

let reader_expect_literal: state -> Input.reader_state -> string -> unit = fun
  state reader literal ->
  let length = String.length literal in
  let rec loop index =
    if Int.equal index length then
      match reader_current_char reader with
      | Some actual when is_value_delimiter (Some actual) -> ()
      | Some actual -> unexpected_character state actual (literal ^ " delimiter")
      | None -> ()
    else
      match reader_current_char reader with
      | None -> unexpected_end state literal
      | Some actual when Char.equal actual (String.get_unchecked literal ~at:index) ->
          reader_advance reader;
          loop (index + 1)
      | Some _ -> error_at (Input.position state.input) ("expected '" ^ literal ^ "'")
  in
  loop 0

let parse_string_reader = fun state reader ->
  reader_skip_whitespace reader;
  reader_expect_char state reader '"' "string";
  let start = reader_token_start reader in
  let rec fast start =
    match reader_scan_while reader ~continue:string_fast_continue with
    | `Stop (stop, '"') ->
        let value = Input.reader_substring reader ~off:start ~len:(stop - start) in
        reader.Input.pos <- stop;
        reader_advance reader;
        value
    | `Stop (stop, '\\') ->
        IO.Buffer.clear state.scratch;
        reader_append_range state.scratch reader start stop;
        reader.Input.pos <- stop;
        slow ()
    | `Stop (_, actual) ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in string")
    | `Boundary stop ->
        IO.Buffer.clear state.scratch;
        reader_append_range state.scratch reader start stop;
        reader.Input.pos <- stop;
        slow ()
    | `Eof -> unexpected_end state "string"
  and slow () =
    match reader_current_char reader with
    | None -> unexpected_end state "string"
    | Some '"' ->
        reader_advance reader;
        IO.Buffer.contents state.scratch
    | Some '\\' ->
        reader_advance reader;
        (
          match reader_current_char reader with
          | None -> unexpected_end state "string escape"
          | Some '"' ->
              IO.Buffer.add_char state.scratch '"';
              reader_advance reader;
              slow ()
          | Some '\\' ->
              IO.Buffer.add_char state.scratch '\\';
              reader_advance reader;
              slow ()
          | Some '/' ->
              IO.Buffer.add_char state.scratch '/';
              reader_advance reader;
              slow ()
          | Some 'b' ->
              IO.Buffer.add_char state.scratch '\b';
              reader_advance reader;
              slow ()
          | Some 'f' ->
              IO.Buffer.add_char state.scratch '\012';
              reader_advance reader;
              slow ()
          | Some 'n' ->
              IO.Buffer.add_char state.scratch '\n';
              reader_advance reader;
              slow ()
          | Some 'r' ->
              IO.Buffer.add_char state.scratch '\r';
              reader_advance reader;
              slow ()
          | Some 't' ->
              IO.Buffer.add_char state.scratch '\t';
              reader_advance reader;
              slow ()
          | Some 'u' ->
              append_unicode_escape state;
              slow ()
          | Some actual -> unexpected_character state actual "valid string escape"
        )
    | Some actual when Char.code actual < 0x20 ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in string")
    | Some _ ->
        let start = reader_token_start reader in
        match reader_scan_while reader ~continue:string_fast_continue with
        | `Stop (stop, '"') ->
            reader_append_range state.scratch reader start stop;
            reader.Input.pos <- stop;
            reader_advance reader;
            IO.Buffer.contents state.scratch
        | `Stop (stop, '\\') ->
            reader_append_range state.scratch reader start stop;
            reader.Input.pos <- stop;
            slow ()
        | `Stop (_, actual) ->
            error_at
              (Input.position state.input)
              ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in string")
        | `Boundary stop ->
            reader_append_range state.scratch reader start stop;
            reader.Input.pos <- stop;
            slow ()
        | `Eof -> unexpected_end state "string"
  in
  fast start

let parse_string_generic = fun state ->
  Input.skip_whitespace state.input;
  expect_char state '"' "string";
  let start = Input.position state.input in
  let rec fast start =
    match Input.scan_while state.input ~continue:string_fast_continue with
    | `Stop (stop, '"') ->
        let value = Input.slice_to_string state.input ~start ~stop in
        Input.set_position state.input stop;
        Input.advance state.input;
        value
    | `Stop (stop, '\\') ->
        IO.Buffer.clear state.scratch;
        append_current_range state start stop;
        Input.set_position state.input stop;
        slow ()
    | `Stop (_, actual) ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in string")
    | `Boundary stop ->
        IO.Buffer.clear state.scratch;
        append_current_range state start stop;
        Input.set_position state.input stop;
        slow ()
    | `Eof _ -> unexpected_end state "string"
  and slow () =
    match Input.current_char state.input with
    | None -> unexpected_end state "string"
    | Some '"' ->
        Input.advance state.input;
        IO.Buffer.contents state.scratch
    | Some '\\' ->
        Input.advance state.input;
        (
          match Input.current_char state.input with
          | None -> unexpected_end state "string escape"
          | Some '"' ->
              IO.Buffer.add_char state.scratch '"';
              Input.advance state.input;
              slow ()
          | Some '\\' ->
              IO.Buffer.add_char state.scratch '\\';
              Input.advance state.input;
              slow ()
          | Some '/' ->
              IO.Buffer.add_char state.scratch '/';
              Input.advance state.input;
              slow ()
          | Some 'b' ->
              IO.Buffer.add_char state.scratch '\b';
              Input.advance state.input;
              slow ()
          | Some 'f' ->
              IO.Buffer.add_char state.scratch '\012';
              Input.advance state.input;
              slow ()
          | Some 'n' ->
              IO.Buffer.add_char state.scratch '\n';
              Input.advance state.input;
              slow ()
          | Some 'r' ->
              IO.Buffer.add_char state.scratch '\r';
              Input.advance state.input;
              slow ()
          | Some 't' ->
              IO.Buffer.add_char state.scratch '\t';
              Input.advance state.input;
              slow ()
          | Some 'u' ->
              append_unicode_escape state;
              slow ()
          | Some actual -> unexpected_character state actual "valid string escape"
        )
    | Some actual when Char.code actual < 0x20 ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in string")
    | Some _ ->
        let start = Input.position state.input in
        match Input.scan_while state.input ~continue:string_fast_continue with
        | `Stop (stop, '"') ->
            append_current_range state start stop;
            Input.set_position state.input stop;
            Input.advance state.input;
            IO.Buffer.contents state.scratch
        | `Stop (stop, '\\') ->
            append_current_range state start stop;
            Input.set_position state.input stop;
            slow ()
        | `Stop (_, actual) ->
            error_at
              (Input.position state.input)
              ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in string")
        | `Boundary stop ->
            append_current_range state start stop;
            Input.set_position state.input stop;
            slow ()
        | `Eof _ -> unexpected_end state "string"
  in
  fast start

let parse_string = fun state ->
  match state.input with
  | Input.Reader_input reader -> parse_string_reader state reader
  | Input.String_input _ -> parse_string_generic state

let skip_string_reader = fun state reader ->
  reader_expect_char state reader '"' "string";
  let rec loop () =
    match reader_scan_while reader ~continue:string_fast_continue with
    | `Stop (stop, '"') ->
        reader.Input.pos <- stop;
        reader_advance reader
    | `Stop (stop, '\\') ->
        reader.Input.pos <- stop;
        reader_advance reader;
        (
          match reader_current_char reader with
          | None -> unexpected_end state "string escape"
          | Some ('"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't') ->
              reader_advance reader;
              loop ()
          | Some 'u' ->
              skip_unicode_escape state;
              loop ()
          | Some actual -> unexpected_character state actual "valid string escape"
        )
    | `Stop (_, actual) ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in string")
    | `Boundary stop ->
        reader.Input.pos <- stop;
        loop ()
    | `Eof -> unexpected_end state "string"
  in
  loop ()

let skip_string_generic = fun state ->
  expect_char state '"' "string";
  let rec loop () =
    match Input.scan_while state.input ~continue:string_fast_continue with
    | `Stop (stop, '"') ->
        Input.set_position state.input stop;
        Input.advance state.input
    | `Stop (stop, '\\') ->
        Input.set_position state.input stop;
        Input.advance state.input;
        (
          match Input.current_char state.input with
          | None -> unexpected_end state "string escape"
          | Some ('"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't') ->
              Input.advance state.input;
              loop ()
          | Some 'u' ->
              skip_unicode_escape state;
              loop ()
          | Some actual -> unexpected_character state actual "valid string escape"
        )
    | `Stop (_, actual) ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in string")
    | `Boundary stop ->
        Input.set_position state.input stop;
        loop ()
    | `Eof _ -> unexpected_end state "string"
  in
  loop ()

let skip_string = fun state ->
  match state.input with
  | Input.Reader_input reader -> skip_string_reader state reader
  | Input.String_input _ -> skip_string_generic state

let read_field_tag_reader = fun state reader fields ->
  reader_skip_whitespace reader;
  reader_expect_char state reader '"' "object key";
  let start = reader_token_start reader in
  let rec fast start =
    match reader_scan_while reader ~continue:string_fast_continue with
    | `Stop (stop, '"') ->
        let tag =
          Input.reader_match_field_range fields reader ~offset:start ~length:(stop - start)
        in
        reader.Input.pos <- stop;
        reader_advance reader;
        tag
    | `Stop (stop, '\\') ->
        IO.Buffer.clear state.scratch;
        reader_append_range state.scratch reader start stop;
        reader.Input.pos <- stop;
        slow ()
    | `Stop (_, actual) ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in object key")
    | `Boundary stop ->
        IO.Buffer.clear state.scratch;
        reader_append_range state.scratch reader start stop;
        reader.Input.pos <- stop;
        slow ()
    | `Eof -> unexpected_end state "object key"
  and slow () =
    match reader_current_char reader with
    | None -> unexpected_end state "object key"
    | Some '"' ->
        reader_advance reader;
        De.Fields.match_buffer fields state.scratch
    | Some '\\' ->
        reader_advance reader;
        (
          match reader_current_char reader with
          | None -> unexpected_end state "object key escape"
          | Some '"' ->
              IO.Buffer.add_char state.scratch '"';
              reader_advance reader;
              slow ()
          | Some '\\' ->
              IO.Buffer.add_char state.scratch '\\';
              reader_advance reader;
              slow ()
          | Some '/' ->
              IO.Buffer.add_char state.scratch '/';
              reader_advance reader;
              slow ()
          | Some 'b' ->
              IO.Buffer.add_char state.scratch '\b';
              reader_advance reader;
              slow ()
          | Some 'f' ->
              IO.Buffer.add_char state.scratch '\012';
              reader_advance reader;
              slow ()
          | Some 'n' ->
              IO.Buffer.add_char state.scratch '\n';
              reader_advance reader;
              slow ()
          | Some 'r' ->
              IO.Buffer.add_char state.scratch '\r';
              reader_advance reader;
              slow ()
          | Some 't' ->
              IO.Buffer.add_char state.scratch '\t';
              reader_advance reader;
              slow ()
          | Some 'u' ->
              append_unicode_escape state;
              slow ()
          | Some actual -> unexpected_character state actual "valid object key escape"
        )
    | Some actual when Char.code actual < 0x20 ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in object key")
    | Some _ ->
        let start = reader_token_start reader in
        match reader_scan_while reader ~continue:string_fast_continue with
        | `Stop (stop, '"') ->
            reader_append_range state.scratch reader start stop;
            reader.Input.pos <- stop;
            reader_advance reader;
            De.Fields.match_buffer fields state.scratch
        | `Stop (stop, '\\') ->
            reader_append_range state.scratch reader start stop;
            reader.Input.pos <- stop;
            slow ()
        | `Stop (_, actual) ->
            error_at
              (Input.position state.input)
              ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in object key")
        | `Boundary stop ->
            reader_append_range state.scratch reader start stop;
            reader.Input.pos <- stop;
            slow ()
        | `Eof -> unexpected_end state "object key"
  in
  fast start

let read_field_tag_generic = fun state fields ->
  Input.skip_whitespace state.input;
  expect_char state '"' "object key";
  let start = Input.position state.input in
  let rec fast start =
    match Input.scan_while state.input ~continue:string_fast_continue with
    | `Stop (stop, '"') ->
        let tag = Input.match_field_range fields state.input ~start ~stop in
        Input.set_position state.input stop;
        Input.advance state.input;
        tag
    | `Stop (stop, '\\') ->
        IO.Buffer.clear state.scratch;
        append_current_range state start stop;
        Input.set_position state.input stop;
        slow ()
    | `Stop (_, actual) ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in object key")
    | `Boundary stop ->
        IO.Buffer.clear state.scratch;
        append_current_range state start stop;
        Input.set_position state.input stop;
        slow ()
    | `Eof _ -> unexpected_end state "object key"
  and slow () =
    match Input.current_char state.input with
    | None -> unexpected_end state "object key"
    | Some '"' ->
        Input.advance state.input;
        De.Fields.match_buffer fields state.scratch
    | Some '\\' ->
        Input.advance state.input;
        (
          match Input.current_char state.input with
          | None -> unexpected_end state "object key escape"
          | Some '"' ->
              IO.Buffer.add_char state.scratch '"';
              Input.advance state.input;
              slow ()
          | Some '\\' ->
              IO.Buffer.add_char state.scratch '\\';
              Input.advance state.input;
              slow ()
          | Some '/' ->
              IO.Buffer.add_char state.scratch '/';
              Input.advance state.input;
              slow ()
          | Some 'b' ->
              IO.Buffer.add_char state.scratch '\b';
              Input.advance state.input;
              slow ()
          | Some 'f' ->
              IO.Buffer.add_char state.scratch '\012';
              Input.advance state.input;
              slow ()
          | Some 'n' ->
              IO.Buffer.add_char state.scratch '\n';
              Input.advance state.input;
              slow ()
          | Some 'r' ->
              IO.Buffer.add_char state.scratch '\r';
              Input.advance state.input;
              slow ()
          | Some 't' ->
              IO.Buffer.add_char state.scratch '\t';
              Input.advance state.input;
              slow ()
          | Some 'u' ->
              append_unicode_escape state;
              slow ()
          | Some actual -> unexpected_character state actual "valid object key escape"
        )
    | Some actual when Char.code actual < 0x20 ->
        error_at
          (Input.position state.input)
          ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in object key")
    | Some _ ->
        let start = Input.position state.input in
        match Input.scan_while state.input ~continue:string_fast_continue with
        | `Stop (stop, '"') ->
            append_current_range state start stop;
            Input.set_position state.input stop;
            Input.advance state.input;
            De.Fields.match_buffer fields state.scratch
        | `Stop (stop, '\\') ->
            append_current_range state start stop;
            Input.set_position state.input stop;
            slow ()
        | `Stop (_, actual) ->
            error_at
              (Input.position state.input)
              ("unescaped control character '" ^ String.make ~len:1 ~char:actual ^ "' in object key")
        | `Boundary stop ->
            append_current_range state start stop;
            Input.set_position state.input stop;
            slow ()
        | `Eof _ -> unexpected_end state "object key"
  in
  fast start

let read_field_tag = fun state fields ->
  match state.input with
  | Input.Reader_input reader -> read_field_tag_reader state reader fields
  | Input.String_input _ -> read_field_tag_generic state fields

let parse_bool = fun state ->
  match state.input with
  | Input.Reader_input reader ->
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some 't' ->
            reader_expect_literal state reader "true";
            true
        | Some 'f' ->
            reader_expect_literal state reader "false";
            false
        | Some actual -> unexpected_character state actual "bool"
        | None -> unexpected_end state "bool"
      )
  | Input.String_input _ ->
      Input.skip_whitespace state.input;
      (
        match Input.current_char state.input with
        | Some 't' ->
            expect_literal state "true";
            true
        | Some 'f' ->
            expect_literal state "false";
            false
        | Some actual -> unexpected_character state actual "bool"
        | None -> unexpected_end state "bool"
      )

let parse_null = fun state ->
  match state.input with
  | Input.Reader_input reader ->
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some 'n' -> reader_expect_literal state reader "null"
        | Some actual -> unexpected_character state actual "null"
        | None -> unexpected_end state "null"
      )
  | Input.String_input _ ->
      Input.skip_whitespace state.input;
      (
        match Input.current_char state.input with
        | Some 'n' -> expect_literal state "null"
        | Some actual -> unexpected_character state actual "null"
        | None -> unexpected_end state "null"
      )

type scanned_number = { text: string; is_float: bool }

exception Use_slow_number_path

let rec reader_append_digits = fun state reader ->
  match reader_current_char reader with
  | Some digit when is_digit digit ->
      IO.Buffer.add_char state.scratch digit;
      reader_advance reader;
      reader_append_digits state reader
  | _ -> ()

let parse_number_text_reader_slow = fun state reader ->
  reader_skip_whitespace reader;
  IO.Buffer.clear state.scratch;
  let append_char value =
    IO.Buffer.add_char state.scratch value;
    reader_advance reader
  in
  (
    match reader_current_char reader with
    | Some '-' -> append_char '-'
    | _ -> ()
  );
  (
    match reader_current_char reader with
    | Some '0' ->
        append_char '0';
        (
          match reader_current_char reader with
          | Some digit when is_digit digit ->
              error_at (Input.position state.input) "leading zeros are not allowed in JSON numbers"
          | _ -> ()
        )
    | Some '1' .. '9' -> reader_append_digits state reader
    | Some actual ->
        error_at
          (Input.position state.input)
          ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' while parsing number")
    | None -> unexpected_end state "number"
  );
  let is_float = ref false in
  (
    match reader_current_char reader with
    | Some '.' ->
        is_float := true;
        append_char '.';
        (
          match reader_current_char reader with
          | Some digit when is_digit digit -> reader_append_digits state reader
          | Some actual ->
              error_at
                (Input.position state.input)
                ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' after decimal point")
          | None -> unexpected_end state "digit after decimal point"
        )
    | _ -> ()
  );
  (
    match reader_current_char reader with
    | Some ('e' | 'E' as exponent) ->
        is_float := true;
        append_char exponent;
        (
          match reader_current_char reader with
          | Some ('+' | '-' as sign) -> append_char sign
          | _ -> ()
        );
        (
          match reader_current_char reader with
          | Some digit when is_digit digit -> reader_append_digits state reader
          | Some actual ->
              error_at
                (Input.position state.input)
                ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' after exponent")
          | None -> unexpected_end state "digit after exponent"
        )
    | _ -> ()
  );
  if not (is_value_delimiter (reader_current_char reader)) then (
    match reader_current_char reader with
    | Some actual -> unexpected_character state actual "number delimiter"
    | None -> ()
  );
  { text = IO.Buffer.contents state.scratch; is_float = !is_float }

let parse_number_text_reader = fun state reader ->
  let fallback () = parse_number_text_reader_slow state reader in
  let start = ref reader.Input.pos in
  try
    reader_skip_whitespace reader;
    start := reader.Input.pos;
    let pos = ref !start in
    let current () =
      if !pos < IO.Buffer.readable_bytes reader.Input.buffer then
        Some (IO.Buffer.get_unchecked reader.Input.buffer ~at:!pos)
      else
        None
    in
    let need_more () =
      !pos >= IO.Buffer.readable_bytes reader.Input.buffer && not reader.Input.eof
    in
    let advance () =
      pos := !pos + 1
    in
    let rec advance_digits () =
      match current () with
      | Some digit when is_digit digit ->
          advance ();
          advance_digits ()
      | _ -> ()
    in
    (
      match current () with
      | Some '-' ->
          advance ();
          if need_more () then
            raise Use_slow_number_path
      | _ -> ()
    );
    (
      match current () with
      | Some '0' ->
          advance ();
          if need_more () then
            raise Use_slow_number_path;
          (
            match current () with
            | Some digit when is_digit digit ->
                error_at
                  (Input.position state.input)
                  "leading zeros are not allowed in JSON numbers"
            | _ -> ()
          )
      | Some '1' .. '9' ->
          advance ();
          advance_digits ();
          if need_more () then
            raise Use_slow_number_path
      | Some actual ->
          error_at
            (Input.position state.input)
            ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' while parsing number")
      | None -> unexpected_end state "number"
    );
    let is_float = ref false in
    (
      match current () with
      | Some '.' ->
          is_float := true;
          advance ();
          if need_more () then
            raise Use_slow_number_path;
          (
            match current () with
            | Some digit when is_digit digit ->
                advance ();
                advance_digits ();
                if need_more () then
                  raise Use_slow_number_path
            | Some actual ->
                error_at
                  (Input.position state.input)
                  ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' after decimal point")
            | None -> unexpected_end state "digit after decimal point"
          )
      | _ -> ()
    );
    (
      match current () with
      | Some ('e' | 'E') ->
          is_float := true;
          advance ();
          if need_more () then
            raise Use_slow_number_path;
          (
            match current () with
            | Some ('+' | '-') ->
                advance ();
                if need_more () then
                  raise Use_slow_number_path
            | _ -> ()
          );
          (
            match current () with
            | Some digit when is_digit digit ->
                advance ();
                advance_digits ();
                if need_more () then
                  raise Use_slow_number_path
            | Some actual ->
                error_at
                  (Input.position state.input)
                  ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' after exponent")
            | None -> unexpected_end state "digit after exponent"
          )
      | _ -> ()
    );
    if !pos >= IO.Buffer.readable_bytes reader.Input.buffer then
      if reader.Input.eof then (
        reader.Input.pos <- !pos;
        {
          text = Input.reader_substring reader ~off:!start ~len:(!pos - !start);
          is_float = !is_float;
        }
      ) else
        raise Use_slow_number_path
    else
      match current () with
      | Some actual when is_value_delimiter (Some actual) ->
          reader.Input.pos <- !pos;
          {
            text = Input.reader_substring reader ~off:!start ~len:(!pos - !start);
            is_float = !is_float;
          }
      | Some actual -> unexpected_character state actual "number delimiter"
      | None ->
          reader.Input.pos <- !pos;
          {
            text = Input.reader_substring reader ~off:!start ~len:(!pos - !start);
            is_float = !is_float;
          }
  with
  | Use_slow_number_path ->
      reader.Input.pos <- !start;
      fallback ()

let parse_number_text_generic = fun state ->
  Input.skip_whitespace state.input;
  let input =
    match state.input with
    | Input.String_input state -> state.input
    | Input.Reader_input _ -> panic "parse_number_text_generic: expected string input"
  in
  let input_length = String.length input in
  let start = Input.position state.input in
  let pos = ref start in
  let current () =
    if !pos < input_length then
      Some (String.unsafe_get input !pos)
    else
      None
  in
  let advance () =
    pos := !pos + 1
  in
  let rec advance_digits () =
    match current () with
    | Some digit when is_digit digit ->
        advance ();
        advance_digits ()
    | _ -> ()
  in
  (
    match current () with
    | Some '-' -> advance ()
    | _ -> ()
  );
  (
    match current () with
    | Some '0' ->
        advance ();
        (
          match current () with
          | Some digit when is_digit digit ->
              error_at (Input.position state.input) "leading zeros are not allowed in JSON numbers"
          | _ -> ()
        )
    | Some '1' .. '9' ->
        advance ();
        advance_digits ()
    | Some actual ->
        error_at
          (Input.position state.input)
          ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' while parsing number")
    | None -> unexpected_end state "number"
  );
  let is_float = ref false in
  (
    match current () with
    | Some '.' ->
        is_float := true;
        advance ();
        (
          match current () with
          | Some digit when is_digit digit ->
              advance ();
              advance_digits ()
          | Some actual ->
              error_at
                (Input.position state.input)
                ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' after decimal point")
          | None -> unexpected_end state "digit after decimal point"
        )
    | _ -> ()
  );
  (
    match current () with
    | Some ('e' | 'E' as _exponent) ->
        is_float := true;
        advance ();
        (
          match current () with
          | Some ('+' | '-' as _sign) -> advance ()
          | _ -> ()
        );
        (
          match current () with
          | Some digit when is_digit digit ->
              advance ();
              advance_digits ()
          | Some actual ->
              error_at
                (Input.position state.input)
                ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' after exponent")
          | None -> unexpected_end state "digit after exponent"
        )
    | _ -> ()
  );
  if not (is_value_delimiter (current ())) then (
    match current () with
    | Some actual -> unexpected_character state actual "number delimiter"
    | None -> ()
  );
  Input.set_position state.input !pos;
  { text = String.sub input ~offset:start ~len:(!pos - start); is_float = !is_float }

let parse_number_text = fun state ->
  match state.input with
  | Input.Reader_input reader -> parse_number_text_reader state reader
  | Input.String_input _ -> parse_number_text_generic state

let invalid_field_type = fun () -> raise (Serde.Decode_error `invalid_field_type)

let parse_int_generic = fun state ->
  Input.skip_whitespace state.input;
  let input =
    match state.input with
    | Input.String_input state -> state.input
    | Input.Reader_input _ -> panic "parse_int_generic: expected string input"
  in
  let input_length = String.length input in
  let pos = ref (Input.position state.input) in
  let current () =
    if !pos < input_length then
      Some (String.unsafe_get input !pos)
    else
      None
  in
  let advance () =
    pos := !pos + 1
  in
  let negative =
    match current () with
    | Some '-' ->
        advance ();
        true
    | _ -> false
  in
  let limit =
    if negative then
      Int.min_int
    else
      -Int.max_int
  in
  let cutoff = limit / 10 in
  let acc = ref 0 in
  let push_digit digit =
    if !acc < cutoff then
      invalid_field_type ();
    let next = (!acc * 10) - digit in
    if next < limit then
      invalid_field_type ();
    acc := next
  in
  let advance_digit digit =
    push_digit digit;
    advance ()
  in
  let rec advance_digits () =
    match current () with
    | Some digit when is_digit digit ->
        advance_digit (Char.code digit - Char.code '0');
        advance_digits ()
    | _ -> ()
  in
  (
    match current () with
    | Some '0' ->
        advance ();
        (
          match current () with
          | Some digit when is_digit digit ->
              error_at (Input.position state.input) "leading zeros are not allowed in JSON numbers"
          | Some ('.' | 'e' | 'E') -> invalid_field_type ()
          | _ -> ()
        )
    | Some ('1' .. '9' as digit) ->
        advance_digit (Char.code digit - Char.code '0');
        advance_digits ();
        (
          match current () with
          | Some ('.' | 'e' | 'E') -> invalid_field_type ()
          | _ -> ()
        )
    | Some actual ->
        error_at
          (Input.position state.input)
          ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' while parsing number")
    | None -> unexpected_end state "number"
  );
  if not (is_value_delimiter (current ())) then (
    match current () with
    | Some actual -> unexpected_character state actual "number delimiter"
    | None -> ()
  );
  Input.set_position state.input !pos;
  if negative then
    !acc
  else
    - !acc

let parse_int_reader = fun state reader ->
  let fallback () =
    let number = parse_number_text_reader_slow state reader in
    if number.is_float then
      invalid_field_type ()
    else
      match Int.parse number.text with
      | Some value -> value
      | None -> invalid_field_type ()
  in
  let start = ref reader.Input.pos in
  try
    reader_skip_whitespace reader;
    start := reader.Input.pos;
    let pos = ref reader.Input.pos in
    let current () =
      if !pos < IO.Buffer.readable_bytes reader.Input.buffer then
        Some (IO.Buffer.get_unchecked reader.Input.buffer ~at:!pos)
      else
        None
    in
    let need_more () =
      !pos >= IO.Buffer.readable_bytes reader.Input.buffer && not reader.Input.eof
    in
    let advance () =
      pos := !pos + 1
    in
    let negative =
      match current () with
      | Some '-' ->
          advance ();
          if need_more () then
            raise Use_slow_number_path;
          true
      | _ -> false
    in
    let limit =
      if negative then
        Int.min_int
      else
        -Int.max_int
    in
    let cutoff = limit / 10 in
    let acc = ref 0 in
    let push_digit digit =
      if !acc < cutoff then
        invalid_field_type ();
      let next = (!acc * 10) - digit in
      if next < limit then
        invalid_field_type ();
      acc := next
    in
    let advance_digit digit =
      push_digit digit;
      advance ()
    in
    let rec advance_digits () =
      match current () with
      | Some digit when is_digit digit ->
          advance_digit (Char.code digit - Char.code '0');
          advance_digits ()
      | _ -> ()
    in
    (
      match current () with
      | Some '0' ->
          advance ();
          if need_more () then
            raise Use_slow_number_path;
          (
            match current () with
            | Some digit when is_digit digit ->
                error_at
                  (Input.position state.input)
                  "leading zeros are not allowed in JSON numbers"
            | Some ('.' | 'e' | 'E') -> invalid_field_type ()
            | _ -> ()
          )
      | Some ('1' .. '9' as digit) ->
          advance_digit (Char.code digit - Char.code '0');
          advance_digits ();
          if need_more () then
            raise Use_slow_number_path;
          (
            match current () with
            | Some ('.' | 'e' | 'E') -> invalid_field_type ()
            | _ -> ()
          )
      | Some actual ->
          error_at
            (Input.position state.input)
            ("unexpected '" ^ String.make ~len:1 ~char:actual ^ "' while parsing number")
      | None -> unexpected_end state "number"
    );
    if not (is_value_delimiter (current ())) then (
      match current () with
      | Some actual -> unexpected_character state actual "number delimiter"
      | None -> ()
    );
    reader.Input.pos <- !pos;
    if negative then
      !acc
    else
      - !acc
  with
  | Use_slow_number_path ->
      reader.Input.pos <- !start;
      fallback ()

let parse_int64 = fun state ->
  let number = parse_number_text state in
  if number.is_float then
    invalid_field_type ()
  else
    match Int64.from_string_opt number.text with
    | Some value -> value
    | None -> invalid_field_type ()

let parse_int = fun state ->
  match state.input with
  | Input.Reader_input reader -> parse_int_reader state reader
  | Input.String_input _ -> parse_int_generic state

let parse_int32 = fun state ->
  let number = parse_number_text state in
  if number.is_float then
    invalid_field_type ()
  else
    match Int32.from_string_opt number.text with
    | Some value -> value
    | None -> invalid_field_type ()

let parse_float_generic = fun state ->
  let number = parse_number_text_generic state in
  match Float.parse number.text with
  | Some value -> value
  | None -> invalid_field_type ()

let parse_float_reader = fun state reader ->
  let number = parse_number_text_reader_slow state reader in
  match Float.parse number.text with
  | Some value -> value
  | None -> invalid_field_type ()

let parse_float = fun state ->
  match state.input with
  | Input.Reader_input reader -> parse_float_reader state reader
  | Input.String_input _ -> parse_float_generic state

let rec skip_value_reader = fun state reader ->
  reader_skip_whitespace reader;
  match reader_current_char reader with
  | Some '{' ->
      reader_advance reader;
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some '}' -> reader_advance reader
        | _ ->
            let rec loop first =
              if first then
                ()
              else
                reader_expect_char state reader ',' "object delimiter";
              reader_skip_whitespace reader;
              skip_string_reader state reader;
              reader_skip_whitespace reader;
              reader_expect_char state reader ':' "':' after object key";
              skip_value_reader state reader;
              reader_skip_whitespace reader;
              match reader_current_char reader with
              | Some '}' -> reader_advance reader
              | Some _ -> loop false
              | None -> unexpected_end state "object"
            in
            loop true
      )
  | Some '[' ->
      reader_advance reader;
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some ']' -> reader_advance reader
        | _ ->
            let rec loop first =
              if first then
                ()
              else
                reader_expect_char state reader ',' "array delimiter";
              skip_value_reader state reader;
              reader_skip_whitespace reader;
              match reader_current_char reader with
              | Some ']' -> reader_advance reader
              | Some _ -> loop false
              | None -> unexpected_end state "array"
            in
            loop true
      )
  | Some '"' -> skip_string_reader state reader
  | Some ('-' | '0' .. '9') -> ignore (parse_float state)
  | Some 't' -> reader_expect_literal state reader "true"
  | Some 'f' -> reader_expect_literal state reader "false"
  | Some 'n' -> reader_expect_literal state reader "null"
  | Some actual -> unexpected_character state actual "JSON value"
  | None -> unexpected_end state "JSON value"

let rec skip_value = fun state ->
  match state.input with
  | Input.Reader_input reader -> skip_value_reader state reader
  | Input.String_input _ ->
      Input.skip_whitespace state.input;
      match Input.current_char state.input with
      | Some '{' ->
          Input.advance state.input;
          Input.skip_whitespace state.input;
          (
            match Input.current_char state.input with
            | Some '}' -> Input.advance state.input
            | _ ->
                let rec loop first =
                  if first then
                    ()
                  else
                    expect_char state ',' "object delimiter";
                  Input.skip_whitespace state.input;
                  skip_string state;
                  expect_char state ':' "':' after object key";
                  skip_value state;
                  Input.skip_whitespace state.input;
                  match Input.current_char state.input with
                  | Some '}' -> Input.advance state.input
                  | Some _ -> loop false
                  | None -> unexpected_end state "object"
                in
                loop true
          )
      | Some '[' ->
          Input.advance state.input;
          Input.skip_whitespace state.input;
          (
            match Input.current_char state.input with
            | Some ']' -> Input.advance state.input
            | _ ->
                let rec loop first =
                  if first then
                    ()
                  else
                    expect_char state ',' "array delimiter";
                  skip_value state;
                  Input.skip_whitespace state.input;
                  match Input.current_char state.input with
                  | Some ']' -> Input.advance state.input
                  | Some _ -> loop false
                  | None -> unexpected_end state "array"
                in
                loop true
          )
      | Some '"' -> skip_string state
      | Some ('-' | '0' .. '9') -> ignore (parse_float state)
      | Some 't' -> expect_literal state "true"
      | Some 'f' -> expect_literal state "false"
      | Some 'n' -> expect_literal state "null"
      | Some actual -> unexpected_character state actual "JSON value"
      | None -> unexpected_end state "JSON value"

let rec option_backend: 'value. state -> 'value De.t -> 'value option = fun state decode ->
  match state.input with
  | Input.Reader_input reader ->
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some 'n' ->
            reader_expect_literal state reader "null";
            None
        | _ -> Some (decode.run backend state)
      )
  | Input.String_input _ ->
      Input.skip_whitespace state.input;
      (
        match Input.current_char state.input with
        | Some 'n' ->
            parse_null state;
            None
        | _ -> Some (decode.run backend state)
      )

and list_backend: 'value. state -> 'value De.t -> 'value vec = fun state decode ->
  match state.input with
  | Input.Reader_input reader ->
      reader_skip_whitespace reader;
      reader_expect_char state reader '[' "array";
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some ']' ->
            reader_advance reader;
            Vector.create ()
        | _ ->
            let acc = Vector.with_capacity ~size:8 in
            let finished = ref false in
            while not !finished do
              let value = decode.run backend state in
              Vector.push acc ~value;
              reader_skip_whitespace reader;
              match reader_current_char reader with
              | Some ',' -> reader_advance reader
              | Some ']' ->
                  reader_advance reader;
                  finished := true
              | Some actual -> unexpected_character state actual "array delimiter"
              | None -> unexpected_end state "array"
            done;
            acc
      )
  | Input.String_input _ ->
      skip_then_expect_char state '[' "array";
      Input.skip_whitespace state.input;
      match Input.current_char state.input with
      | Some ']' ->
          Input.advance state.input;
          Vector.create ()
      | _ ->
          let acc = Vector.with_capacity ~size:8 in
          let finished = ref false in
          while not !finished do
            let value = decode.run backend state in
            Vector.push acc ~value;
            Input.skip_whitespace state.input;
            match Input.current_char state.input with
            | Some ',' -> Input.advance state.input
            | Some ']' ->
                Input.advance state.input;
                finished := true
            | Some actual -> unexpected_character state actual "array delimiter"
            | None -> unexpected_end state "array"
          done;
          acc

and array_backend: 'value. state -> 'value De.t -> 'value array = fun state decode ->
  let values = list_backend state decode in
  Vector.to_array values

and map_backend: 'value. state -> 'value De.t -> (string * 'value) vec = fun state decode ->
  match state.input with
  | Input.Reader_input reader ->
      reader_skip_whitespace reader;
      reader_expect_char state reader '{' "object";
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some '}' ->
            reader_advance reader;
            Vector.create ()
        | _ ->
            let acc = Vector.with_capacity ~size:8 in
            let first = ref true in
            let finished = ref false in
            while not !finished do
              if !first then
                first := false
              else (
                reader_skip_whitespace reader;
                reader_expect_char state reader ',' "object delimiter"
              );
              let key = parse_string_reader state reader in
              reader_skip_whitespace reader;
              reader_expect_char state reader ':' "':' after object key";
              let value = decode.run backend state in
              Vector.push acc ~value:(key, value);
              reader_skip_whitespace reader;
              match reader_current_char reader with
              | Some '}' ->
                  reader_advance reader;
                  finished := true
              | Some _ -> ()
              | None -> unexpected_end state "object"
            done;
            acc
      )
  | Input.String_input _ ->
      skip_then_expect_char state '{' "object";
      Input.skip_whitespace state.input;
      match Input.current_char state.input with
      | Some '}' ->
          Input.advance state.input;
          Vector.create ()
      | _ ->
          let acc = Vector.with_capacity ~size:8 in
          let first = ref true in
          let finished = ref false in
          while not !finished do
            if !first then
              first := false
            else (
              Input.skip_whitespace state.input;
              expect_char state ',' "object delimiter"
            );
            let key = parse_string state in
            skip_then_expect_char state ':' "':' after object key";
            let value = decode.run backend state in
            Vector.push acc ~value:(key, value);
            Input.skip_whitespace state.input;
            match Input.current_char state.input with
            | Some '}' ->
                Input.advance state.input;
                finished := true
            | Some _ -> ()
            | None -> unexpected_end state "object"
          done;
          acc

and record_backend:
  'field 'acc 'value. state ->
  fields:'field De.Fields.t ->
  init:'acc ->
  step:('acc -> 'field option -> 'acc) ->
  finish:('acc -> 'value) ->
  'value = fun state ~fields ~init ~step ~finish ->
  match state.input with
  | Input.Reader_input reader ->
      reader_skip_whitespace reader;
      reader_expect_char state reader '{' "object";
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some '}' ->
            reader_advance reader;
            finish init
        | _ ->
            let acc = ref init in
            let first = ref true in
            let finished = ref false in
            while not !finished do
              if !first then
                first := false
              else (
                reader_skip_whitespace reader;
                reader_expect_char state reader ',' "object delimiter"
              );
              let field = read_field_tag_reader state reader fields in
              reader_skip_whitespace reader;
              reader_expect_char state reader ':' "':' after object key";
              acc := step !acc field;
              reader_skip_whitespace reader;
              match reader_current_char reader with
              | Some '}' ->
                  reader_advance reader;
                  finished := true
              | Some _ -> ()
              | None -> unexpected_end state "object"
            done;
            finish !acc
      )
  | Input.String_input _ ->
      skip_then_expect_char state '{' "object";
      Input.skip_whitespace state.input;
      match Input.current_char state.input with
      | Some '}' ->
          Input.advance state.input;
          finish init
      | _ ->
          let acc = ref init in
          let first = ref true in
          let finished = ref false in
          while not !finished do
            if !first then
              first := false
            else (
              Input.skip_whitespace state.input;
              expect_char state ',' "object delimiter"
            );
            let field = read_field_tag state fields in
            skip_then_expect_char state ':' "':' after object key";
            acc := step !acc field;
            Input.skip_whitespace state.input;
            match Input.current_char state.input with
            | Some '}' ->
                Input.advance state.input;
                finished := true
            | Some _ -> ()
            | None -> unexpected_end state "object"
          done;
          finish !acc

and record_mut_backend:
  'field 'builder 'value. state ->
  fields:'field De.Fields.t ->
  create:(unit -> 'builder) ->
  step:('builder -> 'field option -> unit) ->
  finish:('builder -> 'value) ->
  'value = fun state ~fields ~create ~step ~finish ->
  match state.input with
  | Input.Reader_input reader ->
      reader_skip_whitespace reader;
      reader_expect_char state reader '{' "object";
      reader_skip_whitespace reader;
      let builder = create () in
      (
        match reader_current_char reader with
        | Some '}' ->
            reader_advance reader;
            finish builder
        | _ ->
            let first = ref true in
            let finished = ref false in
            while not !finished do
              if !first then
                first := false
              else (
                reader_skip_whitespace reader;
                reader_expect_char state reader ',' "object delimiter"
              );
              let field = read_field_tag_reader state reader fields in
              reader_skip_whitespace reader;
              reader_expect_char state reader ':' "':' after object key";
              step builder field;
              reader_skip_whitespace reader;
              match reader_current_char reader with
              | Some '}' ->
                  reader_advance reader;
                  finished := true
              | Some _ -> ()
              | None -> unexpected_end state "object"
            done;
            finish builder
      )
  | Input.String_input _ ->
      skip_then_expect_char state '{' "object";
      Input.skip_whitespace state.input;
      let builder = create () in
      match Input.current_char state.input with
      | Some '}' ->
          Input.advance state.input;
          finish builder
      | _ ->
          let first = ref true in
          let finished = ref false in
          while not !finished do
            if !first then
              first := false
            else (
              Input.skip_whitespace state.input;
              expect_char state ',' "object delimiter"
            );
            let field = read_field_tag state fields in
            skip_then_expect_char state ':' "':' after object key";
            step builder field;
            Input.skip_whitespace state.input;
            match Input.current_char state.input with
            | Some '}' ->
                Input.advance state.input;
                finished := true
            | Some _ -> ()
            | None -> unexpected_end state "object"
          done;
          finish builder

and variant_backend: 'value. state -> 'value De.compiled_variant_cases -> 'value = fun
  state cases ->
  let find_unit tag =
    let rec loop index =
      if Int.equal index (Array.length cases) then
        raise (Serde.Decode_error `invalid_tag)
      else
        match Array.get_unchecked cases ~at:index with
        | De.Unit (expected, value) when String.equal expected tag -> value
        | _ -> loop (index + 1)
    in
    loop 0
  in
  let find_object tag =
    let rec loop index =
      if Int.equal index (Array.length cases) then
        raise (Serde.Decode_error `invalid_tag)
      else
        match Array.get_unchecked cases ~at:index with
        | De.Unit (expected, value) when String.equal expected tag ->
            parse_null state;
            value
        | De.Newtype (expected, decode, wrap) when String.equal expected tag ->
            wrap (decode.run backend state)
        | _ -> loop (index + 1)
    in
    loop 0
  in
  match state.input with
  | Input.Reader_input reader ->
      reader_skip_whitespace reader;
      (
        match reader_current_char reader with
        | Some '"' ->
            let tag = parse_string_reader state reader in
            find_unit tag
        | Some '{' ->
            reader_expect_char state reader '{' "variant object";
            let tag = parse_string_reader state reader in
            reader_skip_whitespace reader;
            reader_expect_char state reader ':' "':' after variant tag";
            let value = find_object tag in
            reader_skip_whitespace reader;
            reader_expect_char state reader '}' "closing '}' for variant";
            value
        | Some actual -> unexpected_character state actual "variant"
        | None -> unexpected_end state "variant"
      )
  | Input.String_input _ ->
      Input.skip_whitespace state.input;
      match Input.current_char state.input with
      | Some '"' ->
          let tag = parse_string state in
          find_unit tag
      | Some '{' ->
          expect_char state '{' "variant object";
          let tag = parse_string state in
          skip_then_expect_char state ':' "':' after variant tag";
          let value = find_object tag in
          skip_then_expect_char state '}' "closing '}' for variant";
          value
      | Some actual -> unexpected_character state actual "variant"
      | None -> unexpected_end state "variant"

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
  array = array_backend;
  map = map_backend;
  record = record_backend;
  record_mut = record_mut_backend;
  variant = variant_backend;
}

let finish = fun state value ->
  Input.skip_whitespace state.input;
  match Input.current_char state.input with
  | None -> Ok value
  | Some _ ->
      Error (`Msg ("extra input after JSON value at position "
      ^ Int.to_string (Input.position state.input)))

let from_input = fun decode input ->
  let state = { input; scratch = IO.Buffer.create ~size:64 } in
  let* value = De.run decode backend state in
  finish state value

let from_string = fun decode input -> from_input decode (Input.from_string input)

let from_reader = fun decode reader -> from_input decode (Input.from_reader reader)
