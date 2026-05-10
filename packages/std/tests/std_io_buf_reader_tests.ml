open Std

let slice_to_string = IO.IoSlice.to_string

let rune_to_int = Unicode.Rune.to_int

let rune_from_int = fun n ->
  Unicode.Rune.from_int n
  |> Option.unwrap

let test_read_line_preserves_leftover_content = fun _ctx ->
  let reader =
    IO.Reader.from_string "alpha\nbeta\ngamma"
    |> IO.BufReader.from_reader ~size:8
  in
  match IO.BufReader.read_line reader with
  | Ok line when String.equal (slice_to_string line) "alpha\n" ->
      match IO.BufReader.read_byte reader with
      | Ok 'b' ->
          match IO.BufReader.read_line reader with
          | Ok line when String.equal (slice_to_string line) "eta\n" -> Ok ()
          | Ok _ -> Error "BufReader.read_line should continue from the buffered leftover"
          | Error _ -> Error "BufReader.read_line should not fail for in-memory readers"
      | Ok _ -> Error "BufReader.read_byte should read from the leftover buffer"
      | Error _ -> Error "BufReader.read_byte should not fail for in-memory readers"
  | Ok _ -> Error "BufReader.read_line should preserve newline-terminated lines"
  | Error _ -> Error "BufReader.read_line should not fail for in-memory readers"

let test_peek_and_consume_use_buffered_window = fun _ctx ->
  let reader =
    IO.Reader.from_string "abcdef"
    |> IO.BufReader.from_reader ~size:4
  in
  match IO.BufReader.peek reader ~len:4 with
  | Ok slice when String.equal (slice_to_string slice) "abcd" ->
      match IO.BufReader.consume reader ~len:2 with
      | Ok 2 ->
          match IO.BufReader.peek reader ~len:2 with
          | Ok slice when String.equal (slice_to_string slice) "cd" -> Ok ()
          | Ok _ -> Error "BufReader.peek should expose the remaining borrowed bytes after consume"
          | Error _ -> Error "BufReader.peek should not fail for in-memory readers"
      | Ok _ -> Error "BufReader.consume should discard the requested prefix"
      | Error _ -> Error "BufReader.consume should not fail for in-memory readers"
  | Ok _ -> Error "BufReader.peek should expose the first buffered chunk"
  | Error _ -> Error "BufReader.peek should not fail for in-memory readers"

let test_read_slice_returns_borrowed_delimited_chunks = fun _ctx ->
  let reader =
    IO.Reader.from_string "alpha\nbeta\ngamma"
    |> IO.BufReader.from_reader ~size:8
  in
  match IO.BufReader.read_slice reader ~until:'\n' with
  | Ok first when String.equal (slice_to_string first) "alpha\n" ->
      match IO.BufReader.read_line reader with
      | Ok second when String.equal (slice_to_string second) "beta\n" ->
          match IO.BufReader.read_slice reader ~until:'\n' with
          | Ok tail when String.equal (slice_to_string tail) "gamma" -> Ok ()
          | Ok _ -> Error "BufReader.read_slice should return the remaining tail at EOF"
          | Error _ -> Error "BufReader.read_slice should not fail for in-memory readers"
      | Ok _ -> Error "BufReader.read_line should return the next borrowed line"
      | Error _ -> Error "BufReader.read_line should not fail for in-memory readers"
  | Ok _ -> Error "BufReader.read_slice should return a delimiter-terminated borrowed chunk"
  | Error _ -> Error "BufReader.read_slice should not fail for in-memory readers"

let test_read_string_materializes_delimited_chunks = fun _ctx ->
  let reader =
    IO.Reader.from_string "alpha\nbeta"
    |> IO.BufReader.from_reader ~size:8
  in
  match IO.BufReader.read_string reader ~until:'\n' with
  | Ok "alpha\n" ->
      match IO.BufReader.read_string reader ~until:'\n' with
      | Ok "beta" -> Ok ()
      | Ok _ -> Error "BufReader.read_string should materialize the remaining tail at EOF"
      | Error _ -> Error "BufReader.read_string should not fail for in-memory readers"
  | Ok _ -> Error "BufReader.read_string should materialize delimiter-terminated chunks"
  | Error _ -> Error "BufReader.read_string should not fail for in-memory readers"

let test_to_reader_round_trips_through_generic_io = fun _ctx ->
  let reader =
    IO.Reader.from_string "hello\nworld"
    |> IO.BufReader.from_reader ~size:3
    |> IO.BufReader.to_reader
  in
  let prefix = IO.Buffer.create ~size:6 in
  match IO.Reader.read_exact reader ~into:prefix ~len:6 with
  | Ok () when String.equal (IO.Buffer.contents prefix) "hello\n" ->
      let suffix = IO.Buffer.create ~size:5 in
      match IO.Reader.read_to_end reader ~into:suffix with
      | Ok 5 when String.equal (IO.Buffer.contents suffix) "world" -> Ok ()
      | Ok _ -> Error "BufReader.to_reader should preserve generic buffered reads"
      | Error _ -> Error "BufReader.to_reader should not fail for in-memory readers"
  | Ok () -> Error "BufReader.to_reader should preserve the first buffered chunk"
  | Error _ -> Error "BufReader.to_reader should not fail for in-memory readers"

let test_read_copies_into_owned_buffers = fun _ctx ->
  let reader =
    IO.Reader.from_string "abcdef"
    |> IO.BufReader.from_reader ~size:4
  in
  let first = IO.Buffer.create ~size:2 in
  match IO.BufReader.read reader ~into:first with
  | Ok 2 when String.equal (IO.Buffer.contents first) "ab" ->
      let second = IO.Buffer.create ~size:4 in
      match IO.BufReader.read reader ~into:second with
      | Ok 2 when String.equal (IO.Buffer.contents second) "cd" ->
          let third = IO.Buffer.create ~size:4 in
          match IO.BufReader.read reader ~into:third with
          | Ok 2 when String.equal (IO.Buffer.contents third) "ef" -> Ok ()
          | Ok _ -> Error "BufReader.read should refill after the buffered chunk is drained"
          | Error _ -> Error "BufReader.read should not fail when refilling from in-memory readers"
      | Ok _ -> Error "BufReader.read should drain the buffered bytes before the next refill"
      | Error _ -> Error "BufReader.read should not fail for in-memory readers"
  | Ok _ -> Error "BufReader.read should respect the destination buffer capacity"
  | Error _ -> Error "BufReader.read should not fail for in-memory readers"

let test_fill_reports_buffered_bytes = fun _ctx ->
  let reader =
    IO.Reader.from_string "abcdef"
    |> IO.BufReader.from_reader ~size:4
  in
  if not (Int.equal (IO.BufReader.size reader) 4) then
    Error "BufReader.size should report the configured capacity"
  else
    match IO.BufReader.fill reader with
    | Ok 4 ->
        match IO.BufReader.fill reader with
        | Ok 4 -> Ok ()
        | Ok _ -> Error "BufReader.fill should report the already buffered bytes without refilling"
        | Error _ -> Error "BufReader.fill should not fail while data is buffered"
    | Ok _ -> Error "BufReader.fill should load the first buffered chunk"
    | Error _ -> Error "BufReader.fill should not fail for in-memory readers"

let test_buffered_exposes_current_window = fun _ctx ->
  let reader =
    IO.Reader.from_string "abcdef"
    |> IO.BufReader.from_reader ~size:4
  in
  match IO.BufReader.buffered reader with
  | Ok slice when String.equal (slice_to_string slice) "abcd" ->
      match IO.BufReader.consume reader ~len:2 with
      | Ok 2 ->
          match IO.BufReader.buffered reader with
          | Ok slice when String.equal (slice_to_string slice) "cd" -> Ok ()
          | Ok _ -> Error "BufReader.buffered should expose the unread suffix after consume"
          | Error _ -> Error "BufReader.buffered should not fail while bytes remain buffered"
      | Ok _ -> Error "BufReader.consume should discard the requested prefix from buffered slices"
      | Error _ -> Error "BufReader.consume should not fail for in-memory readers"
  | Ok _ -> Error "BufReader.buffered should expose the full current buffered window"
  | Error _ -> Error "BufReader.buffered should not fail for in-memory readers"

let test_reset_switches_to_a_new_reader = fun _ctx ->
  let reader =
    IO.Reader.from_string "alpha\n"
    |> IO.BufReader.from_reader ~size:8
  in
  match IO.BufReader.read_line reader with
  | Ok line when String.equal (slice_to_string line) "alpha\n" ->
      IO.BufReader.reset reader ~reader:(IO.Reader.from_string "beta\n");
      match IO.BufReader.read_line reader with
      | Ok line when String.equal (slice_to_string line) "beta\n" -> Ok ()
      | Ok _ ->
          Error "BufReader.reset should replace the underlying reader and clear buffered bytes"
      | Error _ -> Error "BufReader.reset should not fail for in-memory readers"
  | Ok _ -> Error "BufReader.read_line should consume the old reader before reset"
  | Error _ -> Error "BufReader.read_line should not fail for in-memory readers"

let test_read_rune_decodes_utf8_sequences = fun _ctx ->
  let payload = "A" ^ "\xC3\xA9" ^ "\xF0\x9F\x98\x80" in
  let reader =
    IO.Reader.from_string payload
    |> IO.BufReader.from_reader ~size:8
  in
  match IO.BufReader.read_rune reader with
  | Ok rune when rune_to_int rune = rune_to_int (Unicode.Rune.from_char 'A') ->
      match IO.BufReader.read_rune reader with
      | Ok rune when rune_to_int rune = rune_to_int (rune_from_int 0xe9) ->
          match IO.BufReader.read_rune reader with
          | Ok rune when rune_to_int rune = rune_to_int (rune_from_int 0x1_f600) -> Ok ()
          | Ok _ -> Error "BufReader.read_rune should decode multi-byte UTF-8 sequences"
          | Error _ -> Error "BufReader.read_rune should not fail for valid UTF-8"
      | Ok _ -> Error "BufReader.read_rune should decode two-byte UTF-8 sequences"
      | Error _ -> Error "BufReader.read_rune should not fail for valid UTF-8"
  | Ok _ -> Error "BufReader.read_rune should decode ASCII bytes as single-byte runes"
  | Error _ -> Error "BufReader.read_rune should not fail for valid UTF-8"

let test_read_rune_reports_invalid_data = fun _ctx ->
  let reader =
    IO.Reader.from_string "\xFF"
    |> IO.BufReader.from_reader ~size:4
  in
  match IO.BufReader.read_rune reader with
  | Error IO.Invalid_data -> Ok ()
  | Ok _ -> Error "BufReader.read_rune should reject invalid UTF-8 leading bytes"
  | Error _ -> Error "BufReader.read_rune should report invalid UTF-8 as Invalid_data"

let test_read_slice_reports_buffer_full_when_delimiter_does_not_fit = fun _ctx ->
  let reader =
    IO.Reader.from_string "abcdef"
    |> IO.BufReader.from_reader ~size:4
  in
  match IO.BufReader.read_slice reader ~until:'\n' with
  | Error IO.Buffer_full -> Ok ()
  | Ok _ ->
      Error "BufReader.read_slice should not return a borrowed chunk when the buffer fills first"
  | Error _ ->
      Error "BufReader.read_slice should report Buffer_full when the delimiter does not fit"

let test_peek_and_consume_validate_counts = fun _ctx ->
  let reader =
    IO.Reader.from_string "abcdef"
    |> IO.BufReader.from_reader ~size:4
  in
  match IO.BufReader.peek reader ~len:5 with
  | Error IO.Buffer_full ->
      match IO.BufReader.consume reader ~len:(-1) with
      | Error IO.Invalid_argument -> Ok ()
      | Ok _ -> Error "BufReader.consume should reject negative counts"
      | Error _ -> Error "BufReader.consume should report Invalid_argument for negative lengths"
  | Ok _ -> Error "BufReader.peek should require enough room for the requested exact length"
  | Error _ -> Error "BufReader.peek should report Buffer_full when len exceeds capacity"

let tests =
  Test.[
    case "BufReader.read_line preserves leftover content" test_read_line_preserves_leftover_content;
    case
      "BufReader.peek and consume use the buffered window"
      test_peek_and_consume_use_buffered_window;
    case
      "BufReader.read_slice returns borrowed delimited chunks"
      test_read_slice_returns_borrowed_delimited_chunks;
    case
      "BufReader.read_string materializes delimited chunks"
      test_read_string_materializes_delimited_chunks;
    case
      "BufReader.to_reader round trips through Std.IO"
      test_to_reader_round_trips_through_generic_io;
    case "BufReader.read copies into owned buffers" test_read_copies_into_owned_buffers;
    case "BufReader.fill reports buffered bytes" test_fill_reports_buffered_bytes;
    case
      "BufReader.buffered exposes the current borrowed window"
      test_buffered_exposes_current_window;
    case "BufReader.reset switches to a new reader" test_reset_switches_to_a_new_reader;
    case "BufReader.read_rune decodes UTF-8 sequences" test_read_rune_decodes_utf8_sequences;
    case "BufReader.read_rune reports invalid data" test_read_rune_reports_invalid_data;
    case
      "BufReader.read_slice reports Buffer_full when delimiter does not fit"
      test_read_slice_reports_buffer_full_when_delimiter_does_not_fit;
    case "BufReader.peek and consume validate counts" test_peek_and_consume_validate_counts;
  ]

let main ~args = Test.Cli.main ~name:"IO.BufReader" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
