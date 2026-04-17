open Std

let test_read_line_preserves_leftover_content = fun _ctx ->
  let reader =
    IO.Reader.from_string "alpha\nbeta\ngamma"
    |> IO.BufferedReader.of_reader ~chunk_size:4
  in
  match IO.BufferedReader.read_line reader with
  | Ok "alpha\n" -> (
      match IO.BufferedReader.read_char reader with
      | Ok (Some 'b') -> (
          match IO.BufferedReader.read_line reader with
          | Ok "eta\n" -> Ok ()
          | Ok _ -> Error "BufferedReader.read_line should continue from the buffered leftover"
          | Error () -> Error "BufferedReader.read_line should not fail for in-memory readers")
      | Ok _ -> Error "BufferedReader.read_char should read from the leftover buffer"
      | Error () -> Error "BufferedReader.read_char should not fail for in-memory readers")
  | Ok _ -> Error "BufferedReader.read_line should preserve newline-terminated lines"
  | Error () -> Error "BufferedReader.read_line should not fail for in-memory readers"

let test_read_to_string_consumes_buffered_content_before_refilling = fun _ctx ->
  let reader =
    IO.Reader.from_string "abcdef"
    |> IO.BufferedReader.of_reader ~chunk_size:4
  in
  match IO.BufferedReader.read_char reader with
  | Ok (Some 'a') -> (
      match IO.BufferedReader.read_to_string reader ~len:3 with
      | Ok "bcd" -> (
          match IO.BufferedReader.read_to_string reader ~len:3 with
          | Ok "ef" -> Ok ()
          | Ok _ -> Error "BufferedReader.read_to_string should return the remaining tail at EOF"
          | Error () -> Error "BufferedReader.read_to_string should not fail for in-memory readers")
      | Ok _ -> Error "BufferedReader.read_to_string should consume buffered leftovers first"
      | Error () -> Error "BufferedReader.read_to_string should not fail for in-memory readers")
  | Ok _ -> Error "BufferedReader.read_char should read the first character"
  | Error () -> Error "BufferedReader.read_char should not fail for in-memory readers"

let test_to_reader_round_trips_through_generic_io = fun _ctx ->
  let reader =
    IO.Reader.from_string "hello\nworld"
    |> IO.BufferedReader.of_reader ~chunk_size:3
    |> IO.BufferedReader.to_reader
  in
  match IO.read_line reader with
  | Ok "hello\n" -> (
      match IO.read_to_string reader ~len:5 with
      | Ok "world" -> Ok ()
      | Ok _ -> Error "BufferedReader.to_reader should preserve generic read_to_string behavior"
      | Error () -> Error "BufferedReader.to_reader should preserve generic read_to_string behavior")
  | Ok _ -> Error "BufferedReader.to_reader should preserve generic line reads"
  | Error () -> Error "BufferedReader.to_reader should preserve generic line reads"

let tests = Test.[
  case "BufferedReader.read_line preserves leftover content" test_read_line_preserves_leftover_content;
  case "BufferedReader.read_to_string consumes buffered content before refilling" test_read_to_string_consumes_buffered_content_before_refilling;
  case "BufferedReader.to_reader round trips through Std.IO" test_to_reader_round_trips_through_generic_io;
]

let () =
  Runtime.run
    ~main:(fun ~args -> Test.Cli.main ~name:"IO.BufferedReader" ~tests ~args)
    ~args:Env.args
    ()
