open Std

let test_stdin_reader_zero_length_read_is_a_noop = fun _ctx ->
  let reader = IO.stdin () in
  let buffer = IO.Bytes.create ~size:0 in
  match IO.read reader buffer with
  | Ok 0 -> Ok ()
  | Ok _ -> Error "IO.stdin should return a reader that treats zero-length reads as a no-op"
  | Error _ -> Error "IO.stdin zero-length reads should not fail"

let test_stdout_writer_handles_empty_operations = fun _ctx ->
  let writer = IO.stdout () in
  let empty_iov = IO.Iovec.from_string_array [||] in
  match IO.write writer ~buf:"" with
  | Ok 0 -> (
      match IO.write_all writer ~buf:"" with
      | Ok () -> (
          match IO.write_owned_vectored writer ~bufs:empty_iov with
          | Ok 0 -> (
              match IO.write_all_vectored writer ~bufs:empty_iov with
              | Ok () -> (
                  match IO.flush writer with
                  | Ok () -> Ok ()
                  | Error _ -> Error "IO.stdout flush should succeed for empty operations")
              | Error _ -> Error "IO.stdout write_all_vectored should accept empty iovecs")
          | Ok _ -> Error "IO.stdout write_owned_vectored should report zero for empty iovecs"
          | Error _ -> Error "IO.stdout write_owned_vectored should accept empty iovecs")
      | Error _ -> Error "IO.stdout write_all should accept empty strings")
  | Ok _ -> Error "IO.stdout write should report zero for empty strings"
  | Error _ -> Error "IO.stdout write should accept empty strings"

let test_stderr_writer_handles_empty_operations = fun _ctx ->
  let writer = IO.stderr () in
  let empty_iov = IO.Iovec.from_string_array [||] in
  match IO.write writer ~buf:"" with
  | Ok 0 -> (
      match IO.write_all writer ~buf:"" with
      | Ok () -> (
          match IO.write_owned_vectored writer ~bufs:empty_iov with
          | Ok 0 -> (
              match IO.write_all_vectored writer ~bufs:empty_iov with
              | Ok () -> (
                  match IO.flush writer with
                  | Ok () -> Ok ()
                  | Error _ -> Error "IO.stderr flush should succeed for empty operations")
              | Error _ -> Error "IO.stderr write_all_vectored should accept empty iovecs")
          | Ok _ -> Error "IO.stderr write_owned_vectored should report zero for empty iovecs"
          | Error _ -> Error "IO.stderr write_owned_vectored should accept empty iovecs")
      | Error _ -> Error "IO.stderr write_all should accept empty strings")
  | Ok _ -> Error "IO.stderr write should report zero for empty strings"
  | Error _ -> Error "IO.stderr write should accept empty strings"

let tests = Test.[
  case "IO.stdin returns a reader for zero-length reads" test_stdin_reader_zero_length_read_is_a_noop;
  case "IO.stdout handles empty writer operations" test_stdout_writer_handles_empty_operations;
  case "IO.stderr handles empty writer operations" test_stderr_writer_handles_empty_operations;
]

let () =
  Runtime.run
    ~main:(fun ~args -> Test.Cli.main ~name:"IO.Stdio" ~tests ~args)
    ~args:Env.args
    ()
