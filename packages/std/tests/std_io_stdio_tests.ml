open Std

let test_stdin_reader_empty_iovec_is_a_noop = fun _ctx ->
  let reader = IO.stdin () in
  let empty_iov =
    IO.IoVec.from_string_array [||]
    |> Result.unwrap
  in
  match IO.read_vectored reader ~into:empty_iov with
  | Ok 0 -> Ok ()
  | Ok _ -> Error "IO.stdin should treat empty iovecs as a no-op"
  | Error _ -> Error "IO.stdin empty-iovec reads should not fail"

let test_stdout_writer_handles_empty_operations = fun _ctx ->
  let writer = IO.stdout () in
  let empty_iov =
    IO.IoVec.from_string_array [||]
    |> Result.unwrap
  in
  match IO.write writer ~from:(IO.Buffer.from_string "") with
  | Ok 0 ->
      (match IO.write_all writer ~from:(IO.Buffer.from_string "") with
      | Ok () ->
          (match IO.write_vectored writer ~from:empty_iov with
          | Ok 0 ->
              (match IO.write_all_vectored writer ~from:empty_iov with
              | Ok () ->
                  (match IO.flush writer with
                  | Ok () -> Ok ()
                  | Error _ -> Error "IO.stdout flush should succeed for empty operations")
              | Error _ -> Error "IO.stdout write_all_vectored should accept empty iovecs")
          | Ok _ -> Error "IO.stdout write_vectored should report zero for empty iovecs"
          | Error _ -> Error "IO.stdout write_vectored should accept empty iovecs")
      | Error _ -> Error "IO.stdout write_all should accept empty strings")
  | Ok _ -> Error "IO.stdout write should report zero for empty strings"
  | Error _ -> Error "IO.stdout write should accept empty strings"

let test_stderr_writer_handles_empty_operations = fun _ctx ->
  let writer = IO.stderr () in
  let empty_iov =
    IO.IoVec.from_string_array [||]
    |> Result.unwrap
  in
  match IO.write writer ~from:(IO.Buffer.from_string "") with
  | Ok 0 ->
      (match IO.write_all writer ~from:(IO.Buffer.from_string "") with
      | Ok () ->
          (match IO.write_vectored writer ~from:empty_iov with
          | Ok 0 ->
              (match IO.write_all_vectored writer ~from:empty_iov with
              | Ok () ->
                  (match IO.flush writer with
                  | Ok () -> Ok ()
                  | Error _ -> Error "IO.stderr flush should succeed for empty operations")
              | Error _ -> Error "IO.stderr write_all_vectored should accept empty iovecs")
          | Ok _ -> Error "IO.stderr write_vectored should report zero for empty iovecs"
          | Error _ -> Error "IO.stderr write_vectored should accept empty iovecs")
      | Error _ -> Error "IO.stderr write_all should accept empty strings")
  | Ok _ -> Error "IO.stderr write should report zero for empty strings"
  | Error _ -> Error "IO.stderr write should accept empty strings"

let tests =
  Test.[
    case "IO.stdin treats empty iovecs as a no-op" test_stdin_reader_empty_iovec_is_a_noop;
    case "IO.stdout handles empty writer operations" test_stdout_writer_handles_empty_operations;
    case "IO.stderr handles empty writer operations" test_stderr_writer_handles_empty_operations;
  ]

let main ~args = Test.Cli.main ~name:"IO.Stdio" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
