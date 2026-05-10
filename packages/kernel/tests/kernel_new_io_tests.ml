open Std

module Test = Std.Test
module Kernel = Kernel

let ( let* ) value fn = Result.and_then value ~fn

let lift_stdin = fun result ->
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.IO.Stdin.error_to_string error)

let lift_stdout = fun result ->
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.IO.Stdout.error_to_string error)

let lift_stderr = fun result ->
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.IO.Stderr.error_to_string error)

let lift_async = fun result ->
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Async.error_to_string error)

let with_poll = fun fn ->
  match Kernel.Async.Poll.make () with
  | Kernel.Result.Error error -> Error (Kernel.Async.error_to_string error)
  | Kernel.Result.Ok poll ->
      try
        let value = fn poll in
        let _ = Kernel.Async.Poll.close poll in
        value
      with
      | error ->
          let _ = Kernel.Async.Poll.close poll in
          raise error

let assert_invalid_slice = fun ~error_to_string ~is_invalid_slice ->
  fun result ->
    match result with
    | Kernel.Result.Ok _ -> Error "expected InvalidSlice error"
    | Kernel.Result.Error error when is_invalid_slice error -> Ok ()
    | Kernel.Result.Error error -> Error ("expected InvalidSlice, got " ^ error_to_string error)

let test_stdin_read_len_zero_noop = fun _ctx ->
  let buffer = Kernel.Bytes.create ~size:16 in
  let* read = lift_stdin (Kernel.IO.Stdin.read ~len:0 buffer) in
  if read = 0 then
    Ok ()
  else
    Error "expected stdin.read with len=0 to return 0"

let test_stdout_write_len_zero_noop = fun _ctx ->
  let buffer = Kernel.Bytes.create ~size:16 in
  let* written = lift_stdout (Kernel.IO.Stdout.write ~len:0 buffer) in
  if written = 0 then
    Ok ()
  else
    Error "expected stdout.write with len=0 to return 0"

let test_stderr_write_len_zero_noop = fun _ctx ->
  let buffer = Kernel.Bytes.create ~size:16 in
  let* written = lift_stderr (Kernel.IO.Stderr.write ~len:0 buffer) in
  if written = 0 then
    Ok ()
  else
    Error "expected stderr.write with len=0 to return 0"

let test_stdin_read_rejects_invalid_slice = fun _ctx ->
  let buffer = Kernel.Bytes.create ~size:16 in
  assert_invalid_slice
    ~error_to_string:Kernel.IO.Stdin.error_to_string
    ~is_invalid_slice:(fun value ->
      match value with
      | Kernel.IO.Stdin.InvalidSlice _ -> true
      | _ -> false)
    (Kernel.IO.Stdin.read ~pos:(-1) ~len:4 buffer)

let test_stdout_write_rejects_invalid_slice = fun _ctx ->
  let buffer = Kernel.Bytes.create ~size:16 in
  assert_invalid_slice
    ~error_to_string:Kernel.IO.Stdout.error_to_string
    ~is_invalid_slice:(fun value ->
      match value with
      | Kernel.IO.Stdout.InvalidSlice _ -> true
      | _ -> false)
    (Kernel.IO.Stdout.write ~pos:(-1) ~len:4 buffer)

let test_stderr_write_rejects_invalid_slice = fun _ctx ->
  let buffer = Kernel.Bytes.create ~size:16 in
  assert_invalid_slice
    ~error_to_string:Kernel.IO.Stderr.error_to_string
    ~is_invalid_slice:(fun value ->
      match value with
      | Kernel.IO.Stderr.InvalidSlice _ -> true
      | _ -> false)
    (Kernel.IO.Stderr.write ~pos:(-1) ~len:4 buffer)

let test_stdin_read_vectored_len_zero_noop = fun _ctx ->
  let iovec =
    Kernel.IO.IoVec.from_bytes_array [|Kernel.Bytes.create ~size:0|]
    |> Result.unwrap
  in
  let* read = lift_stdin (Kernel.IO.Stdin.read_vectored iovec) in
  if read = 0 then
    Ok ()
  else
    Error "expected stdin.read_vectored with empty segment to return 0"

let test_stdout_write_vectored_len_zero_noop = fun _ctx ->
  let iovec =
    Kernel.IO.IoVec.from_bytes_array [|Kernel.Bytes.create ~size:0|]
    |> Result.unwrap
  in
  let* written = lift_stdout (Kernel.IO.Stdout.write_vectored iovec) in
  if written = 0 then
    Ok ()
  else
    Error "expected stdout.write_vectored with empty segment to return 0"

let test_stderr_write_vectored_len_zero_noop = fun _ctx ->
  let iovec =
    Kernel.IO.IoVec.from_bytes_array [|Kernel.Bytes.create ~size:0|]
    |> Result.unwrap
  in
  let* written = lift_stderr (Kernel.IO.Stderr.write_vectored iovec) in
  if written = 0 then
    Ok ()
  else
    Error "expected stderr.write_vectored with empty segment to return 0"

let test_stdin_source_register_and_deregister = fun _ctx ->
  with_poll
    (fun poll ->
      let source = Kernel.IO.Stdin.to_source () in
      let token = Kernel.Async.Token.make "kernel-io-stdin" in
      match Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source with
      | Kernel.Result.Ok () ->
          let* _events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          Ok ()
      | Kernel.Result.Error (
        Kernel.Async.System Kernel.SystemError.InvalidArgument
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Async.error_to_string error))

let test_stdout_source_register_and_deregister = fun _ctx ->
  with_poll
    (fun poll ->
      let source = Kernel.IO.Stdout.to_source () in
      let token = Kernel.Async.Token.make "kernel-io-stdout" in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.writable source)
      in
      let* _events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
      let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
      Ok ())

let test_stderr_source_register_and_deregister = fun _ctx ->
  with_poll
    (fun poll ->
      let source = Kernel.IO.Stderr.to_source () in
      let token = Kernel.Async.Token.make "kernel-io-stderr" in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.writable source)
      in
      let* _events = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
      let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
      Ok ())

let tests = [
  Test.case "Kernel.IO.Stdin.read len=0 is a no-op" test_stdin_read_len_zero_noop;
  Test.case "Kernel.IO.Stdout.write len=0 is a no-op" test_stdout_write_len_zero_noop;
  Test.case "Kernel.IO.Stderr.write len=0 is a no-op" test_stderr_write_len_zero_noop;
  Test.case "Kernel.IO.Stdin.read rejects invalid slice" test_stdin_read_rejects_invalid_slice;
  Test.case "Kernel.IO.Stdout.write rejects invalid slice" test_stdout_write_rejects_invalid_slice;
  Test.case "Kernel.IO.Stderr.write rejects invalid slice" test_stderr_write_rejects_invalid_slice;
  Test.case "Kernel.IO.Stdin.read_vectored len=0 is a no-op" test_stdin_read_vectored_len_zero_noop;
  Test.case
    "Kernel.IO.Stdout.write_vectored len=0 is a no-op"
    test_stdout_write_vectored_len_zero_noop;
  Test.case
    "Kernel.IO.Stderr.write_vectored len=0 is a no-op"
    test_stderr_write_vectored_len_zero_noop;
  Test.case
    "Kernel.IO.Stdin source register/deregister roundtrips"
    test_stdin_source_register_and_deregister;
  Test.case
    "Kernel.IO.Stdout source register/deregister roundtrips"
    test_stdout_source_register_and_deregister;
  Test.case
    "Kernel.IO.Stderr source register/deregister roundtrips"
    test_stderr_source_register_and_deregister;
]

let main ~args = Test.Cli.main ~name:"kernel_new_io_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
