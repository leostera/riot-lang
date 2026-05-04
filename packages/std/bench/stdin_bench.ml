open Std

module Kernel = Kernel
module Buffer = IO.Buffer
module Bytes = IO.Bytes

type worker_mode =
  | Kernel_chunks
  | Std_chunks
  | Std_reader_chunks
  | Std_read_to_string
  | Kernel_chars
  | Std_chars
  | Std_buffered_chars
  | Std_lines

type payload_kind =
  | Chunk_payload
  | Char_payload
  | Line_payload

type payload = { data: string; expected_bytes: int; expected_lines: int }

let mode_to_string = fun __tmp1 ->
  match __tmp1 with
  | Kernel_chunks -> "kernel-chunks"
  | Std_chunks -> "std-chunks"
  | Std_reader_chunks -> "std-reader-chunks"
  | Std_read_to_string -> "std-read-to-string"
  | Kernel_chars -> "kernel-chars"
  | Std_chars -> "std-chars"
  | Std_buffered_chars -> "std-buffered-chars"
  | Std_lines -> "std-lines"

let mode_of_string = fun __tmp1 ->
  match __tmp1 with
  | "kernel-chunks" -> Kernel_chunks
  | "std-chunks" -> Std_chunks
  | "std-reader-chunks" -> Std_reader_chunks
  | "std-read-to-string" -> Std_read_to_string
  | "kernel-chars" -> Kernel_chars
  | "std-chars" -> Std_chars
  | "std-buffered-chars" -> Std_buffered_chars
  | "std-lines" -> Std_lines
  | value -> panic ("unknown stdin bench worker mode: " ^ value)

let payload_for_kind = fun __tmp1 ->
  match __tmp1 with
  | Chunk_payload ->
      let chunk = "0123456789abcdef" in
      let repeat_count = 65_536 in
      let buffer = Buffer.create ~size:(String.length chunk * repeat_count) in
      for _ = 1 to repeat_count do
        Buffer.add_string buffer chunk
      done;
      let data = Buffer.contents buffer in
      { data; expected_bytes = String.length data; expected_lines = 0 }
  | Char_payload ->
      let chunk = "char-stream-" in
      let repeat_count = 4_096 in
      let buffer = Buffer.create ~size:(String.length chunk * repeat_count) in
      for _ = 1 to repeat_count do
        Buffer.add_string buffer chunk
      done;
      let data = Buffer.contents buffer in
      { data; expected_bytes = String.length data; expected_lines = 0 }
  | Line_payload ->
      let line_count = 5_000 in
      let buffer = Buffer.create ~size:(line_count * 48) in
      for index = 1 to line_count do
        Buffer.add_string
          buffer
          (format
            Std.Format.[
              str "line-";
              str (Int.to_string index);
              str ": repeated payload for stdin benchmark";
              str "\n";
            ])
      done;
      let data = Buffer.contents buffer in
      { data; expected_bytes = String.length data; expected_lines = line_count }

let payload_for_mode = fun __tmp1 ->
  match __tmp1 with
  | Kernel_chunks
  | Std_chunks
  | Std_reader_chunks
  | Std_read_to_string -> payload_for_kind Chunk_payload
  | Kernel_chars
  | Std_chars
  | Std_buffered_chars -> payload_for_kind Char_payload
  | Std_lines -> payload_for_kind Line_payload

let executable_path =
  match Env.args with
  | executable :: _ -> Path.v executable
  | [] -> panic "stdin bench requires argv[0] to self-spawn"

let ( let* ) value fn = Result.and_then value ~fn

let lift_process = fun __tmp1 ->
  match __tmp1 with
  | Ok value -> Ok value
  | Error error -> Error (Kernel.Process.error_to_string error)

let lift_async = fun __tmp1 ->
  match __tmp1 with
  | Ok value -> Ok value
  | Error error -> Error (Kernel.Async.error_to_string error)

let lift_file = fun __tmp1 ->
  match __tmp1 with
  | Ok value -> Ok value
  | Error error -> Error (Kernel.Fs.File.error_to_string error)

let is_would_block = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Fs.File.System system_error -> Kernel.SystemError.would_block system_error
  | _ -> false

let bufreader_error_message = IO.error_message

let status_to_string = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Process.Running -> "running"
  | Kernel.Process.Exited code -> "exited(" ^ Int.to_string code ^ ")"
  | Kernel.Process.Signaled signal -> "signaled(" ^ Int.to_string signal ^ ")"
  | Kernel.Process.Stopped signal -> "stopped(" ^ Int.to_string signal ^ ")"

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let with_poll = fun fn ->
  let* poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let* () = lift_async (Kernel.Async.Poll.register poll token interest source) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
      let found =
        List.any
          events
          ~fn:(fun event ->
            Kernel.Async.Token.equal token (Kernel.Async.Event.token event) && pred event)
      in
      if found then
        Ok ()
      else
        Error "expected readiness event")

let wait_readable = fun poll ~token source ->
  wait_for
    poll
    ~token
    ~interest:Kernel.Async.Interest.readable
    ~source
    ~pred:Kernel.Async.Event.is_readable

let wait_writable = fun poll ~token source ->
  wait_for
    poll
    ~token
    ~interest:Kernel.Async.Interest.writable
    ~source
    ~pred:Kernel.Async.Event.is_writable

let write_all = fun poll ~token file buffer ->
  let rec loop pos len =
    if len = 0 then
      Ok ()
    else
      match Kernel.Fs.File.write file ~pos ~len buffer with
      | Ok written ->
          if written <= 0 then
            Error "expected stdin bench pipe write to make progress"
          else
            loop (pos + written) (len - written)
      | Error error ->
          if is_would_block error then
            let* () = wait_writable poll ~token (Kernel.Fs.File.to_source file) in
            loop pos len
          else
            Error (Kernel.Fs.File.error_to_string error)
  in
  loop 0 (Kernel.Bytes.length buffer)

let read_all = fun poll ~token file ->
  let buffer = Kernel.Bytes.create ~size:4_096 in
  let rec loop parts =
    match Kernel.Fs.File.read file buffer with
    | Ok 0 -> Ok (String.concat "" (List.reverse parts))
    | Ok count -> loop (Kernel.Bytes.sub_string buffer ~offset:0 ~len:count :: parts)
    | Error error ->
        if is_would_block error then
          let* () = wait_readable poll ~token (Kernel.Fs.File.to_source file) in
          loop parts
        else
          Error (Kernel.Fs.File.error_to_string error)
  in
  loop []

let wait_for_exit = fun poll ~token process ->
  let rec loop () =
    let* status = lift_process (Kernel.Process.try_wait process) in
    match status with
    | Some status -> Ok status
    | None ->
        let source = Kernel.Process.to_source process in
        let* () =
          lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.priority source)
        in
        protect
          ~finally:(fun () ->
            let _ = Kernel.Async.Poll.deregister poll source in
            ())
          (fun () ->
            let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:1_000_000L poll) in
            loop ())
  in
  loop ()

let run_worker_process = fun mode payload ->
  let stdio = Kernel.Process.{ stdin = Stdin.Pipe; stdout = Stdout.Null; stderr = Stderr.Pipe } in
  let args = [|
    "--worker";
    mode_to_string mode;
    "--expected-bytes";
    Int.to_string payload.expected_bytes;
    "--expected-lines";
    Int.to_string payload.expected_lines;
  |]
  in
  let* process =
    lift_process (Kernel.Process.spawn ~program:(Path.to_string executable_path) ~args ~stdio ())
  in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Process.close process in
      ())
    (fun () ->
      match (Kernel.Process.stdin process, Kernel.Process.stderr process) with
      | (Some stdin_file, Some stderr_file) ->
          with_poll
            (fun poll ->
              let* () =
                write_all
                  poll
                  ~token:(Kernel.Async.Token.make ("stdin-bench-write", mode_to_string mode))
                  stdin_file
                  (Kernel.Bytes.from_string payload.data)
              in
              let* () = lift_file (Kernel.Fs.File.close stdin_file) in
              let* status =
                wait_for_exit
                  poll
                  ~token:(Kernel.Async.Token.make ("stdin-bench-exit", mode_to_string mode))
                  process
              in
              let* stderr =
                read_all
                  poll
                  ~token:(Kernel.Async.Token.make ("stdin-bench-stderr", mode_to_string mode))
                  stderr_file
              in
              match status with
              | Kernel.Process.Exited 0 -> Ok ()
              | _ ->
                  let details =
                    if stderr = "" then
                      ""
                    else
                      " " ^ stderr
                  in
                  Error ("stdin bench worker failed: " ^ status_to_string status ^ details))
      | _ -> Error "stdin bench worker expected stdin and stderr pipes")

let expect_ok = fun __tmp1 ->
  match __tmp1 with
  | Ok () -> ()
  | Error error -> panic error

let run_kernel_chunks = fun expected_bytes ->
  let buffer = Bytes.create ~size:4_096 in
  let rec loop total =
    match Kernel.IO.Stdin.read ~len:4_096 buffer with
    | Ok 0 -> total
    | Ok count -> loop (total + count)
    | Error error -> panic (Kernel.IO.Stdin.error_to_string error)
  in
  let total = loop 0 in
  if total != expected_bytes then
    panic
      (format
        Std.Format.[
          str "kernel chunk worker expected ";
          str (Int.to_string expected_bytes);
          str " bytes but read ";
          str (Int.to_string total);
        ])

let run_std_chunks = fun expected_bytes ->
  let stdin = IO.Stdin.open_ () in
  let buffer = IO.Buffer.create ~size:4_096 in
  let rec loop total =
    IO.Buffer.clear buffer;
    match IO.Stdin.read stdin ~into:buffer with
    | Ok 0 -> total
    | Ok _ -> loop (total + IO.Buffer.readable_bytes buffer)
    | Error error -> panic (IO.error_message error)
  in
  let total = loop 0 in
  if total != expected_bytes then
    panic
      (format
        Std.Format.[
          str "std chunk worker expected ";
          str (Int.to_string expected_bytes);
          str " bytes but read ";
          str (Int.to_string total);
        ])

let run_std_reader_chunks = fun expected_bytes ->
  let reader = IO.stdin () in
  let buffer = IO.Buffer.create ~size:4_096 in
  let rec loop total =
    IO.Buffer.clear buffer;
    match IO.read reader ~into:buffer with
    | Ok 0 -> total
    | Ok _ -> loop (total + IO.Buffer.readable_bytes buffer)
    | Error error -> panic (IO.error_message error)
  in
  let total = loop 0 in
  if total != expected_bytes then
    panic
      (format
        Std.Format.[
          str "std reader worker expected ";
          str (Int.to_string expected_bytes);
          str " bytes but read ";
          str (Int.to_string total);
        ])

let run_std_read_to_string = fun expected_bytes ->
  let reader = IO.stdin () in
  let builder = StringBuilder.create ~size:expected_bytes in
  match IO.read_to_string reader ~into:builder with
  | Ok _ ->
      let data = StringBuilder.contents builder in
      if String.length data != expected_bytes then
        panic
          (format
            Std.Format.[
              str "std read_to_string worker expected ";
              str (Int.to_string expected_bytes);
              str " bytes but read ";
              str (Int.to_string (String.length data));
            ])
  | Error error -> panic (IO.error_message error)

let run_kernel_chars = fun expected_bytes ->
  let buffer = Bytes.create ~size:1 in
  let rec loop total =
    match Kernel.IO.Stdin.read ~len:1 buffer with
    | Ok 0 -> total
    | Ok count -> loop (total + count)
    | Error error -> panic (Kernel.IO.Stdin.error_to_string error)
  in
  let total = loop 0 in
  if total != expected_bytes then
    panic
      (format
        Std.Format.[
          str "kernel char worker expected ";
          str (Int.to_string expected_bytes);
          str " bytes but read ";
          str (Int.to_string total);
        ])

let run_std_chars = fun expected_bytes ->
  let reader = IO.stdin () in
  let buffer = IO.Buffer.create ~size:1 in
  let rec loop total =
    IO.Buffer.clear buffer;
    match IO.read reader ~into:buffer with
    | Ok 0 -> total
    | Ok _ -> loop (total + IO.Buffer.readable_bytes buffer)
    | Error error -> panic (IO.error_message error)
  in
  let total = loop 0 in
  if total != expected_bytes then
    panic
      (format
        Std.Format.[
          str "std char worker expected ";
          str (Int.to_string expected_bytes);
          str " bytes but read ";
          str (Int.to_string total);
        ])

let run_std_buffered_chars = fun expected_bytes ->
  let reader =
    IO.stdin ()
    |> IO.BufReader.from_reader
  in
  let rec loop total =
    match IO.BufReader.read_byte reader with
    | Ok _ -> loop (total + 1)
    | Error IO.End_of_file -> total
    | Error error -> panic (bufreader_error_message error)
  in
  let total = loop 0 in
  if total != expected_bytes then
    panic
      (format
        Std.Format.[
          str "std buffered char worker expected ";
          str (Int.to_string expected_bytes);
          str " bytes but read ";
          str (Int.to_string total);
        ])

let run_std_lines = fun expected_bytes expected_lines ->
  let reader =
    IO.stdin ()
    |> IO.BufReader.from_reader
  in
  let rec loop byte_count line_count =
    match IO.BufReader.read_line reader with
    | Ok line -> loop (byte_count + IO.IoSlice.length line) (line_count + 1)
    | Error IO.End_of_file -> (byte_count, line_count)
    | Error error -> panic (bufreader_error_message error)
  in
  let (byte_count, line_count) = loop 0 0 in
  if byte_count != expected_bytes || line_count != expected_lines then
    panic
      (format
        Std.Format.[
          str "std line worker expected ";
          str (Int.to_string expected_lines);
          str " lines / ";
          str (Int.to_string expected_bytes);
          str " bytes but read ";
          str (Int.to_string line_count);
          str " lines / ";
          str (Int.to_string byte_count);
          str " bytes";
        ])

let run_worker = fun mode ~expected_bytes ~expected_lines ->
  match mode with
  | Kernel_chunks -> run_kernel_chunks expected_bytes
  | Std_chunks -> run_std_chunks expected_bytes
  | Std_reader_chunks -> run_std_reader_chunks expected_bytes
  | Std_read_to_string -> run_std_read_to_string expected_bytes
  | Kernel_chars -> run_kernel_chars expected_bytes
  | Std_chars -> run_std_chars expected_bytes
  | Std_buffered_chars -> run_std_buffered_chars expected_bytes
  | Std_lines -> run_std_lines expected_bytes expected_lines

let parse_worker_args = fun __tmp1 ->
  match __tmp1 with
  | mode :: "--expected-bytes" :: expected_bytes :: "--expected-lines" :: expected_lines :: [] -> (
    mode_of_string mode,
    Int.from_string expected_bytes,
    Int.from_string expected_lines
  )
  | args ->
      panic
        ("stdin bench worker expected: <mode> --expected-bytes N --expected-lines N, got "
        ^ String.concat " " args)

let run_case = fun mode () ->
  let payload = payload_for_mode mode in
  expect_ok (run_worker_process mode payload)

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 8; warmup = 2 }
      "stdin kernel read chunks: 1 MiB"
      (run_case Kernel_chunks);
    with_config
      ~config:{ iterations = 8; warmup = 2 }
      "stdin std read chunks: 1 MiB"
      (run_case Std_chunks);
    with_config
      ~config:{ iterations = 8; warmup = 2 }
      "stdin std reader chunks: 1 MiB"
      (run_case Std_reader_chunks);
    with_config
      ~config:{ iterations = 8; warmup = 2 }
      "stdin std read_to_string: 1 MiB"
      (run_case Std_read_to_string);
    with_config
      ~config:{ iterations = 3; warmup = 1 }
      "stdin kernel read chars: 48 KiB"
      (run_case Kernel_chars);
    with_config
      ~config:{ iterations = 3; warmup = 1 }
      "stdin std read chars: 48 KiB"
      (run_case Std_chars);
    with_config
      ~config:{ iterations = 3; warmup = 1 }
      "stdin std buffered read chars: 48 KiB"
      (run_case Std_buffered_chars);
    with_config
      ~config:{ iterations = 3; warmup = 1 }
      "stdin std read_line: 5k piped lines"
      (run_case Std_lines);
  ]

let main ~args =
  match args with
  | _exe :: "--worker" :: worker_args ->
      let (mode, expected_bytes, expected_lines) = parse_worker_args worker_args in
      run_worker mode ~expected_bytes ~expected_lines;
      Ok ()
  | _ -> Bench.Cli.main ~name:"Stdin Benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
