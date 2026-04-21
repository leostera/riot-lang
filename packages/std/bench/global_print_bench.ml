open Std

external bytes_unsafe_of_string: string -> bytes = "%bytes_of_string"

external fd_write_raw_int: Kernel.Fs.File.t -> bytes -> int -> int -> int = "kernel_new_fs_file_write_raw"

external fd_write_all_raw_int: Kernel.Fs.File.t -> bytes -> int -> int -> int = "kernel_new_fs_file_write_all_raw"

let dev_null =
  match Kernel.Fs.File.open_write (Kernel.Path.from_string "/dev/null") with
  | Result.Ok file -> file
  | Result.Error error -> panic (Kernel.Fs.File.error_to_string error)

let write_all = fun bytes ~len ->
  let rec loop pos remaining =
    if remaining > 0 then
      match Kernel.Fs.File.write dev_null ~pos ~len:remaining bytes with
      | Result.Ok written ->
          if written <= 0 then
            panic "dev_null write returned 0 bytes"
          else
            loop (pos + written) (remaining - written)
      | Result.Error error -> panic (Kernel.Fs.File.error_to_string error)
  in
  loop 0 len

let write_all_raw_int = fun bytes ~len ->
  let rec loop pos remaining =
    if remaining > 0 then
      let written = fd_write_raw_int dev_null bytes pos remaining in
      if written > 0 then
        loop (pos + written) (remaining - written)
      else if written = 0 then
        panic "dev_null raw-int write returned 0 bytes"
      else
        panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code (-written)))
  in
  loop 0 len

let write_all_vectored = fun iovecs ->
  let rec loop iovecs =
    let remaining = Kernel.IO.IoVec.length iovecs in
    if remaining > 0 then
      match Kernel.Fs.File.write_vectored dev_null iovecs with
      | Result.Ok written ->
          if written <= 0 then
            panic "dev_null vectored write returned 0 bytes"
          else if written < remaining then
            loop
              (Kernel.IO.IoVec.sub iovecs ~pos:written ~len:(remaining - written) |> Result.unwrap)
      | Result.Error error -> panic (Kernel.Fs.File.error_to_string error)
  in
  loop iovecs

let write_all_raw_native = fun bytes ~len ->
  let written = fd_write_all_raw_int dev_null bytes 0 len in
  if written = len then
    ()
  else if written = 0 then
    panic "dev_null raw-native write returned 0 bytes"
  else
    panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code (-written)))

let newline = bytes_unsafe_of_string "\n"

let small_message = "test case passed"

let medium_message = "this is a medium-sized human test output line with metadata [large flaky/2] and a long suffix"

let bench_copy_write = fun message () ->
  let bytes = Kernel.Bytes.from_string message in
  write_all bytes ~len:(String.length message)

let bench_zero_copy_write = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  write_all bytes ~len:(String.length message)

let bench_zero_copy_write_raw_int = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  write_all_raw_int bytes ~len:(String.length message)

let bench_zero_copy_write_raw_native = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  write_all_raw_native bytes ~len:(String.length message)

let bench_zero_copy_line_split = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  write_all bytes ~len:(String.length message);
  write_all newline ~len:1

let bench_zero_copy_line_writev = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  let iovecs = Kernel.IO.IoVec.from_bytes_array [|bytes; newline|] |> Result.unwrap in
  write_all_vectored iovecs

let config = { Bench.iterations = 20_000; warmup = 5 }

let benchmarks =
  Bench.[
    with_config ~config "copy write: small" (bench_copy_write small_message);
    with_config ~config "zero-copy write: small" (bench_zero_copy_write small_message);
    with_config
      ~config
      "zero-copy write raw-int: small"
      (bench_zero_copy_write_raw_int small_message);
    with_config
      ~config
      "zero-copy write raw-native: small"
      (bench_zero_copy_write_raw_native small_message);
    with_config ~config "zero-copy line split: small" (bench_zero_copy_line_split small_message);
    with_config ~config "zero-copy line writev: small" (bench_zero_copy_line_writev small_message);
    with_config ~config "copy write: medium" (bench_copy_write medium_message);
    with_config ~config "zero-copy write: medium" (bench_zero_copy_write medium_message);
    with_config
      ~config
      "zero-copy write raw-int: medium"
      (bench_zero_copy_write_raw_int medium_message);
    with_config
      ~config
      "zero-copy write raw-native: medium"
      (bench_zero_copy_write_raw_native medium_message);
    with_config ~config "zero-copy line split: medium" (bench_zero_copy_line_split medium_message);
    with_config ~config "zero-copy line writev: medium" (bench_zero_copy_line_writev medium_message);
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"Global Print Benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
