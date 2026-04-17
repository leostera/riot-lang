open Std

external bytes_unsafe_of_string: string -> bytes = "%bytes_of_string"

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

let write_all_vectored = fun iovecs ->
  let rec loop iovecs =
    let remaining = Kernel.IO.Iovec.length iovecs in
    if remaining > 0 then
      match Kernel.Fs.File.write_vectored dev_null iovecs with
      | Result.Ok written ->
          if written <= 0 then
            panic "dev_null vectored write returned 0 bytes"
          else if written < remaining then
            loop (Kernel.IO.Iovec.sub iovecs ~pos:written ~len:(remaining - written))
      | Result.Error error -> panic (Kernel.Fs.File.error_to_string error)
  in
  loop iovecs

let write_all_pair = fun left ~left_len right ~right_len ->
  let rec loop left_pos left_remaining right_pos right_remaining =
    let remaining = left_remaining + right_remaining in
    if remaining > 0 then
      match
        Kernel.Fs.File.write_pair
          dev_null
          ~left_pos
          ~left_len:left_remaining
          left
          ~right_pos
          ~right_len:right_remaining
          right
      with
      | Result.Ok written ->
          if written <= 0 then
            panic "dev_null write_pair returned 0 bytes"
          else
            let left_written =
              if written < left_remaining then
                written
              else
                left_remaining
            in
            let right_written = written - left_written in
            loop
              (left_pos + left_written)
              (left_remaining - left_written)
              (right_pos + right_written)
              (right_remaining - right_written)
      | Result.Error error -> panic (Kernel.Fs.File.error_to_string error)
  in
  loop 0 left_len 0 right_len

let newline = bytes_unsafe_of_string "\n"

let small_message = "test case passed"

let medium_message =
  "this is a medium-sized human test output line with metadata [large flaky/2] and a long suffix"

let bench_copy_write = fun message () ->
  let bytes = Kernel.Bytes.from_string message in
  write_all bytes ~len:(String.length message)

let bench_zero_copy_write = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  write_all bytes ~len:(String.length message)

let bench_zero_copy_line_split = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  write_all bytes ~len:(String.length message);
  write_all newline ~len:1

let bench_zero_copy_line_writev = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  let iovecs = Kernel.IO.Iovec.from_bytes_array [| bytes; newline |] in
  write_all_vectored iovecs

let bench_zero_copy_line_pair = fun message () ->
  let bytes = bytes_unsafe_of_string message in
  write_all_pair bytes ~left_len:(String.length message) newline ~right_len:1

let config = { Bench.iterations = 20_000; warmup = 5 }

let benchmarks =
  Bench.[
    with_config ~config "copy write: small" (bench_copy_write small_message);
    with_config ~config "zero-copy write: small" (bench_zero_copy_write small_message);
    with_config ~config "zero-copy line split: small" (bench_zero_copy_line_split small_message);
    with_config ~config "zero-copy line writev: small" (bench_zero_copy_line_writev small_message);
    with_config ~config "zero-copy line pair: small" (bench_zero_copy_line_pair small_message);
    with_config ~config "copy write: medium" (bench_copy_write medium_message);
    with_config ~config "zero-copy write: medium" (bench_zero_copy_write medium_message);
    with_config ~config "zero-copy line split: medium" (bench_zero_copy_line_split medium_message);
    with_config ~config "zero-copy line writev: medium" (bench_zero_copy_line_writev medium_message);
    with_config ~config "zero-copy line pair: medium" (bench_zero_copy_line_pair medium_message);
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"Global Print Benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
