open Std

external bytes_unsafe_of_string: string -> bytes = "%bytes_of_string"

let newline = bytes_unsafe_of_string "\n"

type mode =
  | Noop
  | StdPrint
  | StdPrintln
  | LogCompact
  | LogFull
  | RawWrite
  | RawPair
  | FfiWrite
  | FfiPair
  | RawIntWrite
  | RawIntPair
  | RawNativeWrite
  | RawNativePair

type message_kind =
  | Small
  | Medium

type config = {
  iterations: int;
  warmup: int;
  mode: mode;
  message_kind: message_kind;
}

let default_config = {
  iterations = 1_000_000;
  warmup = 10_000;
  mode = StdPrintln;
  message_kind = Small;
}

let small_message = "test case passed"

let medium_message =
  "this is a medium-sized human test output line with metadata [large flaky/2] and a long suffix"

let mode_of_string = function
  | "noop" -> Noop
  | "std-print" -> StdPrint
  | "std-println" -> StdPrintln
  | "log-compact" -> LogCompact
  | "log-full" -> LogFull
  | "raw-write" -> RawWrite
  | "raw-pair" -> RawPair
  | "ffi-write" -> FfiWrite
  | "ffi-pair" -> FfiPair
  | "rawint-write" -> RawIntWrite
  | "rawint-pair" -> RawIntPair
  | "raw-native-write" -> RawNativeWrite
  | "raw-native-pair" -> RawNativePair
  | value -> panic ("unknown mode: " ^ value)

let mode_to_string = function
  | Noop -> "noop"
  | StdPrint -> "std-print"
  | StdPrintln -> "std-println"
  | LogCompact -> "log-compact"
  | LogFull -> "log-full"
  | RawWrite -> "raw-write"
  | RawPair -> "raw-pair"
  | FfiWrite -> "ffi-write"
  | FfiPair -> "ffi-pair"
  | RawIntWrite -> "rawint-write"
  | RawIntPair -> "rawint-pair"
  | RawNativeWrite -> "raw-native-write"
  | RawNativePair -> "raw-native-pair"

let message_kind_of_string = function
  | "small" -> Small
  | "medium" -> Medium
  | value -> panic ("unknown message kind: " ^ value)

let message_kind_to_string = function
  | Small -> "small"
  | Medium -> "medium"

let message_for_kind = function
  | Small -> small_message
  | Medium -> medium_message

let parse_args = fun args ->
  let rec loop config =
    function
    | [] -> Ok config
    | "--iterations" :: value :: rest ->
        loop { config with iterations = Int.of_string value } rest
    | "--warmup" :: value :: rest ->
        loop { config with warmup = Int.of_string value } rest
    | "--mode" :: value :: rest ->
        loop { config with mode = mode_of_string value } rest
    | "--message" :: value :: rest ->
        loop { config with message_kind = message_kind_of_string value } rest
    | "--help" :: _
    | "-h" :: _ ->
        Error ()
    | flag :: [] when String.starts_with ~prefix:"--" flag ->
        panic ("missing value for argument: " ^ flag)
    | value :: _ when String.starts_with ~prefix:"--" value ->
        panic ("unknown argument: " ^ value)
    | value :: _ ->
        panic ("unexpected positional argument: " ^ value)
  in
  loop default_config args

let write_stdout_bytes = fun bytes ~len ->
  let rec loop pos remaining =
    if remaining > 0 then
      match Kernel.IO.Stdout.write ~pos ~len:remaining bytes with
      | Result.Ok written ->
          if written <= 0 then
            panic "stdout write returned 0 bytes"
          else
            loop (pos + written) (remaining - written)
      | Result.Error error -> panic (Kernel.IO.Stdout.error_to_string error)
  in
  loop 0 len

external stdout_write_ffi:
  int -> bytes -> int -> int -> (int, int) Result.t
  = "kernel_new_fs_file_write"

let write_stdout_pair = fun left ~left_len right ~right_len ->
  let rec loop left_pos left_remaining right_pos right_remaining =
    let remaining = left_remaining + right_remaining in
    if remaining > 0 then
      match
        Kernel.IO.Stdout.write_pair
          ~left_pos
          ~left_len:left_remaining
          left
          ~right_pos
          ~right_len:right_remaining
          right
      with
      | Result.Ok written ->
          if written <= 0 then
            panic "stdout write_pair returned 0 bytes"
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
      | Result.Error error -> panic (Kernel.IO.Stdout.error_to_string error)
  in
  loop 0 left_len 0 right_len

external stdout_write_pair_ffi:
  int -> bytes -> int -> int -> bytes -> int -> int -> (int, int) Result.t
  = "kernel_new_fs_file_write_pair_bytecode" "kernel_new_fs_file_write_pair"

external stdout_write_raw_int:
  int -> bytes -> int -> int -> int
  = "kernel_new_fs_file_write_raw"

external stdout_write_pair_raw_int:
  int -> bytes -> int -> int -> bytes -> int -> int -> int
  = "kernel_new_fs_file_write_pair_raw_bytecode" "kernel_new_fs_file_write_pair_raw"

external stdout_write_all_raw_int:
  int -> bytes -> int -> int -> int
  = "kernel_new_fs_file_write_all_raw"

external stdout_write_pair_all_raw_int:
  int -> bytes -> int -> int -> bytes -> int -> int -> int
  = "kernel_new_fs_file_write_pair_all_raw_bytecode" "kernel_new_fs_file_write_pair_all_raw"

let write_stdout_bytes_ffi = fun bytes ~len ->
  let rec loop pos remaining =
    if remaining > 0 then
      match stdout_write_ffi 1 bytes pos remaining with
      | Result.Ok written ->
          if written <= 0 then
            panic "stdout ffi write returned 0 bytes"
          else
            loop (pos + written) (remaining - written)
      | Result.Error code ->
          panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code code))
  in
  loop 0 len

let write_stdout_pair_ffi = fun left ~left_len right ~right_len ->
  let rec loop left_pos left_remaining right_pos right_remaining =
    let remaining = left_remaining + right_remaining in
    if remaining > 0 then
      match stdout_write_pair_ffi 1 left left_pos left_remaining right right_pos right_remaining with
      | Result.Ok written ->
          if written <= 0 then
            panic "stdout ffi write_pair returned 0 bytes"
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
      | Result.Error code ->
          panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code code))
  in
  loop 0 left_len 0 right_len

let write_stdout_bytes_raw_int = fun bytes ~len ->
  let rec loop pos remaining =
    if remaining > 0 then
      let written = stdout_write_raw_int 1 bytes pos remaining in
      if written > 0 then
        loop (pos + written) (remaining - written)
      else if written = 0 then
        panic "stdout raw-int write returned 0 bytes"
      else
        panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code (-written)))
  in
  loop 0 len

let write_stdout_pair_raw_int = fun left ~left_len right ~right_len ->
  let rec loop left_pos left_remaining right_pos right_remaining =
    let remaining = left_remaining + right_remaining in
    if remaining > 0 then
      let written =
        stdout_write_pair_raw_int 1 left left_pos left_remaining right right_pos right_remaining
      in
      if written > 0 then
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
      else if written = 0 then
        panic "stdout raw-int write_pair returned 0 bytes"
      else
        panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code (-written)))
  in
  loop 0 left_len 0 right_len

let write_stdout_bytes_raw_native = fun bytes ~len ->
  let written = stdout_write_all_raw_int 1 bytes 0 len in
  if written = len then
    ()
  else if written = 0 then
    panic "stdout raw-native write returned 0 bytes"
  else
    panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code (-written)))

let write_stdout_pair_raw_native = fun left ~left_len right ~right_len ->
  let total_len = left_len + right_len in
  let written = stdout_write_pair_all_raw_int 1 left 0 left_len right 0 right_len in
  if written = total_len then
    ()
  else if written = 0 then
    panic "stdout raw-native write_pair returned 0 bytes"
  else
    panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code (-written)))

let configure_log = fun format_name ->
  Config.load_string
    (format Format.[
       str "[[log.handler]]\n";
       str "type = \"stdout\"\n";
       str "format = \""; str format_name; str "\"\n";
     ]);
  Log.set_level Log.Info;
  ignore (Log.start_link ())

let run_mode = fun mode message message_bytes message_len ->
  match mode with
  | Noop -> ignore message_len
  | StdPrint -> print message
  | StdPrintln -> println message
  | LogCompact -> Log.info message
  | LogFull -> Log.info message
  | RawWrite -> write_stdout_bytes message_bytes ~len:message_len
  | RawPair -> write_stdout_pair message_bytes ~left_len:message_len newline ~right_len:1
  | FfiWrite -> write_stdout_bytes_ffi message_bytes ~len:message_len
  | FfiPair -> write_stdout_pair_ffi message_bytes ~left_len:message_len newline ~right_len:1
  | RawIntWrite -> write_stdout_bytes_raw_int message_bytes ~len:message_len
  | RawIntPair -> write_stdout_pair_raw_int message_bytes ~left_len:message_len newline ~right_len:1
  | RawNativeWrite -> write_stdout_bytes_raw_native message_bytes ~len:message_len
  | RawNativePair ->
      write_stdout_pair_raw_native message_bytes ~left_len:message_len newline ~right_len:1

let run_loop = fun mode message message_bytes message_len iterations ->
  for _ = 1 to iterations do
    run_mode mode message message_bytes message_len
  done

let main = fun ~args ->
  let args =
    match args with
    | [] -> []
    | _exe :: rest -> rest
  in
  match parse_args args with
  | Error () ->
      eprintln
        "usage: global_print_hot_loop [--iterations N] [--warmup N] [--mode noop|std-print|std-println|log-compact|log-full|raw-write|raw-pair|ffi-write|ffi-pair|rawint-write|rawint-pair|raw-native-write|raw-native-pair] [--message small|medium]";
      Ok ()
  | Ok config ->
      (match config.mode with
       | LogCompact -> configure_log "compact"
       | LogFull -> configure_log "full"
       | _ -> ());
      let message = message_for_kind config.message_kind in
      let message_bytes = bytes_unsafe_of_string message in
      let message_len = String.length message in
      if config.warmup > 0 then
        run_loop config.mode message message_bytes message_len config.warmup;
      let started = Time.Instant.now () in
      run_loop config.mode message message_bytes message_len config.iterations;
      let duration = Time.Instant.elapsed started in
      let total_nanos = Time.Duration.to_nanos duration in
      let per_iteration_nanos =
        if config.iterations > 0 then
          Int64.div total_nanos (Int64.of_int config.iterations)
        else
          0L
      in
      eprintln
        (format Format.[
           str "{";
           str "\"mode\":\""; str (mode_to_string config.mode); str "\",";
           str "\"message\":\""; str (message_kind_to_string config.message_kind); str "\",";
           str "\"iterations\":"; str (Int.to_string config.iterations); str ",";
           str "\"warmup\":"; str (Int.to_string config.warmup); str ",";
           str "\"total_nanos\":"; str (Int64.to_string total_nanos); str ",";
           str "\"per_iteration_nanos\":"; str (Int64.to_string per_iteration_nanos);
           str "}";
         ]);
      Ok ()

let () = Runtime.run ~main ~args:Env.args ()
