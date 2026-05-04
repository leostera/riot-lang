open Std

external bytes_unsafe_of_string: string -> bytes = "%bytes_of_string"

type mode =
  | Noop
  | StdPrint
  | StdPrintln
  | LogCompact
  | LogFull
  | RawWrite
  | FfiWrite
  | RawIntWrite
  | RawNativeWrite

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

let mode_of_string = fun __tmp1 ->
  match __tmp1 with
  | "noop" -> Noop
  | "std-print" -> StdPrint
  | "std-println" -> StdPrintln
  | "log-compact" -> LogCompact
  | "log-full" -> LogFull
  | "raw-write" -> RawWrite
  | "ffi-write" -> FfiWrite
  | "rawint-write" -> RawIntWrite
  | "raw-native-write" -> RawNativeWrite
  | value -> panic ("unknown mode: " ^ value)

let mode_to_string = fun __tmp1 ->
  match __tmp1 with
  | Noop -> "noop"
  | StdPrint -> "std-print"
  | StdPrintln -> "std-println"
  | LogCompact -> "log-compact"
  | LogFull -> "log-full"
  | RawWrite -> "raw-write"
  | FfiWrite -> "ffi-write"
  | RawIntWrite -> "rawint-write"
  | RawNativeWrite -> "raw-native-write"

let message_kind_of_string = fun __tmp1 ->
  match __tmp1 with
  | "small" -> Small
  | "medium" -> Medium
  | value -> panic ("unknown message kind: " ^ value)

let message_kind_to_string = fun __tmp1 ->
  match __tmp1 with
  | Small -> "small"
  | Medium -> "medium"

let message_for_kind = fun __tmp1 ->
  match __tmp1 with
  | Small -> small_message
  | Medium -> medium_message

let parse_args = fun args ->
  let rec loop config = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok config
    | "--iterations" :: value :: rest ->
        loop { config with iterations = Int.from_string value } rest
    | "--warmup" :: value :: rest -> loop { config with warmup = Int.from_string value } rest
    | "--mode" :: value :: rest -> loop { config with mode = mode_of_string value } rest
    | "--message" :: value :: rest ->
        loop { config with message_kind = message_kind_of_string value } rest
    | "--help" :: _
    | "-h" :: _ -> Error ()
    | flag :: [] when String.starts_with ~prefix:"--" flag ->
        panic ("missing value for argument: " ^ flag)
    | value :: _ when String.starts_with ~prefix:"--" value -> panic ("unknown argument: " ^ value)
    | value :: _ -> panic ("unexpected positional argument: " ^ value)
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

external stdout_write_ffi: int -> bytes -> int -> int -> (int, int) Result.t =
  "kernel_new_fs_file_write"

external stdout_write_raw_int: int -> bytes -> int -> int -> int = "kernel_new_fs_file_write_raw"

external stdout_write_all_raw_int: int -> bytes -> int -> int -> int =
  "kernel_new_fs_file_write_all_raw"

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

let write_stdout_bytes_raw_native = fun bytes ~len ->
  let written = stdout_write_all_raw_int 1 bytes 0 len in
  if written = len then
    ()
  else if written = 0 then
    panic "stdout raw-native write returned 0 bytes"
  else
    panic (Kernel.SystemError.to_string (Kernel.SystemError.from_code (-written)))

let configure_log = fun format_name ->
  Config.load_string
    (format
      Std.Format.[
        str "[[log.handler]]\n";
        str "type = \"stdout\"\n";
        str "format = \"";
        str format_name;
        str "\"\n";
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
  | FfiWrite -> write_stdout_bytes_ffi message_bytes ~len:message_len
  | RawIntWrite -> write_stdout_bytes_raw_int message_bytes ~len:message_len
  | RawNativeWrite -> write_stdout_bytes_raw_native message_bytes ~len:message_len

let run_loop = fun mode message message_bytes message_len iterations ->
  for _ = 1 to iterations do
    run_mode mode message message_bytes message_len
  done

let flush_mode = fun __tmp1 ->
  match __tmp1 with
  | LogCompact
  | LogFull -> Log.flush ()
  | _ -> ()

let main ~args =
  let args =
    match args with
    | [] -> []
    | _exe :: rest -> rest
  in
  match parse_args args with
  | Error () ->
      eprintln
        "usage: global_print_hot_loop [--iterations N] [--warmup N] [--mode noop|std-print|std-println|log-compact|log-full|raw-write|ffi-write|rawint-write|raw-native-write] [--message small|medium]";
      Ok ()
  | Ok config ->
      (
        match config.mode with
        | LogCompact -> configure_log "compact"
        | LogFull -> configure_log "full"
        | _ -> ()
      );
      let message = message_for_kind config.message_kind in
      let message_bytes = bytes_unsafe_of_string message in
      let message_len = String.length message in
      if config.warmup > 0 then (
        run_loop config.mode message message_bytes message_len config.warmup;
        flush_mode config.mode
      );
      let started = Time.Instant.now () in
      run_loop config.mode message message_bytes message_len config.iterations;
      flush_mode config.mode;
      let duration = Time.Instant.elapsed started in
      let total_nanos = Time.Duration.to_nanos duration in
      let per_iteration_nanos =
        if config.iterations > 0 then
          Int64.div total_nanos (Int64.from_int config.iterations)
        else
          0L
      in
      eprintln
        (format
          Std.Format.[
            str "{";
            str "\"mode\":\"";
            str (mode_to_string config.mode);
            str "\",";
            str "\"message\":\"";
            str (message_kind_to_string config.message_kind);
            str "\",";
            str "\"iterations\":";
            str (Int.to_string config.iterations);
            str ",";
            str "\"warmup\":";
            str (Int.to_string config.warmup);
            str ",";
            str "\"total_nanos\":";
            str (Int64.to_string total_nanos);
            str ",";
            str "\"per_iteration_nanos\":";
            str (Int64.to_string per_iteration_nanos);
            str "}";
          ]);
      Ok ()

let () = Runtime.run ~main ~args:Env.args ()
