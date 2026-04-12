open Std

let utf8_char_length = fun first_byte ->
  if first_byte land 0x80 = 0 then
    1
  else if first_byte land 0xe0 = 0xc0 then
    2
  else if first_byte land 0xf0 = 0xe0 then
    3
  else if first_byte land 0xf8 = 0xf0 then
    4
  else
    0

let read_utf8 = fun () ->
  let fd = Platform.stdin_fd () in
  let bytes = IO.Bytes.create 4 in
  match Platform.read fd bytes ~offset:0 ~len:1 with
  | Ok 0 ->
      `End
  | Ok 1 ->
      let first_byte = Char.code (IO.Bytes.get bytes 0) in
      let len = utf8_char_length first_byte in
      if len = 0 then
        `Malformed "Invalid UTF-8 start byte"
      else if len = 1 then
        `Read (IO.Bytes.sub_string bytes 0 1)
      else
        (
          match Platform.read fd bytes ~offset:1 ~len:(len - 1) with
          | Ok n when n = len - 1 -> `Read (IO.Bytes.sub_string bytes 0 len)
          | Ok _ -> `Malformed "Incomplete UTF-8 sequence"
          | Error error when Kernel.SystemError.is_would_block error -> `Retry
          | Error _ -> `Malformed "Read error"
        )
  | Ok _ ->
      `Malformed "Unexpected read length"
  | Error error when Kernel.SystemError.is_would_block error ->
      `Retry
  | Error _ ->
      `End

let make_raw = fun () ->
  match Platform.open_tty () with
  | Error _ -> panic "failed to open /dev/tty"
  | Ok fd -> (
      match Platform.get_attributes fd with
      | Error _ ->
          let _ = Platform.close fd in
          panic "failed to read terminal attributes"
      | Ok original_attrs ->
          let raw_attrs = Platform.make_raw_mode original_attrs in
          match Platform.set_attributes fd Platform.Now raw_attrs with
          | Error _ ->
              let _ = Platform.close fd in
              panic "failed to enable raw mode"
          | Ok () ->
              let size =
                match Platform.get_size fd with
                | Ok (cols, rows) -> Terminal.{ rows; cols }
                | Error _ -> Terminal.{ rows = 24; cols = 80 }
              in
              Terminal.{
                fd;
                owns_fd = true;
                input_fd = Platform.stdin_fd ();
                stdout = Platform.stdout_fd ();
                stderr = Platform.stderr_fd ();
                original_attrs;
                size;
                mode = Immediate;
                input_buffer = None;
              }
    )

let restore = fun terminal ->
  let _ = Platform.set_attributes terminal.Terminal.fd Platform.Now terminal.Terminal.original_attrs in
  if terminal.Terminal.owns_fd then
    let _ = Platform.close terminal.Terminal.fd in
    ()
