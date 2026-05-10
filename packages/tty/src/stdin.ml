open Std

let input_buffer = Utf8_reader.create ()

type stdin_cell = {
  mutable current: IO.Stdin.t option;
}

let stdin_handle = { current = None }

let stdin = fun () ->
  match stdin_handle.current with
  | Some stdin -> stdin
  | None ->
      let stdin = IO.Stdin.open_ () in
      stdin_handle.current <- Some stdin;
      stdin

let read_stdin_bytes = fun stdin bytes ~offset ~len ->
  if len = 0 then
    `Ok 0
  else
    let buffer = IO.Buffer.create ~size:len in
    match IO.Stdin.read stdin ~into:buffer with
    | Ok count ->
        let copied = IO.Buffer.to_bytes buffer in
        IO.Bytes.blit_unchecked copied ~src_offset:0 ~dst:bytes ~dst_offset:offset ~len:count;
        `Ok count
    | Error IO.Operation_would_block
    | Error IO.Resource_unavailable_try_again -> `Would_block
    | Error _ -> `Error

let read_utf8 = fun () ->
  Utf8_reader.read
    input_buffer
    ~read:(fun bytes ~offset ~len ->
      read_stdin_bytes (stdin ()) bytes ~offset ~len)

let make_raw = fun () ->
  match Platform.open_tty () with
  | Error _ -> panic "failed to open /dev/tty"
  | Ok fd ->
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
                resume_mode = None;
                input_buffer = Utf8_reader.create ();
              }

let restore = fun terminal ->
  let _ =
    Platform.set_attributes terminal.Terminal.fd Platform.Now terminal.Terminal.original_attrs
  in
  if terminal.Terminal.owns_fd then
    let _ = Platform.close terminal.Terminal.fd in
    ()
