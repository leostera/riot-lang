open Std

type size = { rows: int; cols: int }

type error =
  | NoTtyConnected
  | SystemError of IO.error

type mode =
  | LineBuffered
  | Immediate

type input_buffer = Utf8_reader.t

type t = {
  fd: Platform.fd;
  owns_fd: bool;
  input_fd: Platform.fd;
  stdout: Platform.fd;
  stderr: Platform.fd;
  original_attrs: Platform.termios;
  mutable size: size;
  mutable mode: mode;
  mutable resume_mode: mode option;
  input_buffer: input_buffer;
}

let io_error_of_system_error = fun error -> IO.from_system_error error

let write_with = fun write bytes ->
  let rec loop offset remaining =
    if remaining = 0 then
      ()
    else
      match write bytes ~offset ~len:remaining with
      | Ok 0 -> ()
      | Ok written -> loop (offset + written) (remaining - written)
      | Error _ -> ()
  in
  loop 0 (IO.Bytes.length bytes)

let write_to_fd = fun fd value ->
  let bytes = IO.Bytes.from_string value in
  let rec loop offset remaining =
    if remaining = 0 then
      ()
    else
      match Platform.write fd bytes ~offset ~len:remaining with
      | Ok 0 -> ()
      | Ok written -> loop (offset + written) (remaining - written)
      | Error _ -> ()
  in
  loop 0 (IO.Bytes.length bytes)

let write_escape = fun t code -> write_to_fd t.stdout (Escape_seq.csi ^ code)
