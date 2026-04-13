open Std
open Std.IO
module Escape_seq = Escape_seq
module Color = Color
module Profile = Profile
module Style = Style
module Size = Size
module Input = Input
module Terminal_control = Terminal_control

type fd = Platform.fd

type size = Terminal.size = {
  rows: int;
  cols: int;
}

type error = Terminal.error =
  | NoTtyConnected
  | SystemError of IO.error

type mode = Terminal.mode =
  | LineBuffered
  | Immediate

type t = Terminal.t

let io_error_of_system_error = fun error -> IO.of_system_error error

let error_of_system_error = fun error -> SystemError (io_error_of_system_error error)

let default_size = fun () -> { rows = 24; cols = 80 }

let detect_size = fun fd ->
  match Platform.get_size fd with
  | Ok (cols, rows) -> { rows; cols }
  | Error _ -> default_size ()

let make = fun ?fd ?stdin ?stdout ?stderr ?size ?(mode = LineBuffered) () ->
  let fd_result, owns_fd =
    match fd with
    | Some fd -> (Ok fd, false)
    | None -> (Platform.open_tty (), true)
  in
  match fd_result with
  | Error _ -> Error NoTtyConnected
  | Ok tty_fd -> (
      match Platform.get_attributes tty_fd with
      | Error error ->
          if owns_fd then
            (
              let _ = Platform.close tty_fd in
              ()
            );
          Error (error_of_system_error error)
      | Ok original_attrs ->
          let detected_size =
            match size with
            | Some size -> size
            | None -> detect_size tty_fd
          in
          let terminal =
            Terminal.{
              fd = tty_fd;
              owns_fd;
              input_fd = Option.unwrap_or ~default:tty_fd stdin;
              stdout = Option.unwrap_or ~default:(Platform.stdout_fd ()) stdout;
              stderr = Option.unwrap_or ~default:(Platform.stderr_fd ()) stderr;
              original_attrs;
              size = detected_size;
              mode = LineBuffered;
              resume_mode = None;
              input_buffer = Utf8_reader.create ();
            }
          in
          match mode with
          | LineBuffered -> Ok terminal
          | Immediate -> (
              let raw_attrs = Platform.make_raw_mode original_attrs in
              match Platform.set_attributes tty_fd Platform.Now raw_attrs with
              | Ok () ->
                  terminal.mode <- Immediate;
                  Ok terminal
              | Error error ->
                  if owns_fd then
                    (
                      let _ = Platform.close tty_fd in
                      ()
                    );
                  Error (error_of_system_error error)
            )
    )

let make_raw = fun () -> make ~mode:Immediate ()

let size = fun t -> t.Terminal.size

let refresh_size = fun t ->
  match Platform.get_size t.Terminal.fd with
  | Ok (cols, rows) -> t.Terminal.size <- { rows; cols }
  | Error _ -> ()

let mode = fun t -> t.Terminal.mode

let is_tty = Platform.is_tty

let set_raw = fun t ->
  match t.Terminal.mode with
  | Immediate -> ()
  | LineBuffered -> (
      let raw_attrs = Platform.make_raw_mode t.Terminal.original_attrs in
      match Platform.set_attributes t.Terminal.fd Platform.Now raw_attrs with
      | Ok () -> t.Terminal.mode <- Immediate
      | Error _ -> ()
    )

let set_line_buffered = fun t ->
  match t.Terminal.mode with
  | LineBuffered -> ()
  | Immediate -> (
      match Platform.set_attributes t.Terminal.fd Platform.Flush t.Terminal.original_attrs with
      | Ok () -> t.Terminal.mode <- LineBuffered
      | Error _ -> ()
    )

let restore = fun t ->
  let _ = Platform.set_attributes t.Terminal.fd Platform.Now t.Terminal.original_attrs in
  t.Terminal.mode <- LineBuffered;
  if t.Terminal.owns_fd then
    let _ = Platform.close t.Terminal.fd in
    ()

let suspend = fun t ->
  match t.Terminal.mode with
  | LineBuffered -> ()
  | Immediate ->
      t.Terminal.resume_mode <- Some Immediate;
      set_line_buffered t

let resume = fun t ->
  match t.Terminal.resume_mode with
  | Some Immediate ->
      t.Terminal.resume_mode <- None;
      set_raw t
  | Some LineBuffered
  | None ->
      t.Terminal.resume_mode <- None

type read =
  | Read of string
  | End
  | Malformed of string
  | Retry

let read_from_input = fun input_fd bytes ~offset ~len ->
  if Platform.fd_equal input_fd (Platform.stdin_fd ()) then
    match IO.Stdin.read ~offset ~len bytes with
    | Ok count -> `Ok count
    | Error IO.Operation_would_block -> `Would_block
    | Error _ -> `Error
  else
    match Platform.read input_fd bytes ~offset ~len with
    | Ok count -> `Ok count
      | Error error when Kernel.SystemError.would_block error -> `Would_block
      | Error _ -> `Error

let read_utf8 = fun t ->
  match Utf8_reader.read t.Terminal.input_buffer ~read:(read_from_input t.Terminal.input_fd) with
  | `Read value -> Read value
  | `End -> End
  | `Malformed reason -> Malformed reason
  | `Retry -> Retry

let read = fun t ->
  match read_utf8 t with
  | Read value -> Ok value
  | End -> Error IO.End_of_file
  | Malformed value -> Error (IO.Unknown_error value)
  | Retry -> Error IO.Resource_unavailable_try_again

let read_line = fun t ->
  let buffer = Buffer.create ~size:256 in
  let rec loop () =
    match read t with
    | Ok value ->
        Buffer.add_string buffer value;
        if String.contains value "\n" || String.contains value "\r" then
          Ok (Buffer.contents buffer)
        else
          loop ()
    | Error error -> Error error
  in
  loop ()

let to_string = fun t ->
  "TTY { size="
  ^ Int.to_string t.Terminal.size.cols
  ^ "x"
  ^ Int.to_string t.Terminal.size.rows
  ^ "; mode="
  ^ (
    match t.Terminal.mode with
    | LineBuffered -> "line-buffered"
    | Immediate -> "immediate"
  )
  ^ "; fd="
  ^ Int.to_string (Platform.fd_to_int t.Terminal.fd)
  ^ " }"

let equal = fun left right ->
  Platform.fd_equal left.Terminal.fd right.Terminal.fd

let stdin_fd = Platform.stdin_fd

let stdout_fd = Platform.stdout_fd

let stderr_fd = Platform.stderr_fd
