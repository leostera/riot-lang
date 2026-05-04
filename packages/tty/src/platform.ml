open Std

type fd = int

type termios

type when_to_apply =
  | Now
  | Drain
  | Flush

external stdin_fd: unit -> fd = "tty_stdin_fd"

external stdout_fd: unit -> fd = "tty_stdout_fd"

external stderr_fd: unit -> fd = "tty_stderr_fd"

let fd_equal = Int.equal

let fd_to_int = fun fd -> fd

external open_tty_raw: unit -> (fd, int) result = "tty_open_tty"

external close_raw: fd -> (unit, int) result = "tty_close"

external is_tty: fd -> bool = "tty_is_tty"

external get_size_raw: fd -> (int * int, int) result = "tty_get_size"

external get_attributes_raw: fd -> (termios, int) result = "tty_get_attributes"

external set_attributes_raw: fd -> when_to_apply -> termios -> (unit, int) result =
  "tty_set_attributes"

external make_raw_mode: termios -> termios = "tty_make_raw_mode"

external default_termios: unit -> termios = "tty_default_termios"

external read_raw: fd -> bytes -> int -> int -> (int, int) result = "tty_read"

external write_raw: fd -> bytes -> int -> int -> (int, int) result = "tty_write"

let decode_error = fun code -> IO.from_system_error_code code

let map_error = fun result ->
  match result with
  | Ok value -> Ok value
  | Error code -> Error (decode_error code)

let open_tty = fun () -> map_error (open_tty_raw ())

let close = fun fd -> map_error (close_raw fd)

let get_size = fun fd -> map_error (get_size_raw fd)

let get_attributes = fun fd -> map_error (get_attributes_raw fd)

let set_attributes = fun fd when_to_apply termios ->
  map_error
    (set_attributes_raw fd when_to_apply termios)

let read = fun fd bytes ~offset ~len -> map_error (read_raw fd bytes offset len)

let write = fun fd bytes ~offset ~len -> map_error (write_raw fd bytes offset len)
