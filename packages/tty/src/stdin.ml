open Std
open Std.IO

(* Open /dev/tty directly for terminal control, like notcurses/bubbletea do.
   This is the controlling terminal device and is used for both input and output. *)
let get_tty_fd () =
  try
    let fd = Kernel.Fd.open_file "/dev/tty" [Kernel.Fd.OpenFlags.ReadWrite] 0o666 in
    if not (Kernel.Terminal.is_tty fd) then
      panic "/dev/tty is not a TTY";
    fd
  with
  | Failure _ ->
      (* Fallback to stdin if /dev/tty doesn't work *)
      let fd = Kernel.IO.stdin in
      if not (Kernel.Terminal.is_tty fd) then
        panic "neither /dev/tty nor stdin is a TTY";
      fd

let set_raw_mode () =
  let tty_fd = get_tty_fd () in
  let termios = Kernel.Terminal.get_attributes tty_fd in
  let new_termios = Kernel.Terminal.make_raw_mode termios in
  Kernel.Terminal.set_attributes tty_fd Kernel.Terminal.Now new_termios;
  Terminal.{ 
    fd = tty_fd;
    input = Terminal.SingleFd IO.stdin;
    stdout = IO.stdout;
    stderr = IO.stderr;
    original_attrs = termios;
    size = { rows = 24; cols = 80 }; (* Default, will be updated if needed *)
    mode = Immediate;
    input_buffer = None;
    data_buffer = None;
  }

let restore_mode terminal = 
  Kernel.Terminal.set_attributes terminal.Terminal.fd Kernel.Terminal.Now terminal.Terminal.original_attrs

let utf8_char_length first_byte =
  if first_byte land 0x80 = 0 then 1
  else if first_byte land 0xE0 = 0xC0 then 2
  else if first_byte land 0xF0 = 0xE0 then 3
  else if first_byte land 0xF8 = 0xF0 then 4
  else 0

let read_utf8 () =
  (* Use Riot's async I/O - will properly suspend/resume when data not available *)
  let file = Fs.File.from_fd IO.stdin in
  let bytes = Bytes.create 4 in
  match Fs.File.read file bytes ~offset:0 ~len:1 with
  | Ok 0 -> `End
  | Ok 1 ->
      let first_byte = Char.code (Bytes.get bytes 0) in
      let len = utf8_char_length first_byte in
      if len = 0 then `Malformed "Invalid UTF-8 start byte"
      else if len = 1 then `Read (Bytes.sub_string bytes 0 1)
      else (
        (* Read remaining bytes for multi-byte UTF-8 *)
        match Fs.File.read file bytes ~offset:1 ~len:(len - 1) with
        | Ok n when n = len - 1 -> `Read (Bytes.sub_string bytes 0 len)
        | Ok _ -> `Malformed "Incomplete UTF-8 sequence"
        | Error _ -> `Malformed "Read error"
      )
  | Ok _ -> `Malformed "Unexpected read length"
  | Error _ -> `End

let make_raw () = set_raw_mode ()
let restore termios = restore_mode termios
