open Std

(* We still need Unix fd for terminal control (tcgetattr/tcsetattr) *)
let stdin_fd = Kernel.Fd.to_unix Kernel.IO.stdin

let set_raw_mode () =
  let termios = Unix.tcgetattr stdin_fd in
  let new_termios =
    Unix.
      { termios with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 }
  in
  Unix.tcsetattr stdin_fd TCSANOW new_termios;
  (* Set stdin to non-blocking for async I/O *)
  Unix.set_nonblock stdin_fd;
  termios

let restore_mode termios = 
  Unix.clear_nonblock stdin_fd;
  Unix.tcsetattr stdin_fd TCSANOW termios

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

let setup () = set_raw_mode ()
let shutdown termios = restore_mode termios
