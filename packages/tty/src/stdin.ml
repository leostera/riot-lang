open Std

(* Note: Terminal control requires Unix module for tcgetattr/tcsetattr
   which are POSIX terminal APIs not abstracted in Std yet *)
let stdin_fd = Unix.descr_of_in_channel Stdlib.stdin

let set_raw_mode () =
  let termios = Unix.tcgetattr stdin_fd in
  let new_termios =
    Unix.
      { termios with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 }
  in
  Unix.tcsetattr stdin_fd TCSANOW new_termios;
  termios

let restore_mode termios = Unix.tcsetattr stdin_fd TCSANOW termios

let utf8_char_length first_byte =
  if first_byte land 0x80 = 0 then 1
  else if first_byte land 0xE0 = 0xC0 then 2
  else if first_byte land 0xF0 = 0xE0 then 3
  else if first_byte land 0xF8 = 0xF0 then 4
  else 0

let read_utf8 () =
  let bytes = Bytes.create 4 in
  let ready, _, _ = Unix.select [ stdin_fd ] [] [] 0.0001 in
  if ready = [] then `Retry
  else
    match Unix.read stdin_fd bytes 0 1 with
    | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) ->
        `Retry
    | 0 -> `End
    | 1 -> (
        let first_byte = Char.code (Bytes.get bytes 0) in
        let len = utf8_char_length first_byte in
        if len = 0 then `Malformed "Invalid UTF-8 start byte"
        else if len = 1 then `Read (Bytes.sub_string bytes 0 1)
        else
          match Unix.read stdin_fd bytes 1 (len - 1) with
          | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _))
            ->
              `Malformed "Incomplete UTF-8 sequence"
          | n when n = len - 1 -> `Read (Bytes.sub_string bytes 0 len)
          | _ -> `Malformed "Incomplete UTF-8 sequence")
    | _ -> `Malformed "Unexpected read length"

let setup () = set_raw_mode ()
let shutdown termios = restore_mode termios
