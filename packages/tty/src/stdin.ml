open Std

(* Open /dev/tty directly for terminal control, like notcurses/bubbletea do.
   This is the controlling terminal device and is used for both input and output. *)
let get_tty_fd () =
  try
    let fd = Unix.openfile "/dev/tty" [Unix.O_RDWR] 0o666 in
    if not (Unix.isatty fd) then (
      Unix.close fd;
      raise (Unix.Unix_error (Unix.ENOTTY, "get_tty_fd", "/dev/tty is not a TTY"))
    );
    fd
  with
  | Unix.Unix_error _ as e ->
      (* Fallback to stdin if /dev/tty doesn't work *)
      let fd = Kernel.Fd.to_unix Kernel.IO.stdin in
      if not (Unix.isatty fd) then
        raise (Unix.Unix_error (Unix.ENOTTY, "get_tty_fd", "neither /dev/tty nor stdin is a TTY"));
      fd

let set_raw_mode () =
  let tty_fd = get_tty_fd () in
  let termios = Unix.tcgetattr tty_fd in
  
  (* Minimal "raw mode" configuration following notcurses' cbreak_mode approach:
     Only change 3 termios flags, leave everything else untouched.
     
     This is proven to work reliably across all terminals because it inherits
     the terminal's existing working configuration for output processing, character
     size, parity, etc. See notcurses termdesc.c:cbreak_mode() for reference.
     
     Note: Despite the name "raw mode", this is technically "cbreak mode" - it
     disables canonical mode and echo, but preserves signal handling and output
     processing, which is what TUI applications actually need. *)
  let new_termios =
    Unix.{ termios with 
      (* Local flags: disable echo and canonical mode ONLY *)
      c_echo = false;    (* Don't echo input characters to screen *)
      c_icanon = false;  (* Immediate input availability, no line buffering *)
      
      (* Input flags: disable CR to NL mapping ONLY *)
      c_icrnl = false;   (* Don't map Ctrl+M (carriage return) to Ctrl+J (newline) *)
      
      (* Everything else UNTOUCHED: output processing (c_opost), character size
         (c_csize), parity (c_parenb), control characters (c_vmin, c_vtime), etc.
         are all inherited from the terminal's existing configuration, which is
         already set up correctly for ANSI escape sequence rendering. *)
    }
  in
  (* Use TCSANOW for immediate effect, not TCSAFLUSH which discards unread input *)
  Unix.tcsetattr tty_fd Unix.TCSANOW new_termios;
  Terminal.{ 
    fd = tty_fd; 
    original_attrs = termios;
    size = { rows = 24; cols = 80 }; (* Default, will be updated if needed *)
    mode = Immediate;
  }

let restore_mode terminal = 
  Unix.tcsetattr terminal.Terminal.fd Unix.TCSANOW terminal.Terminal.original_attrs;
  Unix.close terminal.Terminal.fd

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
