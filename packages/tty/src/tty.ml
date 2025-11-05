open Std

(** Re-export types from Terminal module *)
type size = Terminal.size = {
  rows : int;
  cols : int;
}

type error = Terminal.error =
  | NoTtyConnected
  | SystemError of IO.error

type mode = Terminal.mode =
  | LineBuffered
  | Immediate

type t = Terminal.t = {
  fd : Unix.file_descr;
  original_attrs : Kernel.Terminal.termios;
  mutable size : size;
  mutable mode : mode;
}

(* Helper to open /dev/tty *)
let open_tty () =
  try
    match Fs.File.open_read_write (Path.v "/dev/tty") with
    | Ok file ->
        let fd = Fs.File.into_fd file |> Kernel.Fd.to_unix in
        if Kernel.Terminal.is_tty fd then Ok fd
        else (
          Unix.close fd;
          Error NoTtyConnected
        )
    | Error _ ->
        (* Fallback to stdin *)
        let fd = Kernel.Fd.to_unix IO.stdin in
        if Kernel.Terminal.is_tty fd then Ok fd
        else Error NoTtyConnected
  with
  | Unix.Unix_error (err, _, _) -> Error (SystemError (IO.Unknown_error (Unix.error_message err)))
  | e -> Error (SystemError (IO.Unknown_error (Printexc.to_string e)))

let make ?fd ?size ?(mode = LineBuffered) () =
  (* If fd is provided, use it; otherwise try to open TTY *)
  let fd_result = match fd with
    | Some f -> Ok (Kernel.Fd.to_unix f)
    | None -> open_tty ()
  in
  
  match fd_result with
  | Error e -> Error e
  | Ok unix_fd ->
      try
        let is_real_tty = Kernel.Terminal.is_tty unix_fd in
        
        (* Get termios only if this is a real TTY *)
        let original_attrs = 
          if is_real_tty then Unix.tcgetattr unix_fd
          else Unix.{
            c_ignbrk = false; c_brkint = false; c_ignpar = false; c_parmrk = false;
            c_inpck = false; c_istrip = false; c_inlcr = false; c_igncr = false;
            c_icrnl = false; c_ixon = false; c_ixoff = false; c_opost = false;
            c_obaud = 0; c_ibaud = 0; c_csize = 0; c_cstopb = 0; c_cread = false;
            c_parenb = false; c_parodd = false; c_hupcl = false; c_clocal = false;
            c_isig = false; c_icanon = false; c_noflsh = false; c_echo = false;
            c_echoe = false; c_echok = false; c_echonl = false; c_vintr = '\000';
            c_vquit = '\000'; c_verase = '\000'; c_vkill = '\000'; c_veof = '\000';
            c_veol = '\000'; c_vmin = 0; c_vtime = 0; c_vstart = '\000'; c_vstop = '\000';
          }
        in
        
        (* Get size: use provided, detect if real TTY, or default *)
        let detected_size = match size with
          | Some s -> s
          | None when is_real_tty -> (
              match Kernel.Terminal.get_size unix_fd with
              | Ok (cols, rows) -> { rows; cols }
              | Error _ -> { rows = 24; cols = 80 }
            )
          | None -> { rows = 24; cols = 80 }
        in
        
        let t = {
          fd = unix_fd;
          original_attrs;
          size = detected_size;
          mode = LineBuffered;
        } in
        
        (* Apply mode if Immediate requested and this is a real TTY *)
        (match mode, is_real_tty with
        | Immediate, true ->
            let new_attrs = 
              Unix.{ original_attrs with
                c_echo = false;      (* No echo *)
                c_icanon = false;    (* Immediate input *)
                c_icrnl = false;     (* Don't map CR→NL *)
              }
            in
            Kernel.Terminal.set_attributes unix_fd Kernel.Terminal.Now new_attrs;
            t.mode <- Immediate
        | Immediate, false ->
            (* Fake TTY - just set the mode without termios *)
            t.mode <- Immediate
        | LineBuffered, _ -> ());
        
        Ok t
      with
      | Unix.Unix_error (err, _, _) ->
          (match fd with None -> Unix.close unix_fd | Some _ -> ());
          Error (SystemError (IO.Unknown_error (Unix.error_message err)))
      | e ->
          (match fd with None -> Unix.close unix_fd | Some _ -> ());
          Error (SystemError (IO.Unknown_error (Printexc.to_string e)))

(* Convenience function for creating immediate mode TTY *)
let make_raw () = make ~mode:Immediate ()

let set_raw t =
  match t.mode with
  | Immediate -> ()
  | LineBuffered ->
      if Kernel.Terminal.is_tty t.fd then (
        let new_attrs = 
          Unix.{ t.original_attrs with
            c_echo = false;
            c_icanon = false;
            c_icrnl = false;
          }
        in
        Kernel.Terminal.set_attributes t.fd Kernel.Terminal.Now new_attrs
      );
      t.mode <- Immediate

let set_normal t =
  match t.mode with
  | LineBuffered -> ()
  | Immediate ->
      if Kernel.Terminal.is_tty t.fd then
        Kernel.Terminal.set_attributes t.fd Kernel.Terminal.Now t.original_attrs;
      t.mode <- LineBuffered

let restore t =
  set_normal t;
  Unix.close t.fd

let size t = t.size

let width t = t.size.cols

let height t = t.size.rows

let refresh_size t =
  match Kernel.Terminal.get_size t.fd with
  | Ok (cols, rows) ->
      t.size <- { rows; cols }
  | Error _ -> ()

let fd t = Kernel.Fd.of_unix t.fd

(** Read result type *)
type read = 
  | Read of string 
  | End 
  | Malformed of string 
  | Retry

(* UTF-8 input reading *)
let utf8_char_length first_byte =
  if first_byte land 0x80 = 0 then 1
  else if first_byte land 0xE0 = 0xC0 then 2
  else if first_byte land 0xF0 = 0xE0 then 3
  else if first_byte land 0xF8 = 0xF0 then 4
  else 0

let read_utf8 _t =
  let file = Fs.File.from_fd IO.stdin in
  let bytes = Bytes.create 4 in
  match Fs.File.read file bytes ~offset:0 ~len:1 with
  | Ok 0 -> End
  | Ok 1 ->
      let first_byte = Char.code (Bytes.get bytes 0) in
      let len = utf8_char_length first_byte in
      if len = 0 then Malformed "Invalid UTF-8 start byte"
      else if len = 1 then Read (Bytes.sub_string bytes 0 1)
      else (
        match Fs.File.read file bytes ~offset:1 ~len:(len - 1) with
        | Ok n when n = len - 1 -> Read (Bytes.sub_string bytes 0 len)
        | Ok _ -> Malformed "Incomplete UTF-8 sequence"
        | Error _ -> Malformed "Read error"
        )
  | Ok _ -> Malformed "Unexpected read length"
  | Error _ -> End

(* Helper to write escape sequence - use Terminal module *)
let write_escape = Terminal.write_escape

(* Terminal control - all functions take Tty.t *)
let show_cursor t = write_escape t "?25h"
let hide_cursor t = write_escape t "?25l"

let move_cursor t ~row ~col = write_escape t (format "%d;%dH" row col)
let cursor_up t n = write_escape t (format "%dA" n)
let cursor_down t n = write_escape t (format "%dB" n)
let cursor_forward t n = write_escape t (format "%dC" n)
let cursor_back t n = write_escape t (format "%dD" n)

let clear t =
  write_escape t "2J";
  write_escape t "1;1H"

let clear_line t = write_escape t "2K"
let clear_to_end_of_line t = write_escape t "0K"
let clear_to_start_of_line t = write_escape t "1K"

let enter_alt_screen t = write_escape t "?1049h"
let exit_alt_screen t = write_escape t "?1049l"

(* Mouse support *)
type mouse_mode =
  | Press
  | PressAndRelease
  | CellMotion
  | AllMotion

let enable_mouse t ?(extended=true) ?(pixels=false) mode =
  (* Enable basic mouse mode *)
  (match mode with
  | Press -> write_escape t "?9h"
  | PressAndRelease -> write_escape t "?1000h"
  | CellMotion -> write_escape t "?1002h"
  | AllMotion -> write_escape t "?1003h");
  
  (* Enable extended mode if requested *)
  if extended then write_escape t "?1006h";
  
  (* Enable pixel mode if requested *)
  if pixels then write_escape t "?1016h"

let disable_mouse t =
  write_escape t "?1000l";
  write_escape t "?1006l";
  write_escape t "?1016l"

(* Enhanced features *)
let enable_bracketed_paste t = write_escape t "?2004h"
let disable_bracketed_paste t = write_escape t "?2004l"

let enable_focus_tracking t = write_escape t "?1004h"
let disable_focus_tracking t = write_escape t "?1004l"

let enable_kitty_keyboard t = write_escape t ">1u"
let disable_kitty_keyboard t = write_escape t "<u"

let begin_sync t = write_escape t "?2026h"
let end_sync t = write_escape t "?2026l"

(* Signal handling *)
let suspend t =
  match t.mode with
  | LineBuffered -> ()
  | Immediate ->
      set_normal t;
      Unix.kill 0 Sys.sigstop
      (* When SIGCONT arrives, we'll resume in normal mode *)
      (* User can call set_raw again if needed *)

(* Re-export other modules *)
module Color = Color
module Escape_seq = Escape_seq
module Profile = Profile
module Style = Style
module Size = Size
module Input = Input
module Terminal_control = Terminal_control
