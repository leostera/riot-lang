open Std
open Event
open Tty

type t = Pid.t

type state = {
  parent: Pid.t;
  sigwinch_handler: int -> unit;
  termios: Tty.t;
}

type Message.t += 
  | Input of Event.t
  | IoStarted of Pid.t
  | Shutdown

let translate key =
  match key with
  | " " -> Space
  | "\027" -> Escape
  | "\027[A" -> Up
  | "\027[B" -> Down
  | "\027[C" -> Right
  | "\027[D" -> Left
  | "\127" -> Backspace
  | "\n" -> Enter
  | key -> Key key

let rec loop state =
  (* Read from stdin - will suspend process until data available *)
  match Tty.read_utf8 state.termios with
  | Read key ->
      Log.trace "[IO_LOOP] READ KEY: %S\n%!" key;
      let msg =
        match key with
        | "\027" -> (
            match Tty.read_utf8 state.termios with
            | Read "[" -> (
                match Tty.read_utf8 state.termios with
                | Read key -> KeyDown (translate ("\027[" ^ key), No_modifier)
                | _ -> KeyDown (translate key, No_modifier))
            | _ -> KeyDown (translate key, No_modifier))
        | "\n" -> KeyDown (translate key, No_modifier)
        | key when key >= "\x01" && key <= "\x1a" ->
            let key =
              key.[0] |> Char.code |> ( + ) 96 |> Char.chr |> String.make 1
            in
            KeyDown (translate key, Ctrl)
        | key -> KeyDown (translate key, No_modifier)
      in
      send state.parent (Input msg);
      loop state
  | End -> ()
  | Malformed _err -> loop state
  | Retry -> loop state

let sigwinch_handler tty parent _signum =
  Log.trace "[IO_LOOP] SIGWINCH received - terminal resized\n%!";
  let size = Tty.size tty in
  send parent (Input (Event.Resize { width = size.cols; height = size.rows }))

let init ~parent ~tty = 
  Log.trace "[IO_LOOP] Starting IO loop, parent=%s\n%!" (Pid.to_string parent);
  let state = { parent; termios = tty; sigwinch_handler = sigwinch_handler tty parent } in

  (* Set up SIGWINCH handler for terminal resize *)
  (* SIGWINCH is signal 28 on macOS/Linux *)
  let _ = 
    let sigwinch = 28 in
    Sys.set_signal sigwinch (Sys.Signal_handle state.sigwinch_handler) 
  in

  send state.parent (IoStarted (self ()));

  loop state;
  Ok ()

let start ~tty () =
  let parent = self () in
  spawn (fun () -> init ~parent ~tty)
