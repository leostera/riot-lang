open Std
open Event
open Tty

type t = Pid.t

type state = {
  parent: Pid.t;
  sigwinch_handler: int -> unit;
  termios: Tty.t;
  parser: Ansi_parser.parser;
}

type Message.t +=
  | Input of Event.t
  | IoStarted of Pid.t
  | Shutdown
  | ShutdownComplete

let translate = fun key ->
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

let rec loop = fun state ->
  (* Check for shutdown message with timeout *)
  let timeout = Time.Duration.from_millis 100 in
  match receive_any ~timeout () with
  | Shutdown ->
      Log.trace "[IO_LOOP] Received shutdown, exiting";
      ()
  | _ -> (* Try to read with non-blocking check *)
    (
      match Tty.read_utf8 state.termios with
      | Read input ->
          Log.trace ("[IO_LOOP] READ INPUT: " ^ input);
          (* Parse input through ANSI parser *)
          let events = Ansi_parser.parse_string state.parser input in
          List.iter (fun event -> send state.parent (Input event)) events;
          (* If no events were generated and it's a simple character *)
          if List.length events = 0 && String.length input = 1 then
            (
              let c = input.[0] in
              let event =
                if c = '\027' then
                  Event.KeyDown (Event.Escape, Event.NoModifier)
                else
                  (* Regular character *)
                  Event.KeyDown (Ansi_parser.parse_char c, Event.NoModifier)
              in
              send state.parent (Input event)
            );
          loop state
      | End ->
          ()
      | Malformed _err ->
          loop state
      | Retry ->
          (* No data available, yield and try again *)
          yield ();
          loop state
    )

let sigwinch_handler = fun tty parent _signum ->
  Log.trace "[IO_LOOP] SIGWINCH received - terminal resized";
  let size = Tty.size tty in
  send parent (Input (Event.Resize {width = size.cols;height = size.rows;}))

let init = fun ~parent ~tty ->
  Log.trace ("[IO_LOOP] Starting IO loop, parent=" ^ Pid.to_string parent);
  let state = {
    parent;
    termios = tty;
    sigwinch_handler = sigwinch_handler tty parent;
    parser = Ansi_parser.create ();
  } in
  (* Set up SIGWINCH handler for terminal resize *)
  (* SIGWINCH is signal 28 on macOS/Linux *)
  let _ =
    let sigwinch = 28 in
    System.set_signal sigwinch (Signal_handle state.sigwinch_handler)
  in
  send state.parent (IoStarted (self ()));
  loop state;
  Ok ()

let start = fun ~tty () ->
  Log.trace "[Program] Starting IO loop...";
  let parent = self () in
  let pid =
    spawn (fun () -> init ~parent ~tty)
  in
  Log.trace ("[Program] IO loop spawned as " ^ Pid.to_string pid);
  let selector msg =
    match msg with
    | IoStarted pid' when Pid.equal pid pid' -> `select pid
    | _ -> `skip
  in
  let timeout = Time.Duration.from_secs 2 in
  receive ~selector ~timeout ()

let shutdown = fun pid ->
  send pid Shutdown;
  let selector msg =
    match msg with
    | ShutdownComplete -> `select ()
    | _ -> `skip
  in
  let timeout = Time.Duration.from_secs 2 in
  receive ~selector ~timeout ()
