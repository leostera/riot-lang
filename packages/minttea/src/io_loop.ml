open Std
open Event
open Tty

type t = Pid.t

type state = { parent: Pid.t; termios: Tty.t; parser: Ansi_parser.parser; window: Tty.size }

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
  let state =
    Tty.refresh_size state.termios;
    let size = Tty.size state.termios in
    if Int.equal size.cols state.window.cols then
      if Int.equal size.rows state.window.rows then
        state
      else
        (
          send state.parent (Input (Event.Resize { width = size.cols; height = size.rows }));
          { state with window = size }
        )
    else
      (
        send state.parent (Input (Event.Resize { width = size.cols; height = size.rows }));
        { state with window = size }
      )
  in
  (* Check for shutdown message with timeout *)
  let timeout = Time.Duration.from_millis 100 in
  let should_shutdown =
    try
      match receive_any ~timeout () with
      | Shutdown -> true
      | _ -> false
    with
    | Receive_timeout -> false
  in
  if should_shutdown then
    (
      Log.trace "[IO_LOOP] Received shutdown, exiting";
      send state.parent ShutdownComplete
    )
  else
    (
      match Tty.read_utf8 state.termios with
      | Read input ->
          Log.trace ("[IO_LOOP] READ INPUT: " ^ input);
          (* Parse input through ANSI parser *)
          let events = Ansi_parser.parse_string state.parser input in
          List.for_each events ~fn:(
            fun event -> send state.parent (Input event)
          );
          (* If no events were generated and it's a simple character *)
          if List.length events = 0 && String.length input = 1 then
            (
              let c = String.get input ~at:0 |> Option.unwrap in
              let event =
                if c = '\027' then
                  Event.KeyDown (Event.Escape, Event.NoModifier)
                else (* Regular character *)
                Event.KeyDown (Ansi_parser.parse_char c, Event.NoModifier)
              in
              send state.parent (Input event)
            );
          loop state
      | End -> send state.parent ShutdownComplete
      | Malformed _err -> loop state
      | Retry ->
          (* No data available, yield and try again *)
          yield ();
          loop state
    )

let init = fun ~parent ~tty ->
  Log.trace ("[IO_LOOP] Starting IO loop, parent=" ^ Pid.to_string parent);
  let state = {
    parent;
    termios = tty;
    parser = Ansi_parser.create ();
    window = Tty.size tty
  }
  in
  send state.parent (IoStarted (self ()));
  loop state;
  Ok ()

let start = fun ~tty () ->
  Log.trace "[Program] Starting IO loop...";
  let parent = self () in
  let pid =
    spawn
      (
        fun () -> init ~parent ~tty
      )
  in
  Log.trace ("[Program] IO loop spawned as " ^ Pid.to_string pid);
  let selector msg =
    match msg with
    | IoStarted pid' when Pid.equal pid pid' -> `select pid
    | _ -> `skip
  in
  let timeout = Time.Duration.from_secs 2 in receive ~selector ~timeout ()

let shutdown = fun pid ->
  send pid Shutdown;
  let selector msg =
    match msg with
    | ShutdownComplete -> `select ()
    | _ -> `skip
  in
  let timeout = Time.Duration.from_secs 2 in receive ~selector ~timeout ()
