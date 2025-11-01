open Std
open Event
open Tty

type t = Pid.t

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

let rec loop runner =
  (* Read from stdin - will suspend process until data available *)
  match Stdin.read_utf8 () with
  | `Read key ->
      Log.trace "[IO_LOOP] READ KEY: %S\n%!" key;
      let msg =
        match key with
        | "\027" -> (
            match Stdin.read_utf8 () with
            | `Read "[" -> (
                match Stdin.read_utf8 () with
                | `Read key -> KeyDown (translate ("\027[" ^ key), No_modifier)
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
      send runner (Input msg);
      loop runner
  | `End -> ()
  | `Malformed _err -> loop runner

let start () =
  let parent = self () in
  Log.trace "[IO_LOOP] Starting IO loop, parent=%s\n%!" (Pid.to_string parent);
  spawn (fun () ->
    Log.trace "[IO_LOOP] IO loop process started\n%!";
    send parent (IoStarted (self ()));
    let termios = Stdin.setup () in
    Log.trace "[IO_LOOP] Stdin setup complete, entering main loop\n%!";
    loop parent;
    Log.trace "[IO_LOOP] Loop exited\n%!";
    Stdin.shutdown termios;
    Ok ()
  )
