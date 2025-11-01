module Cmd = Command
open Std

type Message.t += 
  | Timer of Timer_ref.t
  | RendererStopped
  | IoStopped

type 'model t = { app : 'model App.t; config : Config.t }

let make ~app ~config = { app; config }

let run t initial_model =
  (* Start renderer *)
  let renderer_pid = Renderer.start ~config:t.config () in
  
  (* Wait for renderer to be ready *)
  let renderer = 
    let selector = function
      | Renderer.RendererStarted pid -> `select pid
      | _ -> `skip
    in
    receive ~selector ()
  in
  
  (* Start IO loop *)
  Log.trace "[Program] Starting IO loop...\n%!";
  let io_pid = Io_loop.start () in
  Log.trace "[Program] IO loop spawned as %s\n%!" (Pid.to_string io_pid);
  
  (* Wait for IO to be ready *)
  Log.trace "[Program] Waiting for IoStarted message...\n%!";
  let io = 
    let selector = function
      | Io_loop.IoStarted pid -> 
          Log.trace "[Program] Received IoStarted from %s\n%!" (Pid.to_string pid);
          `select pid
      | _ -> `skip
    in
    receive ~selector ()
  in
  Log.trace "[Program] IO is ready: %s\n%!" (Pid.to_string io);
  
  Log.trace "Minttea started: renderer=%s io=%s" 
    (Pid.to_string renderer) (Pid.to_string io);

  (* Handle commands by sending messages to renderer. Returns true if should quit. *)
  let rec handle_cmd cmd =
    match (cmd : Cmd.t) with
    | Quit -> 
        Log.trace "[Program] Command.Quit received! Returning true to shutdown";
        true
    | Noop -> false
    | Hide_cursor -> Renderer.hide_cursor renderer; false
    | Show_cursor -> Renderer.show_cursor renderer; false
    | Enter_alt_screen -> Renderer.enter_alt_screen renderer; false
    | Exit_alt_screen -> Renderer.exit_alt_screen renderer; false
    | Enable_mouse mode -> 
        let renderer_mode = match mode with
          | Cell_motion -> Renderer.Cell_motion
          | All_motion -> Renderer.All_motion
        in
        Renderer.enable_mouse renderer renderer_mode;
        false
    | Disable_mouse -> Renderer.disable_mouse renderer; false
    | Enable_bracketed_paste -> Renderer.enable_bracketed_paste renderer; false
    | Disable_bracketed_paste -> Renderer.disable_bracketed_paste renderer; false
    | Enable_focus_tracking -> Renderer.enable_focus_tracking renderer; false
    | Disable_focus_tracking -> Renderer.disable_focus_tracking renderer; false
    | Set_window_title title -> Renderer.set_window_title renderer title; false
    | Batch cmds -> List.exists handle_cmd cmds
    | Sequence cmds -> List.exists handle_cmd cmds
    | Seq cmds -> List.exists handle_cmd cmds
    | Set_timer (ref, after_secs) ->
        let _ = Timer.send_after (self ()) (Timer ref) ~after:after_secs in
        false
    | Query_window_size -> false
  in

  (* Initialize app *)
  let init_cmd = t.app.init initial_model in
  let should_quit = handle_cmd init_cmd in

  if should_quit then (
    Log.trace "Quit during initialization";
    Ok ()
  ) else (
  Renderer.render renderer (t.app.view initial_model);

  (* Shutdown helper *)
  let shutdown () =
    Log.trace "Shutting down Minttea";
    Renderer.show_cursor renderer;
    Renderer.exit_alt_screen renderer;
    Renderer.shutdown renderer;
    send io Io_loop.Shutdown;
    (* Give children a moment to process shutdown before main exits *)
    Unix.sleepf 0.1
  in

  (* Main event loop *)
  let rec loop model =
      let selector = function
        | Io_loop.Input event -> `select (`Event event)
        | Timer ref -> `select (`Timer ref)
        | _ -> `skip
      in
      
      match receive ~selector () with
      | `Timer ref ->
          Log.trace "[Program] Received Timer event";
          let model, cmd = t.app.update (Event.Timer ref) model in
          if handle_cmd cmd then shutdown ()
          else (
            Renderer.render renderer (t.app.view model);
            loop model
          )
      | `Event event ->
          let event_str = Format.asprintf "%a" Event.pp event in
          Log.trace "[Program] Received Event: %s" event_str;
          let model, cmd = t.app.update event model in
          if handle_cmd cmd then shutdown ()
          else (
            Renderer.render renderer (t.app.view model);
            loop model
          )
    in
    
    loop initial_model;
    Log.trace "Program finished";
    Ok ()
  )
