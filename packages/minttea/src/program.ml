module Cmd = Command
open Std

type Message.t += 
  | Timer of Timer.id Ref.t
  | ShutdownComplete

type 'model t = { app : 'model App.t; config : Config.t }

let make ~app ~config = { app; config }

let run t initial_model =
  (* Create TTY once - will be shared by both io_loop and renderer *)
  let tty = match Tty.make_raw () with
    | Ok t -> t
    | Error NoTtyConnected -> 
        Log.error "[PROGRAM] Failed to create TTY: Not a TTY";
        failwith "Not a TTY"
    | Error (SystemError (IO.Unknown_error msg)) ->
        Log.error "[PROGRAM] Failed to create TTY: %s" msg;
        failwith msg
  in
  
  (* Start renderer with TTY handle *)
  let renderer_pid = Renderer.start ~config:t.config ~tty () in
  
  (* Wait for renderer to be ready and capture any early Resize event *)
  let renderer, initial_model = 
    let rec wait_for_renderer model = 
      match receive_any () with
      | Renderer.RendererStarted pid -> (pid, model)
      | Io_loop.Input (Event.Resize { width; height }) ->
          (* Update model with size before init *)
          let updated_model, _ = t.app.update (Event.Resize { width; height }) model in
          wait_for_renderer updated_model
      | _ -> wait_for_renderer model
    in
    wait_for_renderer initial_model
  in
  
  (* Start IO loop with TTY handle *)
  Log.trace "[Program] Starting IO loop...\n%!";
  let io_pid = Io_loop.start ~tty () in
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
    | Enter_alt_screen -> 
        Renderer.enter_alt_screen renderer; false
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
    | Set_timer { ref; duration } ->
        let _ = Timer.send_after (self ()) (Timer ref) ~after:duration in
        false
    | Query_window_size -> false
  in

    let shutdown () =
      Log.trace "Shutting down Minttea";
      Renderer.show_cursor renderer;
      Renderer.exit_alt_screen renderer;
      Renderer.shutdown renderer;
      send io Io_loop.Shutdown;

      (* Wait for renderer to acknowledge shutdown *)
      let rec wait_for_renderer () =
        match receive_any () with
        | Renderer.ShutdownComplete ->
          Log.trace "Renderer shutdown complete"
        | _ -> wait_for_renderer ()
      in
      wait_for_renderer ()
    in
  
  (* Initialize app with initial model - size will be updated by renderer *)
  let model, init_cmd = t.app.init initial_model in
    (* Main event loop *)
    let rec loop model last_view =
      let selector msg = match msg with
        | Io_loop.Input event -> `select event
        | Timer ref -> `select (Event.Timer ref)
        | ShutdownComplete -> `skip  (* Handled in shutdown sequence *)
        | other -> `select (Event.Custom other)  (* User custom messages *)
      in

      let event = receive ~selector () in
      
      (* Forward resize events to renderer *)
      (match event with
      | Event.Resize { width; height } ->
          Renderer.resize renderer ~width ~height
      | _ -> ());
      
      let model, cmd = t.app.update event model in
      let view = t.app.view model in
      (* Only render if view changed *)
      if view <> last_view then begin
        Log.debug "[PROGRAM] View changed, sending render to renderer";
        Renderer.render renderer view
      end;
      (* Handle command after rendering *)
      if handle_cmd cmd then shutdown ()
      else loop model view
    in

    let initial_view = t.app.view model in
    Log.debug "[PROGRAM] Sending initial render";
    Renderer.render renderer initial_view;

    let _should_quit = handle_cmd init_cmd in

    loop model initial_view;
    Log.trace "Program finished";
    Ok ()
