open Std

type Message.t +=
  | Timer of Timer.id Ref.t
  | ShutdownComplete

type 'model state = {
  app: 'model App.t;
  config: Super.Config.t;
  renderer: Renderer.t;
  io: Io_loop.t;
  model: 'model;
  tty: Tty.t;
}

type control_flow =
  Halt
  | Continue

let selector = fun msg ->
  match msg with
  | Io_loop.Input event -> `select event
  | Timer ref -> `select (Event.Timer ref)
  | other -> `select (Event.Custom other)

(* User custom messages *)

let rec loop = fun state ->
  Log.trace "[PROGRAM] Waiting for event...";
  let event = receive ~selector () in
  Log.trace "[PROGRAM] Received event";
  forward_event event state;
  let model, cmd = state.app.update event state.model in
  let state = {state with model;} in
  let view = state.app.view model in
  Renderer.render state.renderer view;
  (* Handle command after rendering *)
  match handle_cmd cmd state with
  | Halt -> handle_shutdown state
  | Continue -> loop state

and forward_event = fun event state ->
  (* Forward resize events to renderer *)
  (
    match event with
    | Event.Resize { width; height } -> Renderer.resize state.renderer ~width ~height
    | _ -> ()
  );

and handle_cmd = fun cmd state ->
  match cmd with
  | Quit ->
      Halt
  | Noop ->
      Continue
  | HideCursor ->
      Renderer.hide_cursor state.renderer;
      Continue
  | ShowCursor ->
      Renderer.show_cursor state.renderer;
      Continue
  | EnterAltScreen ->
      Renderer.enter_alt_screen state.renderer;
      Continue
  | ExitAltScreen ->
      Renderer.exit_alt_screen state.renderer;
      Continue
  | EnableMouse mode ->
      let renderer_mode =
        match mode with
        | Cell_motion -> Renderer.Cell_motion
        | All_motion -> Renderer.All_motion
      in
      Renderer.enable_mouse state.renderer renderer_mode;
      Continue
  | DisableMouse ->
      Renderer.disable_mouse state.renderer;
      Continue
  | EnableBracketedPaste ->
      Renderer.enable_bracketed_paste state.renderer;
      Continue
  | DisableBracketedPaste ->
      Renderer.disable_bracketed_paste state.renderer;
      Continue
  | EnableFocusTracking ->
      Renderer.enable_focus_tracking state.renderer;
      Continue
  | DisableFocusTracking ->
      Renderer.disable_focus_tracking state.renderer;
      Continue
  | SetWindowTitle title ->
      Renderer.set_window_title state.renderer title;
      Continue
  | SetTimer { ref; duration } ->
      let _ = Timer.send_after (self ()) (Timer ref) ~after:duration in
      Continue
  | Seq [] ->
      Continue
  | Seq (cmd :: rest) ->
      match handle_cmd cmd state with
      | Halt -> Halt
      | Continue -> handle_cmd (Seq rest) state

and handle_shutdown = fun state ->
  Log.trace "Shutting down Minttea";
  Renderer.show_cursor state.renderer;
  Renderer.exit_alt_screen state.renderer;
  Renderer.shutdown state.renderer;
  Io_loop.shutdown state.io;
  Ok ()

let init = fun state ->
  (* Initialize app with initial model - size will be updated by renderer *)
  let model, init_cmd = state.app.init state.model in
  let state = {state with model;} in
  (* Handle init command BEFORE first render *)
  let should_quit = handle_cmd init_cmd state in
  let initial_view = state.app.view state.model in
  Renderer.render state.renderer initial_view;
  match should_quit with
  | Halt -> Ok ()
  | Continue -> loop state

let run = fun ~app ~config ~initial_model ->
  (* Create TTY once - will be shared by both io_loop and renderer *)
  let tty =
    match Tty.make_raw () with
    | Ok tty ->
        tty
    | Error NoTtyConnected ->
        Log.error "[PROGRAM] Failed to create TTY: Not a TTY";
        panic "Not a TTY"
    | Error (SystemError (IO.Unknown_error msg)) ->
        Log.error ("[PROGRAM] Failed to create TTY: " ^ msg);
        panic msg
  in
  let renderer = Renderer.start ~config ~tty () in
  let io = Io_loop.start ~tty () in
  Log.trace ("Minttea started: renderer=" ^ Pid.to_string renderer ^ " io=" ^ Pid.to_string io);
  let state = {
    io;
    renderer;
    tty;
    app;
    config;
    model = initial_model;
  }
  in
  init state
