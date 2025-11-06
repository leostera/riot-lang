(* Test 3-process architecture like Minttea: main, renderer, io_loop *)
open Std

type Message.t +=
  | RendererStarted of Pid.t
  | IoStarted of Pid.t
  | Render of string
  | Tick
  | Shutdown
  | ShutdownComplete
  | KeyPress of string
  | TestComplete

module Renderer = struct
  type state = {
    parent: Pid.t;
    buffer: string;
    is_altscreen: bool;
    mutable last_flushed: string;
  }

  let rec loop state =
    let selector msg =
      match msg with
      | Tick -> `select `tick
      | Render text -> `select (`render text)
      | Shutdown -> `select `shutdown
      | _ -> `skip
    in
    match receive ~selector () with
    | `tick -> handle_tick state
    | `render txt -> handle_render state txt
    | `shutdown -> handle_shutdown state

  and handle_tick state = 
    eprintln "[RENDERER.TICK] Received tick, buffer=%d bytes, last_flushed=%d bytes" 
      (String.length state.buffer) (String.length state.last_flushed);
    (* Only flush if buffer changed *)
    if String.length state.buffer > 0 && state.buffer <> state.last_flushed then begin
      eprintln "[RENDERER] About to flush %d bytes" (String.length state.buffer);
      if state.is_altscreen then begin
        eprintln "[RENDERER] print(\"\\x1b[H%%!\") - cursor home";
        print "\x1b[H%!"; (* Go home *)
        Unix.sleepf 0.01; (* Small delay for cursor to settle *)
        eprintln "[RENDERER] Slept 10ms after cursor home";
      end;
      eprintln "[RENDERER] Using Printf.printf to print buffer - %d bytes" (String.length state.buffer);
      
      (* Save buffer to file for inspection *)
      let debug_file = open_out "/tmp/renderer_buffer.txt" in
      output_string debug_file state.buffer;
      close_out debug_file;
      eprintln "[RENDERER] Saved buffer to /tmp/renderer_buffer.txt";
      
      Printf.printf "%s%!" state.buffer;
      eprintln "[RENDERER] Done printing buffer with Printf.printf";
      state.last_flushed <- state.buffer;
      
      (* Count what we actually printed *)
      let newline_count = String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 0 state.buffer in
      eprintln "[RENDERER] Buffer contained %d newlines" newline_count;
    end;
    loop state

  and handle_render state txt = 
    eprintln "[RENDERER] Received render message with %d bytes" (String.length txt);
    loop { state with buffer = txt }

  and handle_shutdown state = 
    send state.parent ShutdownComplete;
    Ok ()

  let init parent = 
    (* Enter alt screen *)
    eprintln "[RENDERER.INIT] print(\"\\x1b[?1049h%%!\") - enter alt screen";
    print "\x1b[?1049h%!";
    eprintln "[RENDERER.INIT] Sleeping 200ms for alt screen to settle";
    Unix.sleepf 0.2;
    eprintln "[RENDERER.INIT] print(\"\\x1b[r%%!\") - reset scroll region";
    print "\x1b[r%!";
    eprintln "[RENDERER.INIT] print(\"\\x1b[2J\\x1b[H%%!\") - clear screen and home";
    print "\x1b[2J\x1b[H%!";
    eprintln "[RENDERER.INIT] Sleeping 100ms before entering loop";
    Unix.sleepf 0.1;
    eprintln "[RENDERER.INIT] Alt screen setup complete, entering loop";
    
    loop { parent; buffer = ""; is_altscreen = true; last_flushed = "" }

  let start () = 
    let parent = self () in
    let pid = spawn (fun () ->
      send parent (RendererStarted (self ()));
      init parent
    ) in
    (* Wait for RendererStarted *)
    let rec wait () =
      match receive_any () with
      | RendererStarted p -> p
      | _ -> wait ()
    in
    wait ()
end

module IoLoop = struct
  type state = {
    parent: Pid.t;
  }

  let rec loop state tty =
    match Tty.read_utf8 tty with
    | Read "q" ->
        send state.parent (KeyPress "q");
        Ok ()
    | Read _key ->
        loop state tty
    | End -> Ok ()
    | Malformed _ -> loop state tty
    | Retry -> loop state tty

  let init parent =
    match Tty.make_raw () with
    | Ok tty ->
        let result = loop { parent } tty in
        Tty.restore tty;
        result
    | Error _ ->
        (* No TTY, just exit *)
        Ok ()

  let start () =
    let parent = self () in
    let _pid = spawn (fun () ->
      send parent (IoStarted (self ()));
      init parent
    ) in
    (* Wait for IoStarted *)
    let rec wait () =
      match receive_any () with
      | IoStarted p -> p
      | _ -> wait ()
    in
    wait ()
end

let main ~args:_ =
  (* 1. Detect terminal size in MAIN process before spawning *)
  let cols, rows = try
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (size.cols, size.rows)
    with _ -> (80, 24)
  in
  
  let run_id = Random.int 10000 in
  eprintln "========== RUN #%d ==========" run_id;
  eprintln "[MAIN] Terminal size: %dx%d\n" cols rows;
  
  (* 2. Build the ANSI output using Minttea's render stack *)
  let open Minttea in
  let style = (Style.default
    |> Style.width_flex 1.0
    |> Style.height_flex 1.0
    |> Style.bg (Style.color "#0000FF")
    |> Style.padding_left 2
    |> Style.padding_right 2
    |> Style.padding_top 2
    |> Style.padding_bottom 2)
  in
  
  let text_content = format "Terminal: %dx%d | 3-process test | Press 'q' to quit" cols rows in
  let element = Element.box ~style
    (Element.text ~style:(Style.default |> Style.fg (Style.color "#FFFFFF"))
      text_content)
  in
  
  let ansi_output = Minttea.Render.Pipeline.to_string element ~width:cols ~height:rows ~mode:Minttea.Render.Ansi_emitter.ContentFit in
  
  (* Debug: check output size *)
  let newline_count = String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 0 ansi_output in
  eprintln "[MAIN] Rendered %dx%d -> %d bytes, %d newlines (expect %d rows)" 
    cols rows (String.length ansi_output) newline_count rows;
  
  (* 3. Start RENDERER *)
  eprintln "[MAIN] Starting renderer...\n";
  let renderer = Renderer.start () in
  eprintln "[MAIN] Renderer ready: %s\n" (Pid.to_string renderer);
  
  (* 4. Start IO_LOOP *)
  eprintln "[MAIN] Starting IO loop...\n";
  let _io = IoLoop.start () in
  eprintln "[MAIN] IO loop ready\n";
  
  (* 5. Start ticker using Timer *)
  eprintln "[MAIN] Starting ticker at 30 FPS...\n";
  let _ticker = Timer.send_interval renderer Tick 
    ~interval:(Time.Duration.from_secs_float (1.0 /. 30.0)) in
  
  eprintln "[MAIN] All processes started\n";
  
  (* 6. Send initial render *)
  eprintln "[MAIN] Sending initial render (%d bytes)\n" (String.length ansi_output);
  send renderer (Render ansi_output);
  
  (* 7. Wait for completion (10 seconds or 'q' key) *)
  let _ = Timer.send_after (self ()) TestComplete ~after:(Time.Duration.from_secs 10) in
  
  let rec wait_for_completion () =
    let selector msg = match msg with
      | TestComplete -> `select `timeout
      | KeyPress "q" -> `select `quit
      | _ -> `skip
    in
    match receive ~selector () with
    | `timeout -> eprintln "[MAIN] Test timeout, shutting down\n"
    | `quit -> eprintln "[MAIN] Quit requested, shutting down\n"
  in
  wait_for_completion ();
  
  (* 8. Cleanup *)
  send renderer Shutdown;
  let rec wait_shutdown () =
    let selector msg = match msg with
      | ShutdownComplete -> `select ()
      | _ -> `skip
    in
    receive ~selector ()
  in
  wait_shutdown ();
  
  eprintln "[MAIN] About to exit alt screen";
  print "\x1b[?1049l%!";
  eprintln "[MAIN] Exited alt screen";
  eprintln "[MAIN] Done!\n";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
