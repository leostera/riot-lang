(* Test: test_11 but start IO AFTER first render *)
open Std

type Message.t += 
  | RendererReady 
  | PrintDone 
  | Tick
  | IoReady
  | KeyPress of string
  | StartIO

let main ~args:_ =
  (* 1. Detect terminal size *)
  let cols, rows = try
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (size.cols, size.rows)
    with _ -> (80, 24)
  in
  
  eprintln "[MAIN] Terminal size: %dx%d" cols rows;
  
  (* 2. Enter alt screen IN MAIN *)
  eprintln "[MAIN] Entering alt screen";
  print "\x1b[?1049h%!";
  Unix.sleepf 0.1;
  print "\x1b[r%!";
  print "\x1b[2J\x1b[H%!";
  eprintln "[MAIN] Alt screen entered";
  
  (* 3. Generate ANSI output IN MAIN *)
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
  
  let text_content = format "Terminal: %dx%d | test_12: IO after render!" cols rows in
  let element = Element.box ~style
    (Element.text ~style:(Style.default |> Style.fg (Style.color "#FFFFFF"))
      text_content)
  in
  
  let ansi_output = Minttea.Render.Pipeline.to_string element ~width:cols ~height:rows in
  eprintln "[MAIN] Generated %d bytes" (String.length ansi_output);
  
  let main_pid = self () in
  
  (* 4. Spawn printer FIRST *)
  let _printer = spawn (fun () ->
    eprintln "[PRINTER] Started";
    send main_pid RendererReady;
    
    (* Start ticker at 30 FPS *)
    eprintln "[PRINTER] Starting ticker at 30 FPS";
    let _ticker = Timer.send_interval (self ()) Tick 
      ~interval:(Time.Duration.from_secs_float 0.033) in
    
    (* Loop: print buffer on first tick only *)
    let rec loop count =
      if count >= 90 then begin
        eprintln "[PRINTER] Reached tick limit";
        send main_pid PrintDone;
        Ok ()
      end else begin
        let selector msg = match msg with Tick -> `select () | _ -> `skip in
        receive ~selector ();
        
        if count mod 30 = 0 then
          eprintln "[PRINTER] Tick #%d" count;
        
        if count = 0 then begin
          eprintln "[PRINTER] First tick - printing buffer";
          print "%s%!" ansi_output;
          eprintln "[PRINTER] Done printing buffer";
          (* Tell main to start IO AFTER we've rendered *)
          send main_pid StartIO;
        end;
        
        loop (count + 1)
      end
    in
    loop 0
  ) in
  
  (* 5. Wait for printer ready *)
  let rec wait_ready () =
    match receive_any () with
    | RendererReady -> eprintln "[MAIN] Printer ready"
    | _ -> wait_ready ()
  in
  wait_ready ();
  
  (* 6. Wait for StartIO signal *)
  let rec wait_start_io () =
    match receive_any () with
    | StartIO -> eprintln "[MAIN] Got StartIO signal, spawning IO process"
    | _ -> wait_start_io ()
  in
  wait_start_io ();
  
  (* 7. NOW spawn IO process AFTER first render *)
  let _io = spawn (fun () ->
    eprintln "[IO] Started AFTER first render";
    send main_pid IoReady;
    
    match Tty.make_raw () with
    | Ok tty ->
        eprintln "[IO] TTY setup complete";
        let rec loop () =
          match Tty.read_utf8 tty with
          | Read "q" ->
              eprintln "[IO] Got 'q'";
              send main_pid (KeyPress "q");
              Ok ()
          | Read _ -> loop ()
          | End -> Ok ()
          | Malformed _ -> loop ()
          | Retry -> loop ()
        in
        let result = loop () in
        Tty.restore tty;
        result
    | Error _ ->
        eprintln "[IO] No TTY";
        Ok ()
  ) in
  
  (* 8. Wait for completion (either timeout or 'q' key) *)
  let rec wait_completion () =
    match receive_any () with
    | PrintDone -> eprintln "[MAIN] Timeout"
    | KeyPress "q" -> eprintln "[MAIN] Got quit"
    | _ -> wait_completion ()
  in
  wait_completion ();
  
  (* 9. Exit alt screen *)
  eprintln "[MAIN] Exiting alt screen";
  print "\x1b[?1049l%!";
  eprintln "[MAIN] Done!";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
