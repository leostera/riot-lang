(* Test: Setup stdin in MAIN before spawning printer *)
open Std

type Message.t += 
  | RendererReady 
  | PrintDone 
  | Tick
  | KeyPress of string

let main ~args:_ =
  (* 1. Detect terminal size *)
  let cols, rows = try
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (size.cols, size.rows)
    with _ -> (80, 24)
  in
  
  eprintln "[MAIN] Terminal size: %dx%d" cols rows;
  
  (* 2. Setup TTY FIRST in main process *)
  eprintln "[MAIN] Setting up TTY";
  let tty = Tty.make_raw () |> Result.unwrap in
  eprintln "[MAIN] TTY setup complete";
  
  (* 3. Enter alt screen *)
  eprintln "[MAIN] Entering alt screen";
  print "\x1b[?1049h%!";
  Unix.sleepf 0.1;
  print "\x1b[r%!";
  print "\x1b[2J\x1b[H%!";
  eprintln "[MAIN] Alt screen entered";
  
  (* 4. Generate ANSI output *)
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
  
  let text_content = format "Terminal: %dx%d | test_13: stdin setup in main!" cols rows in
  let element = Element.box ~style
    (Element.text ~style:(Style.default |> Style.fg (Style.color "#FFFFFF"))
      text_content)
  in
  
  let ansi_output = Minttea.Render.Pipeline.to_string element ~width:cols ~height:rows in
  eprintln "[MAIN] Generated %d bytes" (String.length ansi_output);
  
  let main_pid = self () in
  
  (* 5. Spawn printer with ticker *)
  let _printer = spawn (fun () ->
    eprintln "[PRINTER] Started";
    send main_pid RendererReady;
    
    eprintln "[PRINTER] Starting ticker at 30 FPS";
    let _ticker = Timer.send_interval (self ()) Tick 
      ~interval:(Time.Duration.from_secs_float 0.033) in
    
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
        end;
        
        loop (count + 1)
      end
    in
    loop 0
  ) in
  
  (* 6. Wait for printer ready *)
  let rec wait_ready () =
    match receive_any () with
    | RendererReady -> eprintln "[MAIN] Printer ready"
    | _ -> wait_ready ()
  in
  wait_ready ();
  
  (* 7. Wait for completion *)
  let rec wait_completion () =
    match receive_any () with
    | PrintDone -> eprintln "[MAIN] Timeout"
    | KeyPress "q" -> eprintln "[MAIN] Got quit"
    | _ -> wait_completion ()
  in
  wait_completion ();
  
  (* 8. Cleanup *)
  Tty.restore tty;
  eprintln "[MAIN] Exiting alt screen";
  print "\x1b[?1049l%!";
  eprintln "[MAIN] Done!";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
