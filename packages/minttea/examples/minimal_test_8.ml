(* Test: minimal_test_6 + ticker loop to isolate the rendering issue *)
open Std

type Message.t += Tick

let main ~args:_ =
  (* 1. Detect terminal size in main process *)
  let cols, rows = try
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (size.cols, size.rows)
    with _ -> (80, 24)
  in
  
  eprintln "[MAIN] Terminal size: %dx%d" cols rows;
  
  (* 2. Enter alt screen *)
  eprintln "[MAIN] Entering alt screen";
  print "\x1b[?1049h%!";
  Unix.sleepf 0.1;
  print "\x1b[r%!";
  print "\x1b[2J\x1b[H%!";
  eprintln "[MAIN] Alt screen entered";
  
  (* 3. Create Element using Minttea's Element/Style API *)
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
  
  let text_content = format "Terminal: %dx%d | minimal_test_8 with ticker!" cols rows in
  let element = Element.box ~style
    (Element.text ~style:(Style.default |> Style.fg (Style.color "#FFFFFF"))
      text_content)
  in
  
  (* 4. Generate ANSI output *)
  eprintln "[MAIN] Generating ANSI output";
  let ansi_output = Minttea.Render.Pipeline.to_string element ~width:cols ~height:rows ~mode:Minttea.Render.Ansi_emitter.ContentFit in
  let newline_count = String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 0 ansi_output in
  eprintln "[MAIN] Generated %d bytes, %d newlines" (String.length ansi_output) newline_count;
  
  (* 5. Start ticker at 30 FPS *)
  eprintln "[MAIN] Starting ticker at 30 FPS";
  let _ticker = Timer.send_interval (self ()) Tick 
    ~interval:(Time.Duration.from_secs_float 0.033) in
  
  (* 6. Loop: wait for ticks and print buffer on first tick only *)
  let rec loop count =
    if count >= 90 then begin (* ~3 seconds at 30 FPS *)
      eprintln "[MAIN] Reached tick limit, exiting";
    end else begin
      let selector msg = match msg with Tick -> `select () | _ -> `skip in
      receive ~selector ();
      eprintln "[MAIN] Tick #%d" count;
      
      if count = 0 then begin
        eprintln "[MAIN] First tick - printing buffer (%d bytes)" (String.length ansi_output);
        print "%s%!" ansi_output;
        eprintln "[MAIN] Done printing buffer";
      end;
      
      loop (count + 1)
    end
  in
  loop 0;
  
  (* 7. Exit alt screen *)
  eprintln "[MAIN] Exiting alt screen";
  print "\x1b[?1049l%!";
  eprintln "[MAIN] Done!";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
