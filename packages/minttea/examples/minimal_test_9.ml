(* Test: Can a spawned process print the buffer correctly? *)
open Std

type Message.t += RendererReady | PrintDone

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
  
  let text_content = format "Terminal: %dx%d | spawned printer test!" cols rows in
  let element = Element.box ~style
    (Element.text ~style:(Style.default |> Style.fg (Style.color "#FFFFFF"))
      text_content)
  in
  
  let ansi_output = Minttea.Render.Pipeline.to_string element ~width:cols ~height:rows ~mode:Minttea.Render.Ansi_emitter.ContentFit in
  eprintln "[MAIN] Generated %d bytes" (String.length ansi_output);
  
  (* 4. Spawn a process to PRINT the buffer *)
  let main_pid = self () in
  let _printer = spawn (fun () ->
    eprintln "[PRINTER] Started, sending ready";
    send main_pid RendererReady;
    
    (* Wait for a moment *)
    Unix.sleepf 0.05;
    
    eprintln "[PRINTER] Printing buffer from spawned process";
    print "%s%!" ansi_output;
    eprintln "[PRINTER] Done printing";
    
    send main_pid PrintDone;
    Ok ()
  ) in
  
  (* 5. Wait for printer ready *)
  let rec wait_ready () =
    match receive_any () with
    | RendererReady -> eprintln "[MAIN] Printer ready"
    | _ -> wait_ready ()
  in
  wait_ready ();
  
  (* 6. Wait for print done *)
  let rec wait_done () =
    match receive_any () with
    | PrintDone -> eprintln "[MAIN] Printer done"
    | _ -> wait_done ()
  in
  wait_done ();
  
  (* 7. Wait a bit *)
  Unix.sleep 3;
  
  (* 8. Exit alt screen *)
  eprintln "[MAIN] Exiting alt screen";
  print "\x1b[?1049l%!";
  eprintln "[MAIN] Done!";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
