(* Test: Main process does rendering, separate IO process for input *)
open Std

type Message.t += 
  | Tick
  | KeyPress of string

let main ~args:_ =
  (* 1. Setup TTY FIRST *)
  eprintln "[MAIN] Setting up TTY";
  let tty_opt = match Tty.make_raw () with
    | Ok t -> Some t
    | Error _ -> (eprintln "[MAIN] No TTY available"; None)
  in
  
  (* 2. Detect terminal size *)
  let cols, rows = try
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (size.cols, size.rows)
    with _ -> (80, 24)
  in
  
  eprintln "[MAIN] Terminal size: %dx%d" cols rows;
  
  (* 3. Enter alt screen - with explicit flush after each escape sequence *)
  eprintln "[MAIN] Entering alt screen";
  print "\x1b[?1049h";  (* Enter alternate buffer *)
  flush stdout;
  print "\x1b[?25l";    (* Hide cursor *)
  flush stdout;
  print "\x1b[2J\x1b[H"; (* Clear and home *)
  flush stdout;
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
  
  let text_content = format "Terminal: %dx%d | test_11: main renders!" cols rows in
  let element = Element.box ~style
    (Element.text ~style:(Style.default |> Style.fg (Style.color "#FFFFFF"))
      text_content)
  in
  
  let ansi_output = Minttea.Render.Pipeline.to_string element ~width:cols ~height:rows in
  eprintln "[MAIN] Generated %d bytes" (String.length ansi_output);
  
  (* 5. Spawn IO process for keyboard input *)
  let main_pid = self () in
  let _io = spawn (fun () ->
    eprintln "[IO] Started";

    let rec loop tty =
      match Tty.read_utf8 tty with
      | Read "q" ->
          eprintln "[IO] Got 'q'";
          send main_pid (KeyPress "q");
          Ok ()
      | Read _ -> loop tty
      | End -> Ok ()
      | Malformed _ -> loop tty
      | Retry -> loop tty
    in
    match tty_opt with
    | Some tty -> loop tty
    | None -> Ok ()
  ) in
  
  (* 6. Do initial render BEFORE starting event loop *)
  eprintln "[MAIN] Doing initial render (%d bytes)" (String.length ansi_output);
  print "%s" ansi_output;
  flush stdout;
  eprintln "[MAIN] Initial render done";
  
  (* 7. Start ticker for rendering loop *)
  eprintln "[MAIN] Starting ticker at 30 FPS";
  let _ticker = Timer.send_interval main_pid Tick 
    ~interval:(Time.Duration.from_secs_float 0.033) in
  
  (* 8. Main rendering loop *)
  let rec render_loop count =
    if count >= 90 then begin
      eprintln "[MAIN] Reached tick limit";
    end else begin
      (* Wait for next tick or quit key *)
      let selector msg = match msg with 
        | Tick -> `select msg
        | KeyPress _ -> `select msg
        | _ -> `skip 
      in
      match receive ~selector () with
      | KeyPress "q" -> 
          eprintln "[MAIN] Got quit"
      | Tick ->
          if count mod 30 = 0 then
            eprintln "[MAIN] Tick #%d" count;
          
          (* Render on first tick *)
          if count = 0 then begin
            eprintln "[MAIN] First tick - printing buffer (%d bytes)" (String.length ansi_output);
            print "%s" ansi_output;
            flush stdout;
            eprintln "[MAIN] Done printing buffer - flushed";
            Unix.sleepf 0.1; (* Give terminal time to render *)
          end;
          
          render_loop (count + 1)
      | _ -> render_loop count
    end
  in
  render_loop 0;
  
  (* 9. Cleanup *)
  (match tty_opt with
  | Some t -> Tty.restore t; eprintln "[MAIN] TTY restored"
  | None -> ());
  
  eprintln "[MAIN] Exiting alt screen";
  print "\x1b[?1049l%!";
  eprintln "[MAIN] Done!";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
