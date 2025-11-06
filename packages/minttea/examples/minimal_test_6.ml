(* Test Minttea rendering pipeline without framework *)
open Std

type Message.t += Rendered

let main ~args:_ =
  (* 1. Detect terminal size in main process *)
  let cols, rows = try
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (size.cols, size.rows)
    with _ -> (80, 24)
  in
  
  print "Terminal size: %dx%d\n%!" cols rows;
  
  (* 2. Enter alt screen *)
  print "\x1b[?1049h%!";
  Unix.sleepf 0.1;
  print "\x1b[r%!";
  print "\x1b[2J\x1b[H%!";
  
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
  
  let text_content = format "Terminal: %dx%d | Using Minttea render stack!" cols rows in
  let element = Element.box ~style
    (Element.text ~style:(Style.default |> Style.fg (Style.color "#FFFFFF"))
      text_content)
  in
  
  (* 4. Use Minttea.Render.Pipeline to convert Element to ANSI string *)
  let ansi_output = Minttea.Render.Pipeline.to_string element ~width:cols ~height:rows ~mode:Minttea.Render.Ansi_emitter.ContentFit in
  
  (* 5. Spawn async renderer to print output (like minimal_test_5) *)
  let parent = self () in
  let _renderer_pid = spawn (fun () ->
    Unix.sleepf 0.033;
    print "%s%!" ansi_output;
    send parent Rendered;
    Ok ()
  ) in
  
  (* 6. Wait for render *)
  let _msg = receive_any () in
  
  (* 7. Wait and exit *)
  Unix.sleep 3;
  print "\x1b[?1049l%!";
  print "Done!\n%!";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
