(* Test with async tick like Minttea *)
open Std

type Message.t += Rendered

let main ~args:_ =
  let cols, rows = try
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (size.cols, size.rows)
    with _ -> (80, 24)
  in
  
  print "Terminal size: %dx%d\n%!" cols rows;
  
  (* Enter alt screen *)
  print "\x1b[?1049h%!";
  Unix.sleepf 0.1;
  print "\x1b[r%!";
  print "\x1b[2J\x1b[H%!";
  
  (* Build buffer like Minttea *)
  let buf = Buffer.create (cols * rows * 2) in
  Buffer.add_string buf "\x1b[H";
  Buffer.add_string buf "\x1b[0m";
  Buffer.add_string buf "\x1b[48;2;0;0;255m";
  
  for row = 0 to rows - 1 do
    for _col = 0 to cols - 1 do
      Buffer.add_string buf " ";
    done;
    if row < rows - 1 then
      Buffer.add_string buf "\r\n";
  done;
  
  (* Add text *)
  let mid_row = rows / 2 in
  let mid_col = (cols - 11) / 2 in
  Buffer.add_string buf (format "\x1b[%d;%dH" mid_row mid_col);
  Buffer.add_string buf "\x1b[38;2;255;255;255m";
  Buffer.add_string buf "Hello World";
  
  Buffer.add_string buf "\x1b[0m";
  
  let buffer_contents = Buffer.contents buf in
  
  (* NOW simulate async by spawning a process that prints after a delay *)
  let parent = self () in
  let _renderer_pid = spawn (fun () ->
    Unix.sleepf 0.033;  (* 30fps delay like Minttea *)
    print "%s%!" buffer_contents;
    send parent Rendered;
    Ok ()
  ) in
  
  (* Wait for the renderer to finish *)
  let _msg = receive_any () in
  
  Unix.sleep 3;
  print "\x1b[?1049l%!";
  print "Done!\n%!";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
