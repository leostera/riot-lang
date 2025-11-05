(* Minimal test - exactly mimics the Python script *)
open Std

let main ~args:_ =
  (* Get terminal size using Tty *)
  let cols, rows = try
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (size.cols, size.rows)
    with _ -> (80, 24)
  in
  print "Terminal size: %dx%d\n%!" cols rows;
  print "Starting...\n%!";
  
  (* 1. Enter alt screen *)
  print "\x1b[?1049h%!";
  
  (* 2. Sleep *)
  Unix.sleepf 0.1;
  
  (* 3. Reset scroll region *)
  print "\x1b[r%!";
  
  (* 4. Clear and home *)
  print "\x1b[2J\x1b[H%!";
  
  (* 5. Blue background *)
  print "\x1b[48;2;0;0;255m%!";
  
  (* 6. Fill screen *)
  for row = 0 to rows - 1 do
    for _col = 0 to cols - 1 do
      print " ";
    done;
    if row < rows - 1 then
      print "\r\n";
  done;
  print "%!";
  
  (* 7. Write text *)
  let mid_row = rows / 2 in
  let mid_col = (cols - 11) / 2 in
  print "\x1b[%d;%dH" mid_row mid_col;
  print "\x1b[38;2;255;255;255m";
  print "Hello World%!";
  
  (* 8. Reset *)
  print "\x1b[0m%!";
  
  (* 9. Wait *)
  Unix.sleep 3;
  
  (* 10. Exit alt screen *)
  print "\x1b[?1049l%!";
  
  print "Done!\n%!";
  Ok ()

let () = Miniriot.run ~main ~args:Std.Env.args ()
