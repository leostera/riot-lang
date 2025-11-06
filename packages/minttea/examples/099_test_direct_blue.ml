open Std

let () =
  (* Enter alt screen *)
  print_string "\x1b[?1049h";
  (* Clear screen *)  
  print_string "\x1b[2J";
  (* Move cursor home *)
  print_string "\x1b[H";
  
  (* Set blue background *)
  print_string "\x1b[48;2;0;0;255m";
  
  (* Fill 82x46 terminal with spaces *)
  for row = 1 to 46 do
    for _col = 1 to 82 do
      print_char ' '
    done;
    if row < 46 then print_string "\r\n"
  done;
  
  (* Reset colors *)
  print_string "\x1b[0m";
  
  (* Flush output *)
  flush stdout;
  
  (* Wait 2 seconds *)
  Unix.sleep 2;
  
  (* Exit alt screen *)
  print_string "\x1b[?1049l";
  flush stdout