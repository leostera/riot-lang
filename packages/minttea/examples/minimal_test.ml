(* Minimal test - exactly mimics the Python script *)

let () =
  (* Get terminal size *)
  let get_size () =
    let ic = Unix.open_process_in "stty size" in
    let line = input_line ic in
    let _ = Unix.close_process_in ic in
    match String.split_on_char ' ' line with
    | [rows_str; cols_str] -> 
        (int_of_string cols_str, int_of_string rows_str)
    | _ -> (80, 24)
  in
  
  let cols, rows = get_size () in
  Printf.eprintf "Terminal size: %dx%d\n%!" cols rows;
  Printf.eprintf "Starting...\n%!";
  
  (* 1. Enter alt screen *)
  Printf.printf "\x1b[?1049h%!";
  
  (* 2. Sleep *)
  Unix.sleepf 0.1;
  
  (* 3. Reset scroll region *)
  Printf.printf "\x1b[r%!";
  
  (* 4. Clear and home *)
  Printf.printf "\x1b[2J\x1b[H%!";
  
  (* 5. Blue background *)
  Printf.printf "\x1b[48;2;0;0;255m%!";
  
  (* 6. Fill screen *)
  for row = 0 to rows - 1 do
    for _col = 0 to cols - 1 do
      Printf.printf " ";
    done;
    if row < rows - 1 then
      Printf.printf "\r\n";
  done;
  Printf.printf "%!";
  
  (* 7. Write text *)
  let mid_row = rows / 2 in
  let mid_col = (cols - 11) / 2 in
  Printf.printf "\x1b[%d;%dH" mid_row mid_col;
  Printf.printf "\x1b[38;2;255;255;255m";
  Printf.printf "Hello World%!";
  
  (* 8. Reset *)
  Printf.printf "\x1b[0m%!";
  
  (* 9. Wait *)
  Unix.sleep 3;
  
  (* 10. Exit alt screen *)
  Printf.printf "\x1b[?1049l%!";
  
  Printf.eprintf "Done!\n%!";
