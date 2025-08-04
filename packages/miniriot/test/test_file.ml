type Message.t = 
  | Write_done
  | Read_done of string
  | Error of string

let test_file_io () =
  Miniriot.enable_trace ();
  
  let writer = Miniriot.spawn (fun () ->
    Printf.printf "Writer: Starting\n%!";
    let content = "Hello from miniriot async I/O!" in
    match Miniriot.File.write "test_output.txt" content with
    | Ok () ->
        Printf.printf "Writer: File written successfully\n%!";
        Miniriot.send (Miniriot.self ()) Write_done;
        Miniriot.Process.Normal
    | Error e ->
        Printf.printf "Writer: Error writing file: %s\n%!" 
          (match e with
           | `File_not_found -> "file not found"
           | `Permission_denied -> "permission denied"
           | `Is_a_directory -> "is a directory"
           | `Not_a_directory -> "not a directory"
           | `Already_exists -> "already exists"
           | `No_space -> "no space"
           | `Unknown s -> s);
        Miniriot.Process.Exception (Failure "write failed")
  ) in
  
  let reader = Miniriot.spawn (fun () ->
    Printf.printf "Reader: Starting\n%!";
    (* Wait for writer to finish *)
    Miniriot.sleep 0.1;
    
    match Miniriot.File.read "test_output.txt" with
    | Ok content ->
        Printf.printf "Reader: Read content: %s\n%!" content;
        Miniriot.send writer (Read_done content);
        Miniriot.Process.Normal
    | Error e ->
        Printf.printf "Reader: Error reading file: %s\n%!"
          (match e with
           | `File_not_found -> "file not found"
           | `Permission_denied -> "permission denied"
           | `Is_a_directory -> "is a directory"
           | `Not_a_directory -> "not a directory"
           | `Already_exists -> "already exists"
           | `No_space -> "no space"
           | `Unknown s -> s);
        Miniriot.Process.Exception (Failure "read failed")
  ) in
  
  let main () =
    Printf.printf "Main: Waiting for I/O operations\n%!";
    
    (* Wait for both operations to complete *)
    let rec wait_loop wrote read =
      if wrote && read then (
        Printf.printf "Main: All I/O operations completed!\n%!";
        (* Clean up *)
        ignore (Miniriot.File.remove "test_output.txt");
        Miniriot.Process.Normal
      ) else
        match Miniriot.receive () with
        | Write_done ->
            Printf.printf "Main: Got write done message\n%!";
            wait_loop true read
        | Read_done content ->
            Printf.printf "Main: Got read done message with: %s\n%!" content;
            wait_loop wrote true
        | Error msg ->
            Printf.printf "Main: Got error: %s\n%!" msg;
            Miniriot.Process.Exception (Failure msg)
    in
    
    wait_loop false false
  in
  
  Miniriot.run ~main