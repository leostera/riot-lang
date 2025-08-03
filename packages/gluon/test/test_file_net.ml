(** Tests for File and Net modules *)

open Gluon

let test_file_io () =
  Format.printf "Testing File I/O:\n";
  
  let test_file = "/tmp/gluon_test_file.txt" in
  let test_data = "Hello, Gluon File I/O!" in
  
  (* Write test *)
  begin match File.open_write ~create:true ~truncate:true test_file with
  | Error _ ->
      Format.printf "✗ Failed to open file for writing\n";
      false
  | Ok fd ->
      let bytes = Bytes.of_string test_data in
      match File.write fd bytes with
      | Error _ ->
          Format.printf "✗ Failed to write to file\n";
          File.close fd;
          false
      | Ok n ->
          Format.printf "✓ Wrote %d bytes to file\n" n;
          File.close fd;
          
          (* Read test *)
          match File.open_read test_file with
          | Error _ ->
              Format.printf "✗ Failed to open file for reading\n";
              false
          | Ok fd ->
              let buf = Bytes.create 100 in
              match File.read fd buf with
              | Error _ ->
                  Format.printf "✗ Failed to read from file\n";
                  File.close fd;
                  false
              | Ok n ->
                  let read_data = Bytes.sub_string buf 0 n in
                  Format.printf "✓ Read %d bytes: %S\n" n read_data;
                  File.close fd;
                  
                  (* Cleanup *)
                  Unix.unlink test_file;
                  read_data = test_data
  end

let test_vectored_io () =
  Format.printf "\nTesting vectored I/O:\n";
  
  let test_file = "/tmp/gluon_test_vectored.txt" in
  
  match File.open_write ~create:true ~truncate:true test_file with
  | Error _ ->
      Format.printf "✗ Failed to open file\n";
      false
  | Ok fd ->
      (* Create multiple buffers *)
      let buf1 = Bytes.of_string "First " in
      let buf2 = Bytes.of_string "Second " in
      let buf3 = Bytes.of_string "Third" in
      
      let iovecs = Iovec.create_array [|
        (buf1, 0, Bytes.length buf1);
        (buf2, 0, Bytes.length buf2);
        (buf3, 0, Bytes.length buf3);
      |] in
      
      (* Write vectored *)
      let total_written = ref 0 in
      Array.iter (fun iovec ->
        match File.write_vectored fd iovec with
        | Error _ ->
            Format.printf "✗ Vectored write failed\n"
        | Ok n ->
            total_written := !total_written + n
      ) iovecs;
      
      Format.printf "✓ Wrote %d bytes using vectored I/O\n" !total_written;
      File.close fd;
      
      (* Read it back *)
      match File.open_read test_file with
      | Error _ ->
          Format.printf "✗ Failed to open for reading\n";
          false
      | Ok fd ->
          let buf = Bytes.create 100 in
          match File.read fd buf with
          | Error _ ->
              Format.printf "✗ Failed to read\n";
              File.close fd;
              false
          | Ok n ->
              let content = Bytes.sub_string buf 0 n in
              Format.printf "✓ Read back: %S\n" content;
              File.close fd;
              Unix.unlink test_file;
              content = "First Second Third"

let test_tcp_echo_server () =
  Format.printf "\nTesting TCP echo server:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      (* Start server *)
      let addr = Net.Addr.tcp Net.Addr.loopback 0 in (* port 0 = auto-assign *)
      
      match Net.TcpListener.bind addr with
      | Error _ ->
          Format.printf "✗ Failed to bind\n";
          false
      | Ok listener ->
          (* Get actual port *)
          let server_addr = 
            match Unix.getsockname listener with
            | Unix.ADDR_INET (addr, port) -> 
                Net.Addr.tcp (Unix.string_of_inet_addr addr) port
            | _ -> failwith "Unexpected address type"
          in
          
          Format.printf "✓ Server listening on %a\n" Net.Addr.pp server_addr;
          
          (* Register listener *)
          let listener_token = Token.make "listener" in
          let _ = Gluon.register poll ~fd:listener ~token:listener_token ~interests:Interest.readable in
          
          (* Connect client *)
          match Net.TcpStream.connect server_addr with
          | Error _ ->
              Format.printf "✗ Failed to connect\n";
              Net.TcpListener.close listener;
              false
          | Ok client_status ->
              let client = match client_status with
                | `Connected c | `In_progress c -> c
              in
              
              Format.printf "✓ Client connected\n";
              
              (* Poll for accept *)
              match Gluon.poll ~timeout:100 poll with
              | Error _ ->
                  Format.printf "✗ Poll failed\n";
                  false
              | Ok events ->
                  if Array.length events > 0 && Event.is_readable events.(0) then begin
                    (* Accept connection *)
                    match Net.TcpListener.accept listener with
                    | Error _ ->
                        Format.printf "✗ Accept failed\n";
                        false
                    | Ok (server_client, client_addr) ->
                        Format.printf "✓ Accepted connection from %a\n" Net.Addr.pp client_addr;
                        
                        (* Register server's client socket *)
                        let server_token = Token.make "server_client" in
                        let _ = Gluon.register poll ~fd:server_client ~token:server_token ~interests:Interest.readable in
                        
                        (* Send data from client *)
                        let msg = "Hello, echo server!" in
                        let _ = Net.TcpStream.write client (Bytes.of_string msg) in
                        Format.printf "✓ Client sent: %S\n" msg;
                        
                        (* Poll for data *)
                        match Gluon.poll ~timeout:100 poll with
                        | Error _ ->
                            Format.printf "✗ Poll failed\n";
                            false
                        | Ok events ->
                            if Array.exists (fun e -> 
                              Event.is_readable e && 
                              Token.equal ~eq:(=) (Event.token e) server_token
                            ) events then begin
                              (* Read on server *)
                              let buf = Bytes.create 100 in
                              match Net.TcpStream.read server_client buf with
                              | Error _ ->
                                  Format.printf "✗ Server read failed\n";
                                  false
                              | Ok n ->
                                  let received = Bytes.sub_string buf 0 n in
                                  Format.printf "✓ Server received: %S\n" received;
                                  
                                  (* Echo back *)
                                  let _ = Net.TcpStream.write server_client (Bytes.sub buf 0 n) in
                                  
                                  (* Register client for reading *)
                                  let client_token = Token.make "client" in
                                  let _ = Gluon.register poll ~fd:client ~token:client_token ~interests:Interest.readable in
                                  
                                  (* Poll for echo *)
                                  match Gluon.poll ~timeout:100 poll with
                                  | Error _ ->
                                      Format.printf "✗ Poll failed\n";
                                      false
                                  | Ok events ->
                                      if Array.exists (fun e ->
                                        Event.is_readable e &&
                                        Token.equal ~eq:(=) (Event.token e) client_token
                                      ) events then begin
                                        (* Read echo on client *)
                                        let buf = Bytes.create 100 in
                                        match Net.TcpStream.read client buf with
                                        | Error _ ->
                                            Format.printf "✗ Client read failed\n";
                                            false
                                        | Ok n ->
                                            let echoed = Bytes.sub_string buf 0 n in
                                            Format.printf "✓ Client received echo: %S\n" echoed;
                                            
                                            (* Cleanup *)
                                            Net.TcpStream.close client;
                                            Net.TcpStream.close server_client;
                                            Net.TcpListener.close listener;
                                            
                                            echoed = msg
                                      end else begin
                                        Format.printf "✗ No echo received\n";
                                        false
                                      end
                                  end
                            end else begin
                              Format.printf "✗ No data to read on server\n";
                              false
                            end
                    end
                  end else begin
                    Format.printf "✗ No accept event\n";
                    Net.TcpStream.close client;
                    Net.TcpListener.close listener;
                    false
                  end

let test_address_parsing () =
  Format.printf "\nTesting address parsing:\n";
  
  let test_cases = [
    ("127.0.0.1:8080", true);
    ("0.0.0.0:80", true);
    ("localhost:3000", false); (* Would need DNS lookup *)
    ("invalid", false);
    ("256.256.256.256:8080", false);
  ] in
  
  let all_passed = List.fold_left (fun acc (addr_str, should_succeed) ->
    match Net.Addr.parse addr_str with
    | Ok addr ->
        if should_succeed then begin
          Format.printf "✓ Parsed %s -> %a\n" addr_str Net.Addr.pp addr;
          acc
        end else begin
          Format.printf "✗ Unexpectedly parsed invalid address: %s\n" addr_str;
          false
        end
    | Error _ ->
        if not should_succeed then begin
          Format.printf "✓ Correctly rejected %s\n" addr_str;
          acc
        end else begin
          Format.printf "✗ Failed to parse valid address %s\n" addr_str;
          false
        end
  ) true test_cases in
  
  all_passed

let () =
  Format.printf "Gluon File and Net Tests\n";
  Format.printf "========================\n\n";
  
  let tests = [
    ("file_io", test_file_io);
    ("vectored_io", test_vectored_io);
    ("tcp_echo_server", test_tcp_echo_server);
    ("address_parsing", test_address_parsing);
  ] in
  
  let results = List.map (fun (name, test) ->
    Format.printf "Running %s test...\n" name;
    let result = test () in
    Format.printf "\n";
    (name, result)
  ) tests in
  
  let passed = List.filter (fun (_, result) -> result) results |> List.length in
  let total = List.length results in
  
  Format.printf "Summary: %d/%d tests passed\n" passed total;
  
  if passed = total then
    exit 0
  else
    exit 1