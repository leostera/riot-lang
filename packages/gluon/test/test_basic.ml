(** Basic tests for Gluon kqueue implementation *)

open Gluon

let test_create () =
  match Gluon.create () with
  | Ok poll ->
      Format.printf "✓ Created kqueue poll instance: %a\n" Gluon.pp poll;
      true
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false

let test_pipe_readwrite () =
  Format.printf "\nTesting pipe read/write events:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      (* Create a pipe *)
      let (read_fd, write_fd) = Unix.pipe () in
      
      (* Set non-blocking *)
      let _ = Gluon.set_nonblocking read_fd in
      let _ = Gluon.set_nonblocking write_fd in
      
      (* Register read end for readable events *)
      let read_token = Token.make "read_pipe" in
      let write_token = Token.make "write_pipe" in
      
      begin match Gluon.register poll ~fd:read_fd ~token:read_token ~interests:Interest.readable with
      | Error _ ->
          Format.printf "✗ Failed to register read fd\n";
          false
      | Ok () ->
          Format.printf "✓ Registered read fd\n";
          
          (* Poll - should timeout since no data *)
          begin match Gluon.poll ~timeout:100 poll with
          | Error _ ->
              Format.printf "✗ Poll failed\n";
              false
          | Ok events ->
              if Array.length events = 0 then
                Format.printf "✓ No events (as expected, pipe empty)\n"
              else
                Format.printf "✗ Got unexpected events on empty pipe\n";
              
              (* Write some data *)
              let data = Bytes.of_string "Hello, Gluon!" in
              let _ = Unix.write write_fd data 0 (Bytes.length data) in
              
              (* Poll again - should get readable event *)
              begin match Gluon.poll ~timeout:100 poll with
              | Error _ ->
                  Format.printf "✗ Poll failed\n";
                  false
              | Ok events ->
                  if Array.length events = 1 then begin
                    let event = events.(0) in
                    Format.printf "✓ Got 1 event: %a\n" Event.pp event;
                    
                    (* Verify it's the right event *)
                    let is_correct = 
                      Event.is_readable event &&
                      Token.equal ~eq:(=) (Event.token event) read_token
                    in
                    
                    if is_correct then begin
                      Format.printf "✓ Event is readable and has correct token\n";
                      
                      (* Read the data *)
                      let buf = Bytes.create 100 in
                      let n = Unix.read read_fd buf 0 100 in
                      let msg = Bytes.sub_string buf 0 n in
                      Format.printf "✓ Read data: %S\n" msg;
                      
                      (* Register write fd to test write events *)
                      begin match Gluon.register poll ~fd:write_fd ~token:write_token ~interests:Interest.writable with
                      | Error _ ->
                          Format.printf "✗ Failed to register write fd\n";
                          false
                      | Ok () ->
                          Format.printf "✓ Registered write fd\n";
                          
                          (* Poll - should get writable event immediately *)
                          begin match Gluon.poll ~timeout:100 poll with
                          | Error _ ->
                              Format.printf "✗ Poll failed\n";
                              false
                          | Ok events ->
                              if Array.length events >= 1 then begin
                                (* Find write event *)
                                let write_event = Array.find_opt (fun e ->
                                  Event.is_writable e && 
                                  Token.equal ~eq:(=) (Event.token e) write_token
                                ) events in
                                
                                match write_event with
                                | Some event ->
                                    Format.printf "✓ Got writable event: %a\n" Event.pp event;
                                    
                                    (* Cleanup *)
                                    Unix.close read_fd;
                                    Unix.close write_fd;
                                    true
                                | None ->
                                    Format.printf "✗ No writable event found\n";
                                    Unix.close read_fd;
                                    Unix.close write_fd;
                                    false
                              end else begin
                                Format.printf "✗ Expected writable event\n";
                                Unix.close read_fd;
                                Unix.close write_fd;
                                false
                              end
                          end
                      end
                    end else begin
                      Format.printf "✗ Event has wrong properties\n";
                      Unix.close read_fd;
                      Unix.close write_fd;
                      false
                    end
                  end else begin
                    Format.printf "✗ Expected 1 event, got %d\n" (Array.length events);
                    Unix.close read_fd;
                    Unix.close write_fd;
                    false
                  end
              end
          end
      end

let test_reregister () =
  Format.printf "\nTesting re-registration:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      let (read_fd, write_fd) = Unix.pipe () in
      let _ = Gluon.set_nonblocking read_fd in
      let _ = Gluon.set_nonblocking write_fd in
      
      let token1 = Token.make "token1" in
      let token2 = Token.make "token2" in
      
      (* Register with readable interest *)
      match Gluon.register poll ~fd:read_fd ~token:token1 ~interests:Interest.readable with
      | Error _ ->
          Format.printf "✗ Failed to register\n";
          false
      | Ok () ->
          Format.printf "✓ Initial registration successful\n";
          
          (* Re-register with different token and interests *)
          match Gluon.reregister poll ~fd:read_fd ~token:token2 ~interests:Interest.(readable + writable) with
          | Error _ ->
              Format.printf "✗ Failed to re-register\n";
              false
          | Ok () ->
              Format.printf "✓ Re-registration successful\n";
              
              (* Write data and poll *)
              let _ = Unix.write write_fd (Bytes.of_string "test") 0 4 in
              
              match Gluon.poll ~timeout:100 poll with
              | Error _ ->
                  Format.printf "✗ Poll failed\n";
                  false
              | Ok events ->
                  if Array.length events > 0 then begin
                    let event = events.(0) in
                    let has_new_token = Token.equal ~eq:(=) (Event.token event) token2 in
                    Format.printf "✓ Got event with %s token\n" 
                      (if has_new_token then "new" else "old");
                    Unix.close read_fd;
                    Unix.close write_fd;
                    has_new_token
                  end else begin
                    Format.printf "✗ No events received\n";
                    Unix.close read_fd;
                    Unix.close write_fd;
                    false
                  end

let test_deregister () =
  Format.printf "\nTesting deregistration:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      let (read_fd, write_fd) = Unix.pipe () in
      let _ = Gluon.set_nonblocking read_fd in
      
      let token = Token.make "test" in
      
      (* Register *)
      match Gluon.register poll ~fd:read_fd ~token ~interests:Interest.readable with
      | Error _ ->
          Format.printf "✗ Failed to register\n";
          false
      | Ok () ->
          Format.printf "✓ Registration successful\n";
          
          (* Deregister *)
          match Gluon.deregister poll ~fd:read_fd with
          | Error _ ->
              Format.printf "✗ Failed to deregister\n";
              false
          | Ok () ->
              Format.printf "✓ Deregistration successful\n";
              
              (* Write data *)
              let _ = Unix.write write_fd (Bytes.of_string "test") 0 4 in
              
              (* Poll - should get no events since deregistered *)
              match Gluon.poll ~timeout:100 poll with
              | Error _ ->
                  Format.printf "✗ Poll failed\n";
                  false
              | Ok events ->
                  let success = Array.length events = 0 in
                  Format.printf "%s No events after deregistration\n"
                    (if success then "✓" else "✗");
                  Unix.close read_fd;
                  Unix.close write_fd;
                  success

let test_eof_detection () =
  Format.printf "\nTesting EOF detection:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      let (read_fd, write_fd) = Unix.pipe () in
      let _ = Gluon.set_nonblocking read_fd in
      
      let token = Token.make "eof_test" in
      
      (* Register read end *)
      match Gluon.register poll ~fd:read_fd ~token ~interests:Interest.readable with
      | Error _ ->
          Format.printf "✗ Failed to register\n";
          false
      | Ok () ->
          (* Close write end to trigger EOF *)
          Unix.close write_fd;
          
          (* Poll - should get EOF event *)
          match Gluon.poll ~timeout:100 poll with
          | Error _ ->
              Format.printf "✗ Poll failed\n";
              false
          | Ok events ->
              if Array.length events > 0 then begin
                let event = events.(0) in
                let is_eof = Event.is_eof event in
                Format.printf "%s Got EOF event: %a\n" 
                  (if is_eof then "✓" else "✗") Event.pp event;
                Unix.close read_fd;
                is_eof
              end else begin
                Format.printf "✗ No events received\n";
                Unix.close read_fd;
                false
              end

let () =
  Format.printf "Gluon Basic Tests\n";
  Format.printf "=================\n\n";
  
  let tests = [
    ("create", test_create);
    ("pipe_readwrite", test_pipe_readwrite);
    ("reregister", test_reregister);
    ("deregister", test_deregister);
    ("eof_detection", test_eof_detection);
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