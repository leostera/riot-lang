(** Stress tests for Gluon - test edge cases and high load *)

open Gluon

let test_many_fds () =
  Format.printf "Testing many file descriptors:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      (* Create many pipes *)
      let num_pipes = 100 in
      let pipes = ref [] in
      
      try
        (* Register many fds *)
        for i = 0 to num_pipes - 1 do
          let (r, w) = match Gluon.pipe () with
            | Ok p -> p
            | Error _ -> raise Exit
          in
          
          let token = Token.make i in
          match Gluon.register poll ~fd:r ~token ~interests:Interest.readable with
          | Error _ ->
              Format.printf "✗ Failed to register fd %d\n" i;
              raise Exit
          | Ok () ->
              pipes := (i, r, w) :: !pipes
        done;
        
        Format.printf "✓ Registered %d file descriptors\n" num_pipes;
        
        (* Write to random fds and verify events *)
        let test_indices = [0; 25; 50; 75; 99] in
        
        List.iter (fun idx ->
          (* Find pipe with the correct index *)
          let found = ref false in
          List.iter (fun (i, _, write_fd) ->
            if i = idx && not !found then begin
              found := true;
              let _ = Unix.write write_fd (Bytes.of_string "x") 0 1 in
              ()
            end
          ) !pipes;
          if not !found then begin
            Format.printf "✗ Pipe %d not found\n" idx;
            raise Exit
          end
        ) test_indices;
        
        (* Poll and verify we get the right events *)
        match Gluon.poll ~timeout:100 ~max_events:1024 poll with
        | Error _ ->
            Format.printf "✗ Poll failed\n";
            false
        | Ok events ->
            Format.printf "✓ Got %d events\n" (Array.length events);
            
            (* Verify we got events for the fds we wrote to *)
            let received_indices = Array.map (fun event ->
              Token.unsafe_to_value (Event.token event)
            ) events |> Array.to_list in
            
            let all_found = List.for_all (fun expected ->
              List.mem expected received_indices
            ) test_indices in
            
            if all_found then
              Format.printf "✓ All expected events received\n"
            else
              Format.printf "✗ Some expected events missing\n";
            
            (* Cleanup *)
            List.iter (fun (_, r, w) ->
              Unix.close r;
              Unix.close w
            ) !pipes;
            
            all_found
      with Exit ->
        (* Cleanup on error *)
        List.iter (fun (_, r, w) ->
          try Unix.close r with _ -> ();
          try Unix.close w with _ -> ()
        ) !pipes;
        false

let test_reregister_stress () =
  Format.printf "\nTesting rapid re-registration:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      let (r, w) = match Gluon.pipe () with
        | Ok p -> p
        | Error _ -> exit 1
      in
      
      try
        (* Initial registration *)
        let token = Token.make 0 in
        let _ = Gluon.register poll ~fd:r ~token ~interests:Interest.readable in
        
        (* Rapid re-registrations with different interests *)
        let patterns = [
          Interest.readable;
          Interest.writable;
          Interest.(readable + writable);
          Interest.readable;
        ] in
        
        let success = ref true in
        
        for i = 0 to 99 do
          let interests = List.nth patterns (i mod List.length patterns) in
          let token = Token.make i in
          
          match Gluon.reregister poll ~fd:r ~token ~interests with
          | Error _ ->
              Format.printf "✗ Re-registration %d failed\n" i;
              success := false;
              raise Exit
          | Ok () -> ()
        done;
        
        if !success then
          Format.printf "✓ 100 rapid re-registrations successful\n";
        
        Unix.close r;
        Unix.close w;
        !success
        
      with Exit ->
        Unix.close r;
        Unix.close w;
        false

let test_concurrent_events () =
  Format.printf "\nTesting concurrent events on multiple fds:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      (* Create several pipes *)
      let pipes = Array.init 5 (fun i ->
        let (r, w) = match Gluon.pipe () with
          | Ok p -> p
          | Error _ -> raise Exit
        in
        
        let token = Token.make (Printf.sprintf "pipe_%d" i) in
        match Gluon.register poll ~fd:r ~token ~interests:Interest.readable with
        | Error _ ->
            Format.printf "✗ Failed to register pipe %d\n" i;
            raise Exit
        | Ok () -> (r, w, token)
      ) in
      
      (* Write to all pipes *)
      Array.iter (fun (_, w, _) ->
        let _ = Unix.write w (Bytes.of_string "data") 0 4 in
        ()
      ) pipes;
      
      (* Poll once and verify all events arrive *)
      match Gluon.poll ~timeout:100 ~max_events:10 poll with
      | Error _ ->
          Format.printf "✗ Poll failed\n";
          false
      | Ok events ->
          let expected_count = Array.length pipes in
          let actual_count = Array.length events in
          
          Format.printf "%s Got %d/%d events\n" 
            (if actual_count = expected_count then "✓" else "✗")
            actual_count expected_count;
          
          (* Verify each event corresponds to a pipe *)
          let all_valid = Array.for_all (fun event ->
            Event.is_readable event &&
            Array.exists (fun (_, _, token) ->
              Token.equal ~eq:(=) (Event.token event) token
            ) pipes
          ) events in
          
          if all_valid then
            Format.printf "✓ All events are valid\n"
          else
            Format.printf "✗ Some events are invalid\n";
          
          (* Cleanup *)
          Array.iter (fun (r, w, _) ->
            Unix.close r;
            Unix.close w
          ) pipes;
          
          actual_count = expected_count && all_valid

let test_poll_timeout_accuracy () =
  Format.printf "\nTesting poll timeout accuracy:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      let timeouts = [0; 10; 50; 100] in
      let tolerance = 20.0 in (* 20ms tolerance *)
      
      let all_accurate = List.for_all (fun timeout_ms ->
        let start = Unix.gettimeofday () in
        let _ = Gluon.poll ~timeout:timeout_ms poll in
        let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
        
        let expected = float_of_int timeout_ms in
        let diff = abs_float (elapsed -. expected) in
        let accurate = diff <= tolerance in
        
        Format.printf "%s Timeout %dms: actual %.1fms (diff %.1fms)\n"
          (if accurate then "✓" else "✗")
          timeout_ms elapsed diff;
        
        accurate || timeout_ms = 0  (* 0 timeout is special *)
      ) timeouts in
      
      all_accurate

let test_error_conditions () =
  Format.printf "\nTesting error conditions:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      false
  | Ok poll ->
      let (r, w) = match Gluon.pipe () with
        | Ok p -> p
        | Error _ -> exit 1
      in
      let token = Token.make "test" in
      
      (* Test double registration *)
      let _ = Gluon.register poll ~fd:r ~token ~interests:Interest.readable in
      
      begin match Gluon.register poll ~fd:r ~token ~interests:Interest.readable with
      | Error _ ->
          Format.printf "✓ Double registration correctly failed\n";
          true
      | Ok () ->
          Format.printf "✗ Double registration should have failed\n";
          false
      end &&
      
      (* Test deregister non-existent fd *)
      let (r2, w2) = match Gluon.pipe () with
        | Ok p -> p
        | Error _ -> exit 1
      in
      begin match Gluon.deregister poll ~fd:r2 with
      | Error _ ->
          Format.printf "✓ Deregister non-existent fd correctly failed\n";
          Unix.close r2;
          Unix.close w2;
          true
      | Ok () ->
          Format.printf "✗ Deregister non-existent fd should have failed\n";
          Unix.close r2;
          Unix.close w2;
          false
      end &&
      
      (* Test reregister non-existent fd *)
      begin match Gluon.reregister poll ~fd:r2 ~token ~interests:Interest.readable with
      | Error _ ->
          Format.printf "✓ Reregister non-existent fd correctly failed\n";
          Unix.close r;
          Unix.close w;
          true
      | Ok () ->
          Format.printf "✗ Reregister non-existent fd should have failed\n";
          Unix.close r;
          Unix.close w;
          false
      end

let () =
  Format.printf "Gluon Stress Tests\n";
  Format.printf "==================\n\n";
  
  let tests = [
    ("many_fds", test_many_fds);
    ("reregister_stress", test_reregister_stress);
    ("concurrent_events", test_concurrent_events);
    ("poll_timeout_accuracy", test_poll_timeout_accuracy);
    ("error_conditions", test_error_conditions);
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