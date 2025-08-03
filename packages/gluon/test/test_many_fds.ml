(** Test many file descriptors *)

open Gluon

let () =
  Format.printf "Testing many file descriptors:\n";
  
  match Gluon.create () with
  | Error _ ->
      Format.printf "✗ Failed to create kqueue\n";
      exit 1
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
        
        (* Write to specific fds and verify events *)
        let test_indices = [0; 25; 50; 75; 99] in
        
        
        List.iter (fun idx ->
          match List.find_opt (fun (i, _, _) -> i = idx) !pipes with
          | None -> 
              Format.printf "✗ Pipe %d not found\n" idx;
              raise Exit
          | Some (_, _, write_fd) ->
              let _ = Unix.write write_fd (Bytes.of_string "x") 0 1 in
              ()
        ) test_indices;
        
        (* Give time for events to propagate *)
        Unix.sleepf 0.01;
        
        (* Poll and verify we get the right events *)
        match Gluon.poll ~timeout:100 ~max_events:1024 poll with
        | Error _ ->
            Format.printf "✗ Poll failed\n";
            exit 1
        | Ok events ->
            Format.printf "✓ Got %d events\n" (Array.length events);
            
            (* Verify we got events for the fds we wrote to *)
            let received_indices = Array.map (fun event ->
              Token.unsafe_to_value (Event.token event)
            ) events |> Array.to_list in
            
            
            let all_found = List.for_all (fun expected ->
              let found = List.mem expected received_indices in
              if not found then
                Format.printf "  ✗ Missing event for index %d\n" expected;
              found
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
            
            exit (if all_found then 0 else 1)
      with Exit ->
        (* Cleanup on error *)
        List.iter (fun (_, r, w) ->
          try Unix.close r with _ -> ();
          try Unix.close w with _ -> ()
        ) !pipes;
        exit 1