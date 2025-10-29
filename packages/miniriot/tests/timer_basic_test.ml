open Miniriot

type Message.t += Ping | Timeout_test

let main ~args:_ =
  let my_pid = self () in

  (* Test 1: send_after works *)
  let _ = Timer.send_after my_pid Ping ~after:0.1 in

  let msg = receive_any () in
  (match msg with
  | Ping -> Printf.printf "✓ Test 1 passed: Received Ping after delay\n%!"
  | _ -> Printf.printf "✗ Test 1 failed: Expected Ping\n%!");

  (* Test 2: receive timeout works *)
  (try
     let _ =
       receive
         ~selector:(function Timeout_test -> `select () | _ -> `skip)
         ~timeout:0.05 ()
     in
     Printf.printf "✗ Test 2 failed: Should have timed out\n%!"
   with Receive_timeout ->
     Printf.printf "✓ Test 2 passed: Receive timed out as expected\n%!");

  (* Test 3: Timer cancellation *)
  let timer_id = Timer.send_after my_pid Timeout_test ~after:1.0 in
  Timer.cancel timer_id;

  (try
     let _ =
       receive
         ~selector:(function Timeout_test -> `select () | _ -> `skip)
         ~timeout:0.1 ()
     in
     Printf.printf
       "✗ Test 3 failed: Should have timed out (timer was cancelled)\n%!"
   with Receive_timeout ->
     Printf.printf "✓ Test 3 passed: Cancelled timer didn't fire\n%!");

  Ok ()

let () = Miniriot.run ~main ~args:Env.args ()
