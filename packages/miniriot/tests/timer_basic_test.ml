

type Message.t += Ping | Timeout_test

let main ~args:_ =
  let my_pid = self () in

  (* Test 1: send_after works *)
  let _ = Timer.send_after my_pid Ping ~after:0.1 in

  let msg = receive_any () in
  (match msg with
  | Ping -> println "✓ Test 1 passed: Received Ping after delay"
  | _ -> println "✗ Test 1 failed: Expected Ping");

  (* Test 2: receive timeout works *)
  (try
     let _ =
       receive
         ~selector:(function Timeout_test -> `select () | _ -> `skip)
         ~timeout:0.05 ()
     in
     println "✗ Test 2 failed: Should have timed out"
   with Receive_timeout ->
     println "✓ Test 2 passed: Receive timed out as expected");

  (* Test 3: Timer cancellation *)
  let timer_id = Timer.send_after my_pid Timeout_test ~after:1.0 in
  Timer.cancel timer_id;

  (try
     let _ =
       receive
         ~selector:(function Timeout_test -> `select () | _ -> `skip)
         ~timeout:0.1 ()
     in
     println "✗ Test 3 failed: Should have timed out (timer was cancelled)"
   with Receive_timeout ->
     println "✓ Test 3 passed: Cancelled timer didn't fire");

  Ok ()

let () = Miniriot.run ~main ~args:Env.args ()
