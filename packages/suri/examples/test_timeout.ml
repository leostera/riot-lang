open Std

let receive_selector = fun _msg -> Skip

let main ~args:_ =
  Log.info "Testing receive timeout...";
  (
    try
      let _ = receive ~selector:receive_selector ~timeout:(Time.Duration.from_millis 500) () in
      Log.error "ERROR: receive should have timed out!"
    with
    | Receive_timeout -> Log.info "SUCCESS: Received Receive_timeout exception"
  );
  Log.info "Test complete";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
