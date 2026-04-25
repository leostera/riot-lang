open Std

let main ~args:_ =
  Log.info "Testing receive timeout...";
  let selector _msg = `skip in
  (
    try
      let _ = receive ~selector ~timeout:(Time.Duration.from_millis 500) () in Log.error "ERROR: receive should have timed out!"
    with
    | Receive_timeout -> Log.info "SUCCESS: Received Receive_timeout exception"
  );
  Log.info "Test complete";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
