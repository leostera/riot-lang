open Std

type selected_message =
  | Skip

let select_message = fun _msg -> Skip

let receive_selector = fun msg ->
  match select_message msg with
  | Skip -> `skip

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
