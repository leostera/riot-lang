open Std

type Message.t += Tick of int

let test_timers ~args:_ =
  let start = Time.Instant.now () in
  
  let rec loop count =
    if count > 10 then Ok ()
    else (
      let _timer = Timer.send_after (self ()) (Tick count) 
        ~after:(Time.Duration.from_secs_float 0.1) in
      
      match receive_any () with
      | Tick n ->
          let elapsed = Time.Duration.to_secs_float 
            (Time.Instant.elapsed start) in
          Log.info "Timer #%d fired at %.3fs (expected: %.3fs)" 
            n elapsed (Float.of_int n *. 0.1);
          loop (count + 1)
      | _ -> loop count
    )
  in
  loop 1

let () = Miniriot.run ~main:test_timers ~args:[] ()
