

type Message.t += Tick | Stop

let main ~args:_ =
  let my_pid = self () in

  println "Setting up interval timer (every 50ms)...";

  (* Set up an interval timer *)
  let timer_id = Timer.send_interval my_pid Tick ~interval:0.05 in

  (* Also set up a stop timer *)
  let _ = Timer.send_after my_pid Stop ~after:0.3 in

  (* Count how many ticks we get *)
  let rec loop count =
    match receive_any () with
    | Tick ->
        println ("  Tick " ^ string_of_int count);
        loop (count + 1)
    | Stop ->
        println ("  Stop received after " ^ string_of_int count ^ " ticks");
        (* Cancel the interval timer *)
        Timer.cancel timer_id;
        count
    | _ -> loop count
  in

  let tick_count = loop 0 in

  (* We should get roughly 6 ticks (300ms / 50ms) *)
  if tick_count >= 4 && tick_count <= 8 then
    println ("✓ Interval timer worked! Got " ^ string_of_int tick_count ^ " ticks (expected ~6)")
  else println ("✗ Unexpected tick count: " ^ string_of_int tick_count ^ " (expected ~6)");

  (* Make sure timer is really cancelled - try to receive with timeout *)
  (try
     let _ =
       receive
         ~selector:(function Tick -> `select () | _ -> `skip)
         ~timeout:0.1 ()
     in
     println "✗ Timer not cancelled - still receiving ticks!"
   with Receive_timeout ->
     println "✓ Interval timer successfully cancelled");

  Ok ()

let () = Miniriot.run ~main ~args:Env.args ()
