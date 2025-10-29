open Miniriot

type Message.t += Tick | Stop

let main ~args:_ =
  let my_pid = self () in

  Printf.printf "Setting up interval timer (every 50ms)...\n%!";

  (* Set up an interval timer *)
  let timer_id = Timer.send_interval my_pid Tick ~interval:0.05 in

  (* Also set up a stop timer *)
  let _ = Timer.send_after my_pid Stop ~after:0.3 in

  (* Count how many ticks we get *)
  let rec loop count =
    match receive_any () with
    | Tick ->
        Printf.printf "  Tick %d\n%!" count;
        loop (count + 1)
    | Stop ->
        Printf.printf "  Stop received after %d ticks\n%!" count;
        (* Cancel the interval timer *)
        Timer.cancel timer_id;
        count
    | _ -> loop count
  in

  let tick_count = loop 0 in

  (* We should get roughly 6 ticks (300ms / 50ms) *)
  if tick_count >= 4 && tick_count <= 8 then
    Printf.printf "✓ Interval timer worked! Got %d ticks (expected ~6)\n%!"
      tick_count
  else Printf.printf "✗ Unexpected tick count: %d (expected ~6)\n%!" tick_count;

  (* Make sure timer is really cancelled - try to receive with timeout *)
  (try
     let _ =
       receive
         ~selector:(function Tick -> `select () | _ -> `skip)
         ~timeout:0.1 ()
     in
     Printf.printf "✗ Timer not cancelled - still receiving ticks!\n%!"
   with Receive_timeout ->
     Printf.printf "✓ Interval timer successfully cancelled\n%!");

  Ok ()

let () = Miniriot.run ~main ~args:Env.args ()
