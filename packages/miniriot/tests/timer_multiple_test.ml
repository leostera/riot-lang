

type Message.t += Tick of int

let main ~args:_ =
  let my_pid = self () in

  println "Setting up 5 timers with different delays...";

  (* Set up 5 timers with different delays *)
  let _ = Timer.send_after my_pid (Tick 1) ~after:0.05 in
  let _ = Timer.send_after my_pid (Tick 2) ~after:0.10 in
  let _ = Timer.send_after my_pid (Tick 3) ~after:0.15 in
  let _ = Timer.send_after my_pid (Tick 4) ~after:0.20 in
  let _ = Timer.send_after my_pid (Tick 5) ~after:0.25 in

  (* Collect all 5 ticks *)
  let rec collect n acc =
    if n = 0 then List.rev acc
    else
      match receive_any () with
      | Tick i ->
          println ("  Received Tick " ^ string_of_int i);
          collect (n - 1) (i :: acc)
      | _ -> collect n acc
  in

  let ticks = collect 5 [] in

  if ticks = [ 1; 2; 3; 4; 5 ] then
    println "✓ All timers fired in order!"
  else
    println ("✗ Timers fired out of order: " ^
      String.concat ", " (List.map string_of_int ticks));

  Ok ()

let () = Miniriot.run ~main ~args:Env.args ()
