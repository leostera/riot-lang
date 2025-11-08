

type Message.t += Task of int | WorkerReady | TaskComplete of int

(** Simple worker that processes tasks with timeout *)
let worker coordinator =
  let rec loop () =
    try
      let task =
        receive
          ~selector:(function Task n -> `select n | _ -> `skip)
          ~timeout:0.5 ()
      in
      println ("  Worker: Processing task " ^ string_of_int task);
      Process.sleep 0.1;
      send coordinator (TaskComplete task);
      send coordinator WorkerReady;
      loop ()
    with Receive_timeout ->
      println "  Worker: Timeout, sending ready signal";
      send coordinator WorkerReady;
      loop ()
  in
  loop ()

let main ~args:_ =
  println "Testing worker pool with timeout...";

  let coord_pid = self () in
  let worker_pid = spawn (fun () -> worker coord_pid) in

  (* Wait for initial ready signal (should timeout and send ready) *)
  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:1.0 ()
  in
  println "✓ Worker sent initial ready signal via timeout";

  (* Send a task *)
  send worker_pid (Task 42);

  (* Wait for task completion *)
  let result =
    receive
      ~selector:(function TaskComplete n -> `select n | _ -> `skip)
      ~timeout:1.0 ()
  in
  println ("✓ Task completed: " ^ string_of_int result);

  (* Wait for ready signal after task *)
  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:1.0 ()
  in
  println "✓ Worker sent ready signal after task";

  (* Don't send any more tasks, wait for timeout-based ready signal *)
  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:1.0 ()
  in
  println "✓ Worker sent another ready signal via timeout (heartbeat)";

  println "\n✓ All tests passed! Worker pool timeout mechanism works!";

  Ok ()

let () = Miniriot.run ~main ~args:Env.args ()
