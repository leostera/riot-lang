open Miniriot

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
      Printf.printf "  Worker: Processing task %d\n%!" task;
      Process.sleep 0.1;
      send coordinator (TaskComplete task);
      send coordinator WorkerReady;
      loop ()
    with Receive_timeout ->
      Printf.printf "  Worker: Timeout, sending ready signal\n%!";
      send coordinator WorkerReady;
      loop ()
  in
  loop ()

let main ~args:_ =
  Printf.printf "Testing worker pool with timeout...\n%!";

  let coord_pid = self () in
  let worker_pid = spawn (fun () -> worker coord_pid) in

  (* Wait for initial ready signal (should timeout and send ready) *)
  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:1.0 ()
  in
  Printf.printf "✓ Worker sent initial ready signal via timeout\n%!";

  (* Send a task *)
  send worker_pid (Task 42);

  (* Wait for task completion *)
  let result =
    receive
      ~selector:(function TaskComplete n -> `select n | _ -> `skip)
      ~timeout:1.0 ()
  in
  Printf.printf "✓ Task completed: %d\n%!" result;

  (* Wait for ready signal after task *)
  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:1.0 ()
  in
  Printf.printf "✓ Worker sent ready signal after task\n%!";

  (* Don't send any more tasks, wait for timeout-based ready signal *)
  let _ =
    receive
      ~selector:(function WorkerReady -> `select () | _ -> `skip)
      ~timeout:1.0 ()
  in
  Printf.printf "✓ Worker sent another ready signal via timeout (heartbeat)\n%!";

  Printf.printf "\n✓ All tests passed! Worker pool timeout mechanism works!\n%!";

  Ok ()

let () = Miniriot.run ~main ~args:Env.args ()
