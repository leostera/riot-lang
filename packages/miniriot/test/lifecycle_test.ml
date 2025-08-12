open Miniriot

let test_normal_exit () =
  let exited = ref false in

  let worker () =
    exited := true;
    Process.Normal
  in

  let main () =
    let _pid = spawn worker in
    yield ();
    yield ();

    (* Let worker run and exit *)
    if !exited then Process.Normal
    else Process.Exception (Failure "Worker didn't exit")
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_normal_exit\n"

let test_exception_exit () =
  let worker () = raise (Failure "Worker error") in

  let main () =
    let _pid = spawn worker in
    yield ();
    yield ();
    (* Worker should have crashed but main continues *)
    Process.Normal
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_exception_exit\n"

let test_main_process_exit () =
  let worker_ran = ref false in

  let worker () =
    for _ = 1 to 10 do
      worker_ran := true;
      yield ()
    done;
    Process.Normal
  in

  let main () =
    let _pid = spawn worker in
    (* Exit immediately - worker may or may not run *)
    Process.Normal
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_main_process_exit (worker_ran=%b)\n" !worker_ran

let test_process_state_transitions () =
  let states = ref [] in

  let worker () =
    states := "running" :: !states;
    match receive () with
    | Exit ->
        states := "received_exit" :: !states;
        Process.Normal
    | _ -> Process.Normal
  in

  let main () =
    states := "main_start" :: !states;
    let pid = spawn worker in
    states := "spawned" :: !states;

    yield ();
    (* Let worker start *)
    states := "after_yield" :: !states;

    send pid Exit;
    states := "sent_exit" :: !states;

    yield ();
    yield ();
    (* Let worker process and exit *)
    states := "final" :: !states;

    Process.Normal
  in

  let status = run ~main in
  assert (status = 0);

  (* Check we got expected transitions *)
  let has_state s = List.mem s !states in
  assert (has_state "main_start");
  assert (has_state "spawned");
  assert (has_state "running");
  assert (has_state "final");

  Printf.printf "✓ test_process_state_transitions\n"

let test_scheduler_termination () =
  let counter = ref 0 in

  let rec worker n () =
    if n > 0 then (
      incr counter;
      yield ();
      worker (n - 1) ())
    else Process.Normal
  in

  let main () =
    (* Spawn multiple workers *)
    for i = 1 to 5 do
      let _pid = spawn (worker i) in
      ()
    done;

    (* Let them all run for a bit *)
    for _ = 1 to 20 do
      yield ()
    done;

    Process.Normal
  in

  let status = run ~main in
  assert (status = 0);
  assert (!counter > 0);
  (* Some work was done *)
  Printf.printf "✓ test_scheduler_termination (counter=%d)\n" !counter

let () =
  Printf.printf "=== Lifecycle Tests ===\n";
  test_normal_exit ();
  test_exception_exit ();
  test_main_process_exit ();
  test_process_state_transitions ();
  test_scheduler_termination ();
  Printf.printf "All lifecycle tests passed!\n"
