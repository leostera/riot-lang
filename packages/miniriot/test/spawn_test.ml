open Miniriot

(* Test for spawn functionality *)

let test_spawn_single () =
  let spawned = ref false in
  let worker () =
    spawned := true;
    Process.Normal
  in

  let main () =
    let _pid = spawn worker in
    yield ();
    (* Let worker run *)
    if !spawned then Process.Normal
    else Process.Exception (Failure "Worker didn't run")
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_spawn_single\n"

let test_spawn_multiple () =
  let count = ref 0 in
  let worker id () =
    incr count;
    Printf.printf "  Worker %d running (count=%d)\n" id !count;
    Process.Normal
  in

  let main () =
    for i = 1 to 5 do
      let _pid = spawn (worker i) in
      ()
    done;

    (* Let all workers run *)
    for _ = 1 to 10 do
      yield ()
    done;

    if !count = 5 then Process.Normal
    else
      Process.Exception
        (Failure (Printf.sprintf "Expected 5 workers, got %d" !count))
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_spawn_multiple\n"

let test_self_pid () =
  let main_pid = ref None in
  let worker_pid = ref None in

  let worker () =
    worker_pid := Some (self ());
    Process.Normal
  in

  let main () =
    main_pid := Some (self ());
    let spawned_pid = spawn worker in
    yield ();

    match !worker_pid with
    | None -> Process.Exception (Failure "Worker didn't set its pid")
    | Some wpid ->
        if
          Pid.equal spawned_pid wpid
          && not (Pid.equal wpid (Option.get !main_pid))
        then Process.Normal
        else Process.Exception (Failure "PID mismatch")
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_self_pid\n"

let () =
  Printf.printf "=== Spawn Tests ===\n";
  test_spawn_single ();
  test_spawn_multiple ();
  test_self_pid ();
  Printf.printf "All spawn tests passed!\n"
