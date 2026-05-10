open Std

type Message.t +=
  | Task_started
  | Task_unrelated of string
  | Slow_task_registered
  | Register_slow of Pid.t
  | Release_slow
  | Slow_task_released

let await_message = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let is_failure_message = fun __tmp1 ->
  match __tmp1 with
  | Failure message -> String.equal message "boom"
  | _ -> false

let test_async_await_returns_ok = fun _ctx ->
  match Task.await (Task.async (fun () -> 42)) with
  | Ok 42 -> Ok ()
  | Ok value -> Error ("expected Task.await to return 42, got " ^ Int.to_string value)
  | Error _ -> Error "expected Task.await to succeed"

let test_async_await_returns_error_for_exception = fun _ctx ->
  match Task.await (Task.async (fun () -> raise (Failure "boom"))) with
  | Error exn when is_failure_message exn -> Ok ()
  | Error _ -> Error "expected Task.await to return Failure boom"
  | Ok _ -> Error "expected Task.await to return Error for a crashing task"

let test_await_all_singleton = fun _ctx ->
  match Task.await_all
    [
      Task.async (fun () -> "ok");
    ] with
  | [ Ok "ok" ] -> Ok ()
  | _ -> Error "expected Task.await_all on a singleton list to return one Ok result"

let test_await_all_empty_returns_immediately = fun _ctx ->
  match Task.await_all [] with
  | [] -> Ok ()
  | _ -> Error "expected Task.await_all [] to return []"

let test_await_all_multiple_successes = fun _ctx ->
  match Task.await_all
    [
      Task.async (fun () -> 1);
      Task.async (fun () -> 2);
      Task.async (fun () -> 3);
    ] with
  | [ Ok 1; Ok 2; Ok 3 ] -> Ok ()
  | _ -> Error "expected Task.await_all to preserve all successful results"

let test_await_all_preserves_mixed_success_and_failure = fun _ctx ->
  match Task.await_all
    [
      Task.async (fun () -> 1);
      Task.async (fun () -> raise (Failure "boom"));
      Task.async (fun () -> 3);
    ] with
  | [ Ok 1; Error exn; Ok 3 ] when is_failure_message exn -> Ok ()
  | _ -> Error "expected Task.await_all to preserve both Ok and Error results"

let test_await_all_preserves_input_order = fun _ctx ->
  let parent = self () in
  let gate =
    spawn
      (fun () ->
        let slow_pid =
          receive
            ~selector:(fun __tmp1 ->
              match __tmp1 with
              | Register_slow slow_pid -> Select slow_pid
              | _ -> Skip)
            ()
        in
        send parent Slow_task_registered;
        receive
          ~selector:(fun __tmp1 ->
            match __tmp1 with
            | Release_slow -> Select ()
            | _ -> Skip)
          ();
        send slow_pid Slow_task_released;
        Ok ())
  in
  let slow =
    Task.async
      (fun () ->
        send gate (Register_slow (self ()));
        receive
          ~selector:(fun __tmp1 ->
            match __tmp1 with
            | Slow_task_released -> Select ()
            | _ -> Skip)
          ();
        "slow")
  in
  match await_message
    ~what:"slow task registration"
    (fun __tmp1 ->
      match __tmp1 with
      | Slow_task_registered -> Select ()
      | _ -> Skip) with
  | Error _ as err -> err
  | Ok () ->
      let fast =
        Task.async
          (fun () ->
            send gate Release_slow;
            "fast")
      in
      match Task.await_all [ slow; fast ] with
      | [ Ok "slow"; Ok "fast" ] -> Ok ()
      | _ -> Error "expected Task.await_all to return results in input order"

let test_await_all_ignores_unrelated_messages = fun _ctx ->
  send (self ()) (Task_unrelated "noise");
  let results =
    Task.await_all
      [
        Task.async (fun () -> 7);
        Task.async (fun () -> 9);
      ]
  in
  match results with
  | [ Ok 7; Ok 9 ] ->
      (match await_message
        ~what:"unrelated message"
        (fun __tmp1 ->
          match __tmp1 with
          | Task_unrelated payload -> Select payload
        | _ -> Skip) with
      | Ok "noise" -> Ok ()
      | Ok payload -> Error ("expected unrelated payload noise, got " ^ payload)
      | Error _ as err -> err)
  | _ -> Error "expected Task.await_all to ignore unrelated mailbox messages"

let test_async_starts_eagerly = fun _ctx ->
  let parent = self () in
  let task =
    Task.async
      (fun () ->
        send parent Task_started;
        sleep (Time.Duration.from_millis 25);
        42)
  in
  match await_message
    ~what:"task start"
    (fun __tmp1 ->
      match __tmp1 with
      | Task_started -> Select ()
      | _ -> Skip) with
  | Error _ as err -> err
  | Ok () ->
      match Task.await task with
      | Ok 42 -> Ok ()
      | Ok value -> Error ("expected task result 42, got " ^ Int.to_string value)
      | Error _ -> Error "expected eager task to succeed"

let tests =
  Test.[
    case "Task.await returns Ok for a pure task" test_async_await_returns_ok;
    case "Task.await returns Error for a crashing task" test_async_await_returns_error_for_exception;
    case "Task.await_all on a singleton list returns one result" test_await_all_singleton;
    case
      "Task.await_all on an empty list returns immediately"
      test_await_all_empty_returns_immediately;
    case "Task.await_all preserves multiple successful results" test_await_all_multiple_successes;
    case
      "Task.await_all preserves mixed success and failure results"
      test_await_all_preserves_mixed_success_and_failure;
    case "Task.await_all preserves input order" test_await_all_preserves_input_order;
    case
      "Task.await_all ignores unrelated mailbox messages"
      test_await_all_ignores_unrelated_messages;
    case "Task.async starts eagerly before await" test_async_starts_eagerly;
  ]

let main ~args = Test.Cli.main ~name:"Task" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
