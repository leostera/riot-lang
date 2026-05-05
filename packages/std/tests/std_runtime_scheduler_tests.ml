open Std

module HashSet = Collections.HashSet

type Message.t +=
  | Scheduler_worker_ready of {
      run_ref: unit Ref.t;
      pid: Pid.t;
      scheduler: int option;
    }
  | Scheduler_worker_ping of {
      run_ref: unit Ref.t;
    }
  | Scheduler_worker_ack of {
      run_ref: unit Ref.t;
      scheduler: int option;
    }
  | Scheduler_burst_ready of {
      run_ref: unit Ref.t;
      pid: Pid.t;
    }
  | Scheduler_burst_value of {
      run_ref: unit Ref.t;
      value: int;
    }
  | Scheduler_burst_done of {
      run_ref: unit Ref.t;
      values: int list;
    }
  | Scheduler_actor_done of {
      run_ref: unit Ref.t;
      value: int;
    }
  | Scheduler_failed of {
      run_ref: unit Ref.t;
      reason: string;
    }

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 2) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let scheduler_index = fun () ->
  match Runtime.current_scheduler_id () with
  | None -> None
  | Some scheduler -> Some (Runtime.Scheduler_id.to_int scheduler)

let sort_ints = fun values -> List.sort values ~compare:Int.compare

let range = fun first last ->
  let rec loop current acc =
    if current < first then
      acc
    else
      loop (current - 1) (current :: acc)
  in
  loop last []

let has_expected_values = fun ~expected ~actual ->
  let expected_set = HashSet.from_list expected in
  let actual_set = HashSet.from_list actual in
  Int.equal (List.length actual) (List.length expected)
  && Int.equal (HashSet.length actual_set) (HashSet.length expected_set)
  && sort_ints actual = sort_ints expected

let await_worker_event = fun run_ref ~what ->
  await
    ~what
    (fun __tmp1 ->
      match __tmp1 with
      | Scheduler_worker_ready { run_ref = received_ref; pid; scheduler } when Ref.equal
        run_ref
        received_ref -> Select (`ready (pid, scheduler))
      | Scheduler_worker_ack { run_ref = received_ref; scheduler } when Ref.equal
        run_ref
        received_ref -> Select (`ack scheduler)
      | Scheduler_failed { run_ref = received_ref; reason } when Ref.equal run_ref received_ref ->
          Select (`failed reason)
      | _ -> Skip)

let test_pinned_worker_wakes_after_parking = fun _ctx ->
  let run_ref = Ref.make () in
  let parent = self () in
  let child =
    Runtime.spawn_pinned
      ~scheduler:1
      (fun () ->
        send
          parent
          (Scheduler_worker_ready {
            run_ref;
            pid = self ();
            scheduler = scheduler_index ();
          });
        try
          let () =
            receive
              ~timeout:(Time.Duration.from_secs 1)
              ~selector:(fun __tmp1 ->
                match __tmp1 with
                | Scheduler_worker_ping { run_ref = received_ref } when Ref.equal run_ref received_ref ->
                    Select ()
                | _ -> Skip)
              ()
          in
          send
            parent
            (Scheduler_worker_ack {
              run_ref;
              scheduler = scheduler_index ();
            });
          Ok ()
        with
        | Receive_timeout ->
            send parent (Scheduler_failed { run_ref; reason = "child did not receive ping" });
            Ok ())
  in
  match await_worker_event run_ref ~what:"pinned worker readiness" with
  | Error _ as err -> err
  | Ok (`failed reason) -> Error reason
  | Ok (`ack _) -> Error "received worker ack before readiness"
  | Ok (`ready (ready_pid, ready_scheduler)) ->
      if not (Pid.equal ready_pid child) then
        Error "pinned worker reported the wrong pid"
      else if not (Option.equal ready_scheduler (Some 1) ~fn:Int.equal) then
        Error "pinned worker did not start on scheduler 1"
      else (
        sleep (Time.Duration.from_millis 20);
        ignore
          (
            Runtime.spawn_pinned
              ~scheduler:0
              (fun () ->
                send child (Scheduler_worker_ping { run_ref });
                Ok ())
          );
        match await_worker_event run_ref ~what:"pinned worker wakeup" with
        | Error _ as err -> err
        | Ok (`failed reason) -> Error reason
        | Ok (`ready _) -> Error "received duplicate readiness after ping"
        | Ok (`ack ack_scheduler) ->
            if ack_scheduler = Some 1 then
              Ok ()
            else
              Error "pinned worker resumed on the wrong scheduler"
      )

let test_parked_worker_receives_burst_once_per_message = fun _ctx ->
  let run_ref = Ref.make () in
  let parent = self () in
  let total = 64 in
  let child =
    Runtime.spawn_pinned
      ~scheduler:1
      (fun () ->
        send parent (Scheduler_burst_ready { run_ref; pid = self () });
        let rec collect remaining values =
          if remaining = 0 then (
            send parent (Scheduler_burst_done { run_ref; values });
            Ok ()
          ) else
            try
              let value =
                receive
                  ~timeout:(Time.Duration.from_secs 1)
                  ~selector:(fun __tmp1 ->
                    match __tmp1 with
                    | Scheduler_burst_value { run_ref = received_ref; value } when Ref.equal
                      run_ref
                      received_ref -> Select value
                    | _ -> Skip)
                  ()
              in
              collect (remaining - 1) (value :: values)
            with
            | Receive_timeout ->
                send parent (Scheduler_failed { run_ref; reason = "burst receiver timed out" });
                Ok ()
        in
        collect total [])
  in
  match await
    ~what:"burst receiver readiness"
    (fun __tmp1 ->
      match __tmp1 with
      | Scheduler_burst_ready { run_ref = received_ref; pid } when Ref.equal run_ref received_ref ->
          Select (`ready pid)
      | Scheduler_failed { run_ref = received_ref; reason } when Ref.equal run_ref received_ref ->
          Select (`failed reason)
      | _ -> Skip) with
  | Error _ as err -> err
  | Ok (`failed reason) -> Error reason
  | Ok (`ready ready_pid) when not (Pid.equal ready_pid child) ->
      Error "burst receiver reported the wrong pid"
  | Ok (`ready _) ->
      sleep (Time.Duration.from_millis 20);
      ignore
        (
          Runtime.spawn_pinned
            ~scheduler:0
            (fun () ->
              for value = 1 to total do
                send child (Scheduler_burst_value { run_ref; value })
              done;
              Ok ())
        );
      match await
        ~what:"burst receiver completion"
        (fun __tmp1 ->
          match __tmp1 with
          | Scheduler_burst_done { run_ref = received_ref; values } when Ref.equal
            run_ref
            received_ref -> Select (`done_ values)
          | Scheduler_failed { run_ref = received_ref; reason } when Ref.equal run_ref received_ref ->
              Select (`failed reason)
          | _ -> Skip) with
      | Error _ as err -> err
      | Ok (`failed reason) -> Error reason
      | Ok (`done_ values) ->
          let expected = range 1 total in
          if has_expected_values ~expected ~actual:values then
            Ok ()
          else
            Error "burst receiver did not observe each message exactly once"

let test_normal_actor_queue_pressure_completes_each_actor_once = fun _ctx ->
  let run_ref = Ref.make () in
  let parent = self () in
  let total = 128 in
  for value = 1 to total do
    ignore
      (
        Runtime.spawn
          (fun () ->
            send parent (Scheduler_actor_done { run_ref; value });
            Ok ())
      )
  done;
  let rec collect remaining values =
    if remaining = 0 then
      Ok values
    else
      match await
        ~what:"normal actor completion"
        (fun __tmp1 ->
          match __tmp1 with
          | Scheduler_actor_done { run_ref = received_ref; value } when Ref.equal
            run_ref
            received_ref -> Select value
          | _ -> Skip) with
      | Error _ as err -> err
      | Ok value -> collect (remaining - 1) (value :: values)
  in
  match collect total [] with
  | Error _ as err -> err
  | Ok values ->
      let expected = range 1 total in
      if has_expected_values ~expected ~actual:values then
        Ok ()
      else
        Error "normal actor pressure run did not complete each actor exactly once"

let tests =
  Test.[
    case
      "runtime scheduler wakes a parked pinned worker"
      test_pinned_worker_wakes_after_parking;
    case
      "runtime scheduler delivers a burst to a parked worker exactly once"
      test_parked_worker_receives_burst_once_per_message;
    case
      "runtime scheduler completes normal actor queue pressure exactly once"
      test_normal_actor_queue_pressure_completes_each_actor_once;
  ]

let main ~args = Test.Cli.main ~name:"Runtime.Scheduler" ~tests ~args ()

let () =
  Runtime.run
    ~main
    ~args:Env.args
    ~config:(Runtime.Config.make ~scheduler_count:4 ())
    ()
