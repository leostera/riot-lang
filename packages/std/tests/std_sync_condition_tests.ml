open Std

type Message.t +=
  | Condition_waiter_ready
  | Condition_waiter_resumed of bool
  | Condition_broadcast_waiter_ready of int
  | Condition_broadcast_waiter_resumed of int
  | Condition_signaler_done
  | Condition_wait_failed of string
  | Condition_wait_returned

type 'a box = {
  mutable value: 'a;
}

let box = fun value -> { value }

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let test_condition_wait_releases_mutex_and_reacquires_after_signal =
  Test.case "sync condition wait releases the mutex and reacquires after signal"
    (fun _ctx ->
      let mutex = Sync.Mutex.create () in
      let condition = Sync.Condition.create () in
      let parent = self () in
      let signaler_entered = Sync.Atomic.make false in
      ignore
        (
          spawn
            (fun () ->
              Sync.Mutex.lock mutex;
              send parent Condition_waiter_ready;
              Sync.Condition.wait condition mutex;
              let entered = Sync.Atomic.get signaler_entered in
              Sync.Mutex.unlock mutex;
              send parent (Condition_waiter_resumed entered);
              Ok ())
        );
      match
        await ~what:"condition waiter readiness"
          (
            function
            | Condition_waiter_ready -> `select ()
            | _ -> `skip
          )
      with
      | Error _ as err -> err
      | Ok () ->
          sleep (Time.Duration.from_millis 20);
          ignore
            (
              spawn
                (fun () ->
                  Sync.Mutex.lock mutex;
                  Sync.Atomic.set signaler_entered true;
                  Sync.Condition.signal condition;
                  Sync.Mutex.unlock mutex;
                  send parent Condition_signaler_done;
                  Ok ())
            );
          let saw_signaler = box false in
          let waiter_entered = box None in
          let rec collect remaining =
            if remaining = 0 then
              Ok ()
            else
              match
                await ~what:"condition signal flow"
                  (
                    function
                    | Condition_signaler_done -> `select `Signaler
                    | Condition_waiter_resumed entered -> `select (`Waiter entered)
                    | _ -> `skip
                  )
              with
              | Error _ as err ->
                  err
              | Ok `Signaler ->
                  saw_signaler.value <- true;
                  collect (remaining - 1)
              | Ok (`Waiter entered) ->
                  waiter_entered.value <- Some entered;
                  collect (remaining - 1)
          in
          match collect 2 with
          | Error _ as err -> err
          | Ok () -> (
              match waiter_entered.value with
              | Some true when saw_signaler.value -> Ok ()
              | Some true -> Error "condition waiter resumed before the signaler reported completion"
              | Some false -> Error "condition waiter resumed without another actor acquiring the mutex"
              | None -> Error "condition waiter never resumed"
            ))

let test_condition_wait_requires_mutex_ownership =
  Test.case "sync condition wait requires owning the mutex"
    (fun _ctx ->
      let mutex = Sync.Mutex.create () in
      let condition = Sync.Condition.create () in
      let parent = self () in
      ignore
        (
          spawn
            (fun () ->
              let outcome =
                try
                  Sync.Condition.wait condition mutex;
                  `Returned
                with
                | Failure reason -> `Failed reason
              in
              (
                match outcome with
                | `Returned -> send parent Condition_wait_returned
                | `Failed reason -> send parent (Condition_wait_failed reason)
              );
              Ok ())
        );
      match
        await ~what:"condition wait failure"
          (
            function
            | Condition_wait_failed reason -> `select (`Failed reason)
            | Condition_wait_returned -> `select `Returned
            | _ -> `skip
          )
      with
      | Error _ as err -> err
      | Ok (`Failed reason) when String.contains reason "mutex wait" -> Ok ()
      | Ok (`Failed reason) -> Error ("unexpected condition wait failure: " ^ reason)
      | Ok `Returned -> Error "expected condition wait without mutex ownership to fail")

let test_condition_broadcast_wakes_all_waiters =
  Test.case "sync condition broadcast wakes all waiting actors"
    (fun _ctx ->
      let mutex = Sync.Mutex.create () in
      let condition = Sync.Condition.create () in
      let parent = self () in
      let waiter_count = 3 in
      for idx = 1 to waiter_count do
        ignore
          (
            spawn
              (fun () ->
                Sync.Mutex.lock mutex;
                send parent (Condition_broadcast_waiter_ready idx);
                Sync.Condition.wait condition mutex;
                Sync.Mutex.unlock mutex;
                send parent (Condition_broadcast_waiter_resumed idx);
                Ok ())
          )
      done;
      let rec await_ready remaining =
        if remaining = 0 then
          Ok ()
        else
          match
            await ~what:"broadcast waiter readiness"
              (
                function
                | Condition_broadcast_waiter_ready _idx -> `select ()
                | _ -> `skip
              )
          with
          | Error _ as err -> err
          | Ok () -> await_ready (remaining - 1)
      in
      match await_ready waiter_count with
      | Error _ as err -> err
      | Ok () ->
          sleep (Time.Duration.from_millis 20);
          ignore
            (
              spawn
                (fun () ->
                  Sync.Mutex.lock mutex;
                  Sync.Condition.broadcast condition;
                  Sync.Mutex.unlock mutex;
                  send parent Condition_signaler_done;
                  Ok ())
            );
          let resumed = Sync.Atomic.make 0 in
          let rec collect remaining =
            if remaining = 0 then
              Ok ()
            else
              match
                await ~what:"broadcast wakeup"
                  (
                    function
                    | Condition_signaler_done -> `select `Signaled
                    | Condition_broadcast_waiter_resumed _idx -> `select `Resumed
                    | _ -> `skip
                  )
              with
              | Error _ as err ->
                  err
              | Ok `Signaled ->
                  collect (remaining - 1)
              | Ok `Resumed ->
                  let _ = Sync.Atomic.fetch_and_add resumed 1 in
                  collect (remaining - 1)
          in
          match collect (waiter_count + 1) with
          | Error _ as err -> err
          | Ok () ->
              if Sync.Atomic.get resumed = waiter_count then
                Ok ()
              else
                Error "expected broadcast to wake every waiting actor")

let name = "Sync.Condition"

let tests = [
  test_condition_wait_releases_mutex_and_reacquires_after_signal;
  test_condition_wait_requires_mutex_ownership;
  test_condition_broadcast_wakes_all_waiters;
]

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
