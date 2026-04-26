open Std

type Message.t +=
  | Atomic_worker_done of int

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let test_atomic_make_and_get =
  Test.case
    "sync atomic make stores and returns the initial value"
    (fun _ctx ->
      let atomic = Sync.Atomic.make "value" in
      if String.equal (Sync.Atomic.get atomic) "value" then
        Ok ()
      else
        Error "expected Atomic.make to preserve the initial value")

let test_atomic_set_updates_value =
  Test.case
    "sync atomic set updates the stored value"
    (fun _ctx ->
      let atomic = Sync.Atomic.make 1 in
      Sync.Atomic.set atomic 7;
      if Sync.Atomic.get atomic = 7 then
        Ok ()
      else
        Error "expected set to replace the stored value")

let test_atomic_exchange_returns_previous_value =
  Test.case
    "sync atomic exchange returns the previous value and stores the new one"
    (fun _ctx ->
      let atomic = Sync.Atomic.make "left" in
      let previous = Sync.Atomic.exchange atomic "right" in
      if String.equal previous "left" && String.equal (Sync.Atomic.get atomic) "right" then
        Ok ()
      else
        Error "expected exchange to return the previous value and store the new one")

let test_atomic_compare_and_set_success_and_failure =
  Test.case
    "sync atomic compare_and_set only updates on a match"
    (fun _ctx ->
      let atomic = Sync.Atomic.make 3 in
      let first = Sync.Atomic.compare_and_set atomic 3 9 in
      let second = Sync.Atomic.compare_and_set atomic 3 11 in
      if first && not second && Sync.Atomic.get atomic = 9 then
        Ok ()
      else
        Error "expected compare_and_set to update only when the expected value matches")

let test_atomic_fetch_and_add_returns_previous_value =
  Test.case
    "sync atomic fetch_and_add returns the previous value and increments"
    (fun _ctx ->
      let atomic = Sync.Atomic.make 5 in
      let previous = Sync.Atomic.fetch_and_add atomic 4 in
      if previous = 5 && Sync.Atomic.get atomic = 9 then
        Ok ()
      else
        Error "expected fetch_and_add to return the old value and apply the increment")

let test_atomic_fetch_and_add_serializes_concurrent_updates =
  Test.case
    "sync atomic fetch_and_add preserves concurrent increments"
    (fun _ctx ->
      let counter = Sync.Atomic.make 0 in
      let parent = self () in
      let worker_count = 4 in
      let iterations = 100 in
      for worker = 1 to worker_count do
        ignore
          (
            spawn
              (fun () ->
                for _ = 1 to iterations do
                  let _ = Sync.Atomic.fetch_and_add counter 1 in
                  ()
                done;
                send parent (Atomic_worker_done worker);
                Ok ())
          )
      done;
      let rec await_workers remaining =
        if remaining = 0 then
          Ok ()
        else
          match await
            ~what:"atomic worker completion"
            (
              function
              | Atomic_worker_done _ -> `select ()
              | _ -> `skip
            ) with
          | Error _ as err -> err
          | Ok () -> await_workers (remaining - 1)
      in
      match await_workers worker_count with
      | Error _ as err -> err
      | Ok () ->
          let expected = worker_count * iterations in
          if Sync.Atomic.get counter = expected then
            Ok ()
          else
            Error "expected concurrent fetch_and_add calls to preserve every increment")

let name = "Sync.Atomic"

let tests = [
  test_atomic_make_and_get;
  test_atomic_set_updates_value;
  test_atomic_exchange_returns_previous_value;
  test_atomic_compare_and_set_success_and_failure;
  test_atomic_fetch_and_add_returns_previous_value;
  test_atomic_fetch_and_add_serializes_concurrent_updates;
]

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
