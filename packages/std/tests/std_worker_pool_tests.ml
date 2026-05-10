open Std

type Message.t +=
  | Worker_pool_task_received of {
      payload: string;
      run_ref: string Ref.t;
    }

let is_failure = fun exn ~message ->
  match exn with
  | Failure reason -> String.equal reason message
  | _ -> false

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let cast_worker:
  type task other. (task, other) Type.eq ->
  other WorkerPool.DynamicWorkerPool.worker ->
  task WorkerPool.DynamicWorkerPool.worker = fun Type.Equal worker -> worker

let await_ready_worker:
  type task. task WorkerPool.DynamicWorkerPool.t ->
  (task WorkerPool.DynamicWorkerPool.worker, string) result = fun pool ->
  await
    ~what:"worker readiness"
    (fun __tmp1 ->
      match __tmp1 with
      | WorkerPool.DynamicWorkerPool.WorkerReady worker ->
          match Ref.type_equal
            pool.WorkerPool.DynamicWorkerPool.task_ref
            (WorkerPool.DynamicWorkerPool.get_worker_task_ref worker) with
          | Some witness -> Select (cast_worker witness worker)
          | None -> Skip
      | _ -> Skip)

let test_simple_worker_pool_empty_returns_immediately =
  Test.case
    "simple worker pool run on an empty task list returns []"
    (fun _ctx ->
      match WorkerPool.SimpleWorkerPool.run ~tasks:[] ~fn:(fun x -> x) () with
      | [] -> Ok ()
      | _ -> Error "expected SimpleWorkerPool.run [] to return []")

let test_simple_worker_pool_singleton_returns_indexed_result =
  Test.case
    "simple worker pool run on a singleton task returns one indexed result"
    (fun _ctx ->
      match WorkerPool.SimpleWorkerPool.run ~tasks:[ "alpha" ] ~fn:String.length () with
      | [ (0, 5) ] -> Ok ()
      | _ -> Error "expected a singleton worker pool run to return [(0, 5)]")

let test_simple_worker_pool_preserves_input_order =
  Test.case
    "simple worker pool preserves input order instead of completion order"
    (fun _ctx ->
      let results =
        WorkerPool.SimpleWorkerPool.run
          ~concurrency:2
          ~tasks:[ "slow"; "fast"; "medium" ]
          ~fn:(fun task ->
            if String.equal task "slow" then
              sleep (Time.Duration.from_millis 25)
            else if String.equal task "medium" then
              sleep (Time.Duration.from_millis 5);
            task)
          ()
      in
      match results with
      | [ (0, "slow"); (1, "fast"); (2, "medium") ] -> Ok ()
      | _ -> Error "expected SimpleWorkerPool.run to return results in input order")

let test_simple_worker_pool_concurrency_one_is_sequential =
  Test.case
    "simple worker pool concurrency 1 behaves like a sequential indexed map"
    (fun _ctx ->
      match WorkerPool.SimpleWorkerPool.run
        ~concurrency:1
        ~tasks:[ 1; 2; 3 ]
        ~fn:(fun value -> value * 10)
        () with
      | [ (0, 10); (1, 20); (2, 30) ] -> Ok ()
      | _ -> Error "expected concurrency 1 to preserve sequential indexed results")

let test_simple_worker_pool_handles_concurrency_greater_than_task_count =
  Test.case
    "simple worker pool with excess concurrency returns every result once"
    (fun _ctx ->
      let results =
        WorkerPool.SimpleWorkerPool.run
          ~concurrency:8
          ~tasks:[ 3; 1 ]
          ~fn:(fun value -> value + 1)
          ()
      in
      match results with
      | [ (0, 4); (1, 2) ] -> Ok ()
      | _ -> Error "expected excess concurrency to return one indexed result per task")

let test_simple_worker_pool_propagates_worker_exceptions =
  Test.case
    "simple worker pool re-raises worker exceptions"
    (fun _ctx ->
      try
        let _ =
          WorkerPool.SimpleWorkerPool.run
            ~concurrency:2
            ~tasks:[ 1; 2; 3 ]
            ~fn:(fun value ->
              if value = 2 then
                raise (Failure "boom");
              value)
            ()
        in
        Error "expected SimpleWorkerPool.run to re-raise worker failures"
      with
      | exn when is_failure exn ~message:"boom" -> Ok ()
      | Failure reason -> Error ("unexpected failure: " ^ reason))

let test_dynamic_worker_pool_sends_worker_ready_messages =
  Test.case
    "dynamic worker pool sends WorkerReady messages to the owner"
    (fun _ctx ->
      let pool =
        WorkerPool.DynamicWorkerPool.start
          ~concurrency:2
          ~owner:(self ())
          ~worker_fn:(fun ~owner:_ ~task:_ -> ())
          ()
      in
      let rec collect ready_count =
        if ready_count = 2 then
          Ok ()
        else
          match await_ready_worker pool with
          | Ok _worker -> collect (ready_count + 1)
          | Error _ as err -> err
      in
      collect 0)

let test_dynamic_worker_pool_send_task_executes_payload =
  Test.case
    "dynamic worker pool send_task delivers the supplied payload to the worker"
    (fun _ctx ->
      let run_ref = Ref.make () in
      let parent = self () in
      let pool =
        WorkerPool.DynamicWorkerPool.start
          ~concurrency:1
          ~owner:parent
          ~worker_fn:(fun ~owner ~task ->
            send
              owner
              (Worker_pool_task_received { payload = task; run_ref }))
          ()
      in
      match await_ready_worker pool with
      | Error _ as err -> err
      | Ok worker ->
          WorkerPool.DynamicWorkerPool.send_task pool worker "payload";
          let received =
            await
              ~what:"dynamic worker payload"
              (fun __tmp1 ->
                match __tmp1 with
                | Worker_pool_task_received { payload; run_ref = received_ref } ->
                    match Ref.type_equal run_ref received_ref with
                    | Some Type.Equal -> Select payload
                    | None -> Skip
                | _ -> Skip)
          in
          match received with
          | Ok "payload" ->
              match await_ready_worker pool with
              | Ok _ -> Ok ()
              | Error _ as err -> err
          | Ok payload -> Error ("expected dynamic worker to receive payload, got " ^ payload)
          | Error _ as err -> err)

let name = "WorkerPool"

let tests = [
  test_simple_worker_pool_empty_returns_immediately;
  test_simple_worker_pool_singleton_returns_indexed_result;
  test_simple_worker_pool_preserves_input_order;
  test_simple_worker_pool_concurrency_one_is_sequential;
  test_simple_worker_pool_handles_concurrency_greater_than_task_count;
  test_simple_worker_pool_propagates_worker_exceptions;
  test_dynamic_worker_pool_sends_worker_ready_messages;
  test_dynamic_worker_pool_send_task_executes_payload;
]

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
