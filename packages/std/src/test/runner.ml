open Global
open Collections

exception Test_timeout of Time.Duration.t

type Runtime.Message.t +=
  | Test_runner_start

let find_segment_index = fun segments needle ->
  let rec loop idx = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | segment :: rest ->
        if String.equal segment needle then
          Some idx
        else
          loop (idx + 1) rest
  in
  loop 0 segments

let take = fun count xs ->
  let rec loop remaining acc = fun __tmp1 ->
    match __tmp1 with
    | _ when remaining <= 0 -> List.reverse acc
    | [] -> List.reverse acc
    | x :: rest -> loop (remaining - 1) (x :: acc) rest
  in
  loop count [] xs

let join_path_segments = fun segments ->
  match segments with
  | [] -> "."
  | "" :: rest -> "/" ^ String.concat "/" rest
  | _ -> String.concat "/" segments

let derive_package_name = fun binary_path ->
  match binary_path with
  | None -> None
  | Some path ->
      let segments =
        Path.components path
        |> List.map ~fn:Path.to_string
      in
      match find_segment_index segments "out" with
      | Some idx when List.length segments > idx + 1 -> List.get segments ~at:(idx + 1)
      | _ -> None

let derive_workspace_root = fun ~current_dir ~binary_path ->
  match binary_path with
  | None -> current_dir
  | Some path -> (
      let segments =
        Path.components path
        |> List.map ~fn:Path.to_string
      in
      match find_segment_index segments "_build" with
      | Some 0 -> current_dir
      | Some idx -> (
          match take idx segments with
          | []
          | [ "." ] -> current_dir
          | prefix -> (
              match Path.from_string (join_path_segments prefix) with
              | Ok root -> Some root
              | Error _ -> current_dir
            )
        )
      | None -> current_dir
    )

type mode =
  | Sequential
  | Shuffle

type size_filter =
  | All_sizes
  | Only_small
  | Only_large

type target = {
  query: string option;
  size_filter: size_filter;
  flaky_only: bool;
}

type test_descriptor = {
  index: int;
  name: string;
  test_type: Test_case.test_type;
  size: Test_case.size;
  reliability: Test_case.reliability;
}

type event =
  | SuiteStarted of { suite_name: string; total: int }
  | TestStarted of test_descriptor
  | TestProgress of {
      test: test_descriptor;
      attempt: int;
      progress: Test_context.progress;
    }
  | TestAttemptStarted of {
      test: test_descriptor;
      attempt: int;
      timeout: Time.Duration.t option;
    }
  | TestHeartbeat of {
      test: test_descriptor;
      attempt: int;
      elapsed: Time.Duration.t;
    }
  | TestAttemptFinished of {
      test: test_descriptor;
      attempt: int;
      result: Test_result.single_result;
      duration: Time.Duration.t;
    }
  | TestFinished of Test_result.t

type scheduled_test = {
  index: int;
  test: Test_case.t;
}

type event_handler = event -> unit

type policy = {
  small_test_timeout: Time.Duration.t option;
  flaky_max_retries: int;
}

type config = {
  concurrency: int;
  reporter: (module Reporter.Intf);
  mode: mode;
  target: target;
  policy: policy;
  suite_info: Reporter.suite_info;
  context_store: Test_context.Store.t;
  event_handler: event_handler;
}

type run_summary = Test_result.summary

let default_policy = {
  small_test_timeout = Some (Time.Duration.from_millis 500);
  flaky_max_retries = 0;
}

let no_event_handler: event_handler = fun _ -> ()

let heartbeat_interval = Time.Duration.from_secs 1

type Message.t +=
  | Test_runner_worker_event of {
      run_ref: unit Ref.t;
      event: event;
    }
  | Test_runner_worker_failed of {
      run_ref: unit Ref.t;
      exn: exn;
    }

let make_ctx = fun ~(suite_info:Reporter.suite_info) ~context_store ~index (test: Test_case.t) ->
  let current_dir =
    Env.current_dir ()
    |> Result.to_option
  in
  Test_context.{
    suite_name = suite_info.name;
    context_store;
    test_name = test.name;
    test_index = index;
    source_file = suite_info.source_file;
    binary_path = suite_info.binary_path;
    built_binaries = suite_info.built_binaries;
    workspace_root = Option.or_
      suite_info.workspace_root
      (derive_workspace_root ~current_dir ~binary_path:suite_info.binary_path);
    package_name = Option.or_ suite_info.package_name (derive_package_name suite_info.binary_path);
    fixture = None;
    progress_handler = Test_context.no_progress_handler;
  }

let filter_tests = fun target tests ->
  let matches_query (test: Test_case.t) =
    match target.query with
    | None -> true
    | Some query -> String.contains test.name query
  in
  let matches_size (test: Test_case.t) =
    match (target.size_filter, test.size) with
    | (All_sizes, _)
    | (Only_small, Test_case.Small)
    | (Only_large, Test_case.Large) -> true
    | _ -> false
  in
  let matches_flaky (test: Test_case.t) =
    not target.flaky_only || match test.reliability with
    | Test_case.Stable -> false
    | Test_case.Flaky _ -> true
  in
  List.filter tests ~fn:(fun test -> matches_query test && matches_size test && matches_flaky test)

let shuffle_list = fun lst ->
  let arr = Array.from_list lst in
  let len = Array.length arr in
  let shuffle_index i =
    let modulus = i + 1 in
    let candidate = Int.rem ((i * 48_271) + 1) modulus in
    if candidate < 0 then
      candidate + modulus
    else
      candidate
  in
  for i = len - 1 downto 1 do
    let j = shuffle_index i in
    let temp = Array.get_unchecked arr ~at:i in
    Array.set_unchecked
      arr
      ~at:i
      ~value:(Array.get_unchecked arr ~at:j);
    Array.set_unchecked arr ~at:j ~value:temp
  done;
  Array.fold_right arr ~init:[] ~fn:(fun item acc -> item :: acc)

let render_exception_failure = fun exn ->
  let exn = Exception.to_string exn in
  let bt = Exception.raw_backtrace_to_string (Exception.get_raw_backtrace ()) in
  exn ^ "\n\n" ^ bt

let test_timeout_for = fun policy (test: Test_case.t) ->
  match (test.size, policy.small_test_timeout) with
  | (Test_case.Small, Some timeout) -> Some timeout
  | _ -> None

let retry_budget = fun policy (test: Test_case.t) ->
  match test.reliability with
  | Test_case.Stable -> 0
  | Test_case.Flaky { retry_attempts } ->
      if policy.flaky_max_retries > 0 then
        Int.min retry_attempts policy.flaky_max_retries
      else
        retry_attempts

let test_descriptor_of_case = fun index (test: Test_case.t) ->
  {
    index;
    name = test.name;
    test_type = test.test_type;
    size = test.size;
    reliability = test.reliability;
  }

let wait_for_exit = fun pid ?timeout () ->
  receive
    ~selector:(fun (msg: Runtime.Message.t) ->
      match msg with
      | Runtime.Actor.DOWN { pid = down_pid; reason; _ } when Pid.equal down_pid pid ->
          Select reason
      | _ -> Skip)
    ?timeout
    ()

let wait_for_start = fun () ->
  receive
    ~selector:(fun (msg: Runtime.Message.t) ->
      match msg with
      | Test_runner_start -> Select ()
      | _ -> Skip)
    ()

let cast_worker:
  type task other. (task, other) Type.eq ->
  other Worker_pool.DynamicWorkerPool.worker ->
  task Worker_pool.DynamicWorkerPool.worker = fun witness worker ->
  match witness with
  | Type.Equal -> worker

let run_single_attempt = fun ~ctx ~on_event ~test_info (test: Test_case.t) ~attempt ~timeout ->
  on_event (TestAttemptStarted { test = test_info; attempt; timeout });
  let ctx =
    Test_context.with_progress_handler
      ctx
      (fun progress -> on_event (TestProgress { test = test_info; attempt; progress }))
  in
  let outcome: ((unit, string) result option) Sync.Atomic.t = Sync.Atomic.make None in
  let child =
    spawn
      (fun () ->
        wait_for_start ();
        let result =
          match test.fn ctx with
          | Ok () -> Ok ()
          | Error msg -> Error msg
          | exception exn -> Error (render_exception_failure exn)
        in
        Sync.Atomic.set outcome (Some result);
        Ok ())
  in
  let monitor_ref = Runtime.Actor.monitor child in
  let started = Time.Instant.now () in
  send child Test_runner_start;
  let rec wait_loop () =
    let elapsed = Time.Instant.elapsed started in
    let wait_timeout =
      match timeout with
      | None -> heartbeat_interval
      | Some timeout ->
          let remaining = Time.Duration.sub timeout elapsed in
          if Time.Duration.is_zero remaining then
            Time.Duration.zero
          else
            Time.Duration.min remaining heartbeat_interval
    in
    if Time.Duration.is_zero wait_timeout then (
      match timeout with
      | Some timeout ->
          Runtime.Actor.kill child ~reason:(Test_timeout timeout);
          wait_for_exit child ()
      | None -> wait_for_exit child ()
    ) else
      try wait_for_exit child ~timeout:wait_timeout () with
      | Receive_timeout ->
          on_event
            (TestHeartbeat { test = test_info; attempt; elapsed = Time.Instant.elapsed started });
          wait_loop ()
  in
  let exit_reason = wait_loop () in
  Runtime.Actor.demonitor monitor_ref;
  let duration = Time.Instant.elapsed started in
  let result =
    match Sync.Atomic.get outcome with
    | Some (Ok ()) -> Test_result.Passed
    | Some (Error msg) -> Failed msg
    | None -> (
        match exit_reason with
        | Error (Test_timeout timeout) -> Timed_out { timeout }
        | Ok () -> Failed "test actor exited without reporting a result"
        | Error exn -> Failed (render_exception_failure exn)
      )
  in
  on_event
    (
      TestAttemptFinished {
        test = test_info;
        attempt;
        result;
        duration;
      }
    );
  (result, duration)

let should_retry = fun policy (test: Test_case.t) attempts (result: Test_result.single_result) ->
  attempts <= retry_budget policy test && match result with
  | Test_result.Passed
  | Test_result.Skipped -> false
  | Test_result.Failed _
  | Test_result.Timed_out _ -> true

let run_single_test = fun ~suite_info ~context_store ~policy ~on_event index (test: Test_case.t) ->
  let name = test.name in
  let test_type = test.test_type in
  let ctx = make_ctx ~suite_info ~context_store ~index test in
  let test_info = test_descriptor_of_case index test in
  on_event (TestStarted test_info);
  let result =
    if test.skip then
      Test_result.{
        index;
        name;
        test_type;
        size = test.size;
        reliability = test.reliability;
        attempts = 0;
        result = Skipped;
        duration = Time.Duration.zero;
      }
    else
      (
        let timeout = test_timeout_for policy test in
        let rec loop attempts total_duration =
          let (attempt_result, attempt_duration) =
            run_single_attempt ~ctx ~on_event ~test_info test ~attempt:attempts ~timeout
          in
          let total_duration = Time.Duration.add total_duration attempt_duration in
          if should_retry policy test attempts attempt_result then
            loop (attempts + 1) total_duration
          else
            Test_result.{
              index;
              name;
              test_type;
              size = test.size;
              reliability = test.reliability;
              attempts;
              result = attempt_result;
              duration = total_duration;
            }
        in
        loop 1 Time.Duration.zero
      )
  in
  on_event (TestFinished result);
  result

type parallel_run_state = {
  owner: Pid.t;
  pool: scheduled_test Worker_pool.DynamicWorkerPool.t;
  pending: scheduled_test Queue.t;
  completed: (int, Test_result.t) HashMap.t;
  mutable finished_count: int;
  mutable next_report_index: int;
  mutable results_rev: Test_result.t list;
  total: int;
  reporter: (module Reporter.Intf);
  on_event: event_handler;
  run_ref: unit Ref.t;
}

type parallel_worker_event =
  | Parallel_worker_ready of scheduled_test Worker_pool.DynamicWorkerPool.worker
  | Parallel_worker_event of event
  | Parallel_worker_failed of exn

let flush_reporter_results = fun (state: parallel_run_state) ->
  let module R = (val state.reporter : Reporter.Intf) in
  let rec loop () =
    match HashMap.get state.completed ~key:state.next_report_index with
    | None -> ()
    | Some result ->
        let _ = HashMap.remove state.completed ~key:state.next_report_index in
        R.on_result state.next_report_index result;
        state.next_report_index <- state.next_report_index + 1;
        loop ()
  in
  loop ()

let run_tests_parallel = fun ~(config:config) tests_to_run ->
  let module R = (val config.reporter : Reporter.Intf) in
  let owner = self () in
  let run_ref = Ref.make () in
  let pending = Queue.create () in
  let completed = HashMap.create () in
  let tests_with_indices =
    List.enumerate tests_to_run
    |> List.map ~fn:(fun (idx, test) -> { index = idx + 1; test })
  in
  List.for_each tests_with_indices ~fn:(fun scheduled -> Queue.push pending ~value:scheduled);
  let worker_fn ~owner ~task =
    let { index; test }: scheduled_test = task in
    let on_event event = send owner (Test_runner_worker_event { run_ref; event }) in
    try
      let _ =
        run_single_test
          ~suite_info:config.suite_info
          ~context_store:config.context_store
          ~policy:config.policy
          ~on_event
          index
          test
      in
      ()
    with
    | exn -> send owner (Test_runner_worker_failed { run_ref; exn })
  in
  let pool =
    Worker_pool.DynamicWorkerPool.start
      ~concurrency:(Int.max 1 config.concurrency)
      ~owner
      ~worker_fn
      ()
  in
  let state = {
    owner;
    pool;
    pending;
    completed;
    finished_count = 0;
    next_report_index = 1;
    results_rev = [];
    total = List.length tests_to_run;
    reporter = config.reporter;
    on_event = config.event_handler;
    run_ref;
  }
  in
  let rec loop () =
    if Int.equal state.finished_count state.total then
      ()
    else
      let selector: parallel_worker_event selector = fun __tmp1 ->
        match __tmp1 with
        | Worker_pool.DynamicWorkerPool.WorkerReady worker -> (
            match Ref.type_equal
              state.pool.task_ref
              (Worker_pool.DynamicWorkerPool.get_worker_task_ref worker) with
            | Some witness -> Select (Parallel_worker_ready (cast_worker witness worker))
            | None -> Skip
          )
        | Test_runner_worker_event { run_ref; event } when Ref.equal state.run_ref run_ref ->
            Select (Parallel_worker_event event)
        | Test_runner_worker_failed { run_ref; exn } when Ref.equal state.run_ref run_ref ->
            Select (Parallel_worker_failed exn)
        | _ -> Skip
      in
      match receive ~selector () with
      | Parallel_worker_ready worker -> (
          match Queue.pop state.pending with
          | Some task ->
              Worker_pool.DynamicWorkerPool.send_task state.pool worker task;
              loop ()
          | None -> loop ()
        )
      | Parallel_worker_event event ->
          state.on_event event;
          (
            match event with
            | TestFinished result ->
                state.finished_count <- state.finished_count + 1;
                state.results_rev <- result :: state.results_rev;
                let _ = HashMap.insert state.completed ~key:result.index ~value:result in
                flush_reporter_results state
            | _ -> ()
          );
          loop ()
      | Parallel_worker_failed exn -> raise exn
  in
  loop ();
  let results =
    List.sort
      state.results_rev
      ~compare:(fun (left: Test_result.t) (right: Test_result.t) ->
        Int.compare
          left.index
          right.index)
  in
  let summary = Test_result.make_summary results in
  R.finalize summary;
  summary

let run_tests = fun ~config tests ->
  Exception.record_backtrace true;
  let filtered_tests = filter_tests config.target tests in
  let tests_to_run =
    match config.mode with
    | Sequential -> filtered_tests
    | Shuffle -> shuffle_list filtered_tests
  in
  let module R = (val config.reporter : Reporter.Intf) in
  R.init config.suite_info (List.length tests_to_run);
  config.event_handler
    (SuiteStarted { suite_name = config.suite_info.name; total = List.length tests_to_run });
  if config.concurrency <= 1 || List.length tests_to_run <= 1 then (
    let rec run_all index = fun __tmp1 ->
      match __tmp1 with
      | [] -> []
      | test :: rest ->
          let result =
            run_single_test
              ~suite_info:config.suite_info
              ~context_store:config.context_store
              ~policy:config.policy
              ~on_event:config.event_handler
              index
              test
          in
          R.on_result index result;
          result :: run_all (index + 1) rest
    in
    let results = run_all 1 tests_to_run in
    let summary = Test_result.make_summary results in
    R.finalize summary;
    summary
  ) else
    run_tests_parallel ~config tests_to_run
