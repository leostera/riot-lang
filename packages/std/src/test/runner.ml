open Global
open Collections

exception Test_timeout of Time.Duration.t

type Runtime.Message.t +=
  | Test_runner_start

let find_segment_index = fun segments needle ->
  let rec loop idx = function
    | [] -> None
    | segment :: rest ->
        if String.equal segment needle then
          Some idx
        else
          loop (idx + 1) rest
  in
  loop 0 segments

let take = fun count xs ->
  let rec loop remaining acc = function
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
  match Env.get Env.String ~var:"RIOT_PACKAGE_NAME" with
  | Some package_name -> Some package_name
  | None -> (
      match binary_path with
      | None -> None
      | Some path ->
          let segments = Path.components path |> List.map ~fn:Path.to_string in
          match find_segment_index segments "out" with
          | Some idx when List.length segments > idx + 1 -> List.get segments ~at:(idx + 1)
          | _ -> None
    )

let derive_workspace_root = fun ~current_dir ~binary_path ->
  match Env.get Env.String ~var:"RIOT_WORKSPACE_ROOT" with
  | Some root -> (
      match Path.from_string root with
      | Ok root -> Some root
      | Error _ -> current_dir
    )
  | None -> (
      match binary_path with
      | None -> current_dir
      | Some path -> (
          let segments = Path.components path |> List.map ~fn:Path.to_string in
          match find_segment_index segments "_build" with
          | Some 0 ->
              current_dir
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
          | None ->
              current_dir
        )
    )

type mode =
  Sequential
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
  | TestProgress of { test: test_descriptor; attempt: int; progress: Test_context.progress }
  | TestAttemptStarted of { test: test_descriptor; attempt: int; timeout: Time.Duration.t option }
  | TestHeartbeat of { test: test_descriptor; attempt: int; elapsed: Time.Duration.t }
  | TestAttemptFinished of {
      test: test_descriptor;
      attempt: int;
      result: Test_result.single_result;
      duration: Time.Duration.t
    }
  | TestFinished of Test_result.t

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
  event_handler: event_handler;
}

type run_summary = Test_result.summary

let default_policy = { small_test_timeout = None; flaky_max_retries = 0 }

let no_event_handler: event_handler = fun _ -> ()

let heartbeat_interval = Time.Duration.from_secs 1

let make_ctx = fun ~(suite_info:Reporter.suite_info) ~index (test: Test_case.t) ->
  let current_dir = Env.current_dir () |> Result.to_option in
  Test_context.{
    suite_name = suite_info.name;
    test_name = test.name;
    test_index = index;
    source_file = suite_info.source_file;
    binary_path = suite_info.binary_path;
    workspace_root = derive_workspace_root ~current_dir ~binary_path:suite_info.binary_path;
    package_name = derive_package_name suite_info.binary_path;
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
    not target.flaky_only
    || match test.reliability with
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
    Array.set_unchecked arr ~at:i ~value:(Array.get_unchecked arr ~at:j);
    Array.set_unchecked arr ~at:j ~value:temp
  done;
  Array.fold_right arr ~acc:[] ~fn:(fun item acc -> item :: acc)

let render_exception_failure = fun exn ->
  let exn = Exception.to_string exn in
  let bt = Exception.raw_backtrace_to_string (Exception.get_raw_backtrace ()) in
  exn ^ "\n\n" ^ bt

let test_timeout_for = fun policy (test: Test_case.t) ->
  match (test.size, policy.small_test_timeout) with
  | Test_case.Small, Some timeout -> Some timeout
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
      | Runtime.Actor.DOWN { pid=down_pid; reason; _ } when Pid.equal down_pid pid -> `select reason
      | _ -> `skip)
    ?timeout
    ()

let wait_for_start = fun () ->
  receive
    ~selector:(fun (msg: Runtime.Message.t) ->
      match msg with
      | Test_runner_start -> `select ()
      | _ -> `skip)
    ()

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
    if Time.Duration.is_zero wait_timeout then
      (
        match timeout with
        | Some timeout ->
            Runtime.Actor.kill child ~reason:(Test_timeout timeout);
            wait_for_exit child ()
        | None -> wait_for_exit child ()
      )
    else
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
    | Some (Ok ()) ->
        Test_result.Passed
    | Some (Error msg) ->
        Failed msg
    | None -> (
        match exit_reason with
        | Error (Test_timeout timeout) -> Timed_out { timeout }
        | Ok () -> Failed "test actor exited without reporting a result"
        | Error exn -> Failed (render_exception_failure exn)
      )
  in
  on_event (TestAttemptFinished { test = test_info; attempt; result; duration });
  (result, duration)

let should_retry = fun policy (test: Test_case.t) attempts (result: Test_result.single_result) ->
  attempts <= retry_budget policy test && match result with
  | Test_result.Passed
  | Test_result.Skipped -> false
  | Test_result.Failed _
  | Test_result.Timed_out _ -> true

let run_single_test = fun reporter ~suite_info ~policy ~on_event index (test: Test_case.t) ->
  let name = test.name in
  let test_type = test.test_type in
  let ctx = make_ctx ~suite_info ~index test in
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
          let attempt_result, attempt_duration = run_single_attempt
            ~ctx
            ~on_event
            ~test_info
            test
            ~attempt:attempts
            ~timeout in
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
  let module R = (val reporter : Reporter.Intf) in
  R.on_result index result;
  on_event (TestFinished result);
  result

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
  let rec run_all index = function
    | [] -> []
    | test :: rest -> run_single_test
      config.reporter
      ~suite_info:config.suite_info
      ~policy:config.policy
      ~on_event:config.event_handler
      index
      test
    :: run_all (index + 1) rest
  in
  let results = run_all 1 tests_to_run in
  let summary = Test_result.make_summary results in
  R.finalize summary;
  summary
