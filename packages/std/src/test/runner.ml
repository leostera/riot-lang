open Global
open Collections

exception Test_timeout of Time.Duration.t

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
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: rest -> loop (remaining - 1) (x :: acc) rest
  in
  loop count [] xs

let join_path_segments = fun segments ->
  match segments with
  | [] -> "."
  | "" :: rest -> "/" ^ String.concat "/" rest
  | _ -> String.concat "/" segments

let derive_package_name = fun binary_path ->
  match Env.var Env.String ~name:"RIOT_PACKAGE_NAME" with
  | Some package_name -> Some package_name
  | None -> (
      match binary_path with
      | None -> None
      | Some path ->
          let segments = Path.components path |> List.map Path.to_string in
          match find_segment_index segments "out" with
          | Some idx when List.length segments > idx + 1 -> Some (List.nth segments (idx + 1))
          | _ -> None
    )

let derive_workspace_root = fun ~current_dir ~binary_path ->
  match Env.var Env.String ~name:"RIOT_WORKSPACE_ROOT" with
  | Some root -> (
      match Path.of_string root with
      | Ok root -> Some root
      | Error _ -> current_dir
    )
  | None -> (
      match binary_path with
      | None -> current_dir
      | Some path -> (
          let segments = Path.components path |> List.map Path.to_string in
          match find_segment_index segments "_build" with
          | Some 0 ->
              current_dir
          | Some idx -> (
              match take idx segments with
              | []
              | [ "." ] -> current_dir
              | prefix -> (
                  match Path.of_string (join_path_segments prefix) with
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
  | Only_long

type target = {
  query: string option;
  size_filter: size_filter;
  flaky_only: bool;
}

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
}

type run_summary = Test_result.summary

let default_policy = {
  small_test_timeout = None;
  flaky_max_retries = 0;
}

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
  }

let filter_tests = fun target tests ->
  let matches_query = fun (test: Test_case.t) ->
    match target.query with
    | None -> true
    | Some query -> String.contains test.name query
  in
  let matches_size = fun (test: Test_case.t) ->
    match (target.size_filter, test.size) with
    | All_sizes, _
    | Only_small, Test_case.Small
    | Only_long, Test_case.Long -> true
    | _ -> false
  in
  let matches_flaky = fun (test: Test_case.t) ->
    not target.flaky_only
    ||
    match test.reliability with
    | Test_case.Stable -> false
    | Test_case.Flaky _ -> true
  in
  List.filter
    (fun test -> matches_query test && matches_size test && matches_flaky test)
    tests

let shuffle_list = fun lst ->
  let arr = Array.of_list lst in
  let len = Array.length arr in
  for i = len - 1 downto 1 do
    let j = Kernel.Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let render_exception_failure = fun exn ->
  let exn = Exception.to_string exn in
  let bt = Exception.get_backtrace () in
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

let wait_for_exit = fun pid ?timeout () ->
  receive
    ~selector:
      (fun (msg: Actors.Message.t) ->
        match msg with
        | Actors.Process.DOWN { pid = down_pid; reason; _ } when Pid.equal down_pid pid -> `select reason
        | _ -> `skip)
    ?timeout
    ()

let run_single_attempt = fun ~ctx (test: Test_case.t) ~timeout ->
  let outcome: ((unit, string) result option) Sync.Atomic.t = Sync.Atomic.make None in
  let child =
    spawn
      (fun () ->
        let result =
          match test.fn ctx with
          | Ok () -> Ok ()
          | Error msg -> Error msg
          | exception exn -> Error (render_exception_failure exn)
        in
        Sync.Atomic.set outcome (Some result);
        Ok ())
  in
  let monitor_ref = Actors.Process.monitor child in
  let started = Time.Instant.now () in
  let exit_reason =
    match timeout with
    | None -> wait_for_exit child ()
    | Some timeout -> (
        try wait_for_exit child ~timeout ()
        with
        | Receive_timeout ->
            Actors.Process.kill child ~reason:(Test_timeout timeout);
            wait_for_exit child ()
      )
  in
  Actors.Process.demonitor monitor_ref;
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
  (result, duration)

let should_retry = fun policy (test: Test_case.t) attempts (result: Test_result.single_result) ->
  attempts <= retry_budget policy test
  &&
  match result with
  | Test_result.Passed
  | Test_result.Skipped -> false
  | Test_result.Failed _
  | Test_result.Timed_out _ -> true

let run_single_test = fun reporter ~suite_info ~policy index (test: Test_case.t) ->
  let name = test.name in
  let test_type = test.test_type in
  let ctx = make_ctx ~suite_info ~index test in
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
    else (
      let timeout = test_timeout_for policy test in
      let rec loop attempts total_duration =
        let attempt_result, attempt_duration = run_single_attempt ~ctx test ~timeout in
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
  let results =
    List.mapi
      (fun i test ->
        run_single_test config.reporter ~suite_info:config.suite_info ~policy:config.policy (i + 1) test)
      tests_to_run
  in
  let summary = Test_result.make_summary results in
  R.finalize summary;
  summary
