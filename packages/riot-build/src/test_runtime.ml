open Std

type suite_binary = {
  package_name: string;
  suite_name: string;
}

type test_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  suite_filter: string option;
  profile: string;
  extra_args: string list;
}

type test_case_type =
  | Test
  | Property of { examples: int }

type test_case_size =
  | Small
  | Large

type test_case_reliability =
  | Stable
  | Flaky of { retry_attempts: int }

type test_case_status =
  | Passed
  | Failed of string
  | Timed_out of { timeout_ms: int }
  | Skipped

type test_case_result = {
  index: int;
  name: string;
  test_type: test_case_type;
  size: test_case_size;
  reliability: test_case_reliability;
  attempts: int;
  result: test_case_status;
  duration_us: int;
}

type listed_test_case = {
  index: int;
  name: string;
  test_type: test_case_type;
  size: test_case_size;
  reliability: test_case_reliability;
  skip: bool;
}

type listed_test_suite = {
  suite: suite_binary;
  source_path: Path.t option;
  tests: listed_test_case list;
}

type failed_test = {
  suite: suite_binary;
  name: string;
  message: string;
  duration_us: int;
}

type test_suite_summary = {
  total: int;
  passed: int;
  failed: int;
  skipped: int;
  duration_us: int;
  results: test_case_result list;
}

type test_event =
  | Build of Build_runtime.build_event
  | NoSuitesFound of { package_name: string option; suite_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of {
      suite: suite_binary;
      status: int;
      stdout: string;
      stderr: string;
      started_at_us: int option;
      completed_at_us: int option;
      duration_us: int option;
      summary: test_suite_summary
    }
  | Summary of { total: int; passed: int; failed: int; skipped: int; failed_tests: failed_test list }

type test_error =
  | BuildFailed of Build_runtime.build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

type Message.t +=
  | ListedTestsReady of (suite_binary * (listed_test_suite, test_error) result)

let no_event: test_event -> unit = fun _ -> ()

let no_listed_suite: listed_test_suite -> unit = fun _ -> ()

let no_list_error: suite_binary -> test_error -> unit = fun _ _ -> ()

let is_test_binary_name = fun name ->
  String.ends_with ~suffix:"_tests" name || String.ends_with ~suffix:"-tests" name

let compare_suite_binary = fun left right ->
  String.compare
    (left.package_name ^ ":" ^ left.suite_name)
    (right.package_name ^ ":" ^ right.suite_name)

let requested_packages = fun suites ->
  suites |> List.map (fun (suite: suite_binary) -> suite.package_name) |> List.sort_uniq String.compare

let collect_suite_binaries = fun (workspace: Riot_model.Workspace.t) ?package_filter ?suite_filter () ->
  workspace.packages |> List.filter Riot_model.Package.is_workspace_member |> List.filter
    (fun (pkg: Riot_model.Package.t) ->
      match package_filter with
      | None -> true
      | Some package_name -> String.equal pkg.name package_name) |> List.concat_map
    (fun (pkg: Riot_model.Package.t) ->
      List.filter_map
        (fun (bin: Riot_model.Package.binary) ->
          if is_test_binary_name bin.name && (
              match suite_filter with
              | None -> true
              | Some suite_name -> String.equal bin.name suite_name
            ) then
            Some { package_name = pkg.name; suite_name = bin.name }
          else
            None)
        pkg.binaries) |> List.sort compare_suite_binary

let find_suite_source_path = fun ~(workspace:Riot_model.Workspace.t) (suite: suite_binary) ->
  workspace.packages |> List.find_map
    (fun (pkg: Riot_model.Package.t) ->
      if String.equal pkg.name suite.package_name then
        pkg.binaries |> List.find_map
          (fun (bin: Riot_model.Package.binary) ->
            if String.equal bin.name suite.suite_name then
              Some Path.(pkg.path / bin.path)
            else
              None)
      else
        None)

let test_error_message = function
  | BuildFailed err -> Build_runtime.error_message err
  | ClientError err -> Client.error_message err
  | SuiteArtifactNotFound { reason; _ } -> reason
  | SuiteExecutionError { reason; _ } -> reason
  | SuitesFailed count -> Int.to_string count ^ " test suite(s) failed"

let rec json_type_name = function
  | Data.Json.Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Embed json -> json_type_name json

let error_expected = fun expected actual ->
  Error ("expected " ^ expected ^ " but got " ^ json_type_name actual)

let get_object = function
  | Data.Json.Object fields -> Ok fields
  | other -> error_expected "object" other

let get_array = function
  | Data.Json.Array values -> Ok values
  | other -> error_expected "array" other

let get_string = function
  | Data.Json.String value -> Ok value
  | other -> error_expected "string" other

let get_int = function
  | Data.Json.Int value -> Ok value
  | other -> error_expected "int" other

let get_bool = function
  | Data.Json.Bool value -> Ok value
  | other -> error_expected "bool" other

let field = fun name fields ->
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error _ as err -> err

let optional_int_field = fun name fields ->
  match List.assoc_opt name fields with
  | Some value -> get_int value |> Result.map Option.some
  | None -> Ok None

let split_json_stdout = fun stdout ->
  let lines = String.split_on_char '\n' stdout in
  let indexed =
    List.mapi (fun idx line -> (idx, line)) lines
  in
  match indexed
  |> List.rev
  |> List.find_opt (fun (_, line) -> not (String.equal (String.trim line) "")) with
  | None -> Error "missing JSON output"
  | Some (json_idx, json_line) ->
      let prefix =
        indexed
        |> List.filter_map
          (fun (idx, line) ->
            if idx < json_idx then
              Some line
            else
              None)
        |> String.concat "\n"
      in
      Ok (prefix, json_line)

let remove_json_args = fun args ->
  let rec loop acc = function
    | [] -> List.rev acc
    | "--json" :: rest -> loop acc rest
    | "--format" :: _value :: rest -> loop acc rest
    | arg :: rest when String.starts_with ~prefix:"--format=" arg -> loop acc rest
    | arg :: rest -> loop (arg :: acc) rest
  in
  loop [] args

let remove_list_args = fun args ->
  let rec loop acc = function
    | [] -> List.rev acc
    | "--json" :: rest -> loop acc rest
    | "--format" :: _value :: rest -> loop acc rest
    | arg :: rest when String.starts_with ~prefix:"--format=" arg -> loop acc rest
    | "--shuffle" :: rest -> loop acc rest
    | "--concurrency" :: _value :: rest -> loop acc rest
    | "--small-timeout-ms" :: _value :: rest -> loop acc rest
    | "--flaky-max-retries" :: _value :: rest -> loop acc rest
    | arg :: rest -> loop (arg :: acc) rest
  in
  loop [] args

let test_type_of_json = fun json ->
  let* fields = get_object json in
  let* type_json = field "type" fields in
  let* kind = get_string type_json in
  match kind with
  | "test" ->
      Ok Test
  | "property" ->
      let* examples_json = field "examples" fields in
      let* examples = get_int examples_json in
      Ok (Property { examples })
  | other ->
      Error ("unknown test type " ^ other)

let test_size_of_json = fun fields ->
  match List.assoc_opt "size" fields with
  | None -> Ok Small
  | Some value ->
      let* size = get_string value in
      match size with
      | "small" -> Ok Small
      | "large" -> Ok Large
      | other -> Error ("unknown test size " ^ other)

let test_reliability_of_json = fun fields ->
  match List.assoc_opt "reliability" fields with
  | None -> Ok Stable
  | Some value ->
      let* reliability = get_string value in
      match reliability with
      | "stable" ->
          Ok Stable
      | "flaky" ->
          let retry_attempts =
            match List.assoc_opt "retry_attempts" fields with
            | Some value -> get_int value
            | None -> Ok 0
          in
          retry_attempts |> Result.map (fun retry_attempts -> Flaky { retry_attempts })
      | other ->
          Error ("unknown test reliability " ^ other)

let test_status_of_json = fun json ->
  let* fields = get_object json in
  let* status_json = field "status" fields in
  let* status = get_string status_json in
  match status with
  | "passed" ->
      Ok Passed
  | "skipped" ->
      Ok Skipped
  | "failed" ->
      let* message_json = field "message" fields in
      let* message = get_string message_json in
      Ok (Failed message)
  | "timed_out" ->
      let* timeout_json = field "timeout_ms" fields in
      let* timeout_ms = get_int timeout_json in
      Ok (Timed_out { timeout_ms })
  | other ->
      Error ("unknown test status " ^ other)

let test_result_of_json = fun index json ->
  let* fields = get_object json in
  let* name_json = field "name" fields in
  let* name = get_string name_json in
  let* test_type = test_type_of_json json in
  let* size = test_size_of_json fields in
  let* reliability = test_reliability_of_json fields in
  let* attempts = optional_int_field "attempts" fields in
  let* result = test_status_of_json json in
  let* duration_us = optional_int_field "duration_us" fields in
  Ok {
    index;
    name;
    test_type;
    size;
    reliability;
    attempts = Option.unwrap_or ~default:1 attempts;
    result;
    duration_us = Option.unwrap_or ~default:0 duration_us;
  }

let listed_test_case_of_json = fun json ->
  let* fields = get_object json in
  let* index_json = field "index" fields in
  let* name_json = field "name" fields in
  let* index = get_int index_json in
  let* name = get_string name_json in
  let* test_type = test_type_of_json json in
  let* size = test_size_of_json fields in
  let* reliability = test_reliability_of_json fields in
  let skip =
    match List.assoc_opt "skip" fields with
    | Some value -> get_bool value
    | None -> Ok false
  in
  let* skip = skip in
  Ok {
    index;
    name;
    test_type;
    size;
    reliability;
    skip;
  }

let test_summary_of_json = fun json ->
  let* fields = get_object json in
  let* total_json = field "total" fields in
  let* passed_json = field "passed" fields in
  let* failed_json = field "failed" fields in
  let* skipped_json = field "skipped" fields in
  let* total = get_int total_json in
  let* passed = get_int passed_json in
  let* failed = get_int failed_json in
  let* skipped = get_int skipped_json in
  let* duration_us = optional_int_field "duration_us" fields in
  Ok (total, passed, failed, skipped, Option.unwrap_or ~default:0 duration_us)

let parse_test_suite_output = fun stdout ->
  let* (prefix_stdout, json_line) = split_json_stdout stdout in
  let* json = Data.Json.of_string json_line |> Result.map_error Data.Json.error_to_string in
  let* fields = get_object json in
  let* tests_json = field "tests" fields in
  let* summary_json = field "summary" fields in
  let* tests = get_array tests_json in
  let rec parse_results index acc = function
    | [] -> Ok (List.rev acc)
    | test_json :: rest ->
        let* result = test_result_of_json index test_json in
        parse_results (index + 1) (result :: acc) rest
  in
  let* results = parse_results 1 [] tests in
  let* (total, passed, failed, skipped, summary_duration_us) = test_summary_of_json summary_json in
  let* started_at_us = optional_int_field "started_at_us" fields in
  let* completed_at_us = optional_int_field "completed_at_us" fields in
  let* duration_us = optional_int_field "duration_us" fields in
  Ok (
    prefix_stdout,
    started_at_us,
    completed_at_us,
    duration_us,
    {
      total;
      passed;
      failed;
      skipped;
      duration_us = summary_duration_us;
      results;
    }
  )

let parse_listed_tests_output = fun stdout ->
  let* (_prefix_stdout, json_line) = split_json_stdout stdout in
  let* json = Data.Json.of_string json_line |> Result.map_error Data.Json.error_to_string in
  let* fields = get_object json in
  let* tests_json = field "tests" fields in
  let* tests = get_array tests_json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | json :: rest ->
        let* listed = listed_test_case_of_json json in
        loop (listed :: acc) rest
  in
  loop [] tests

let empty_suite_summary = {
  total = 0;
  passed = 0;
  failed = 0;
  skipped = 0;
  duration_us = 0;
  results = [];
}

let is_blank = fun s ->
  String.equal (String.trim s) ""

let summarize_output = fun value ->
  let trimmed = String.trim value in
  if String.equal trimmed "" then
    "<empty>"
  else
    let limit = 200 in
    if String.length trimmed <= limit then
      trimmed
    else
      String.sub trimmed 0 limit ^ "..."

let parse_failure_reason = fun ~suite ~(output:Command.output) reason ->
  String.concat ""
    [
      "failed to parse test results from suite '";
      suite.suite_name;
      "': ";
      reason;
      " (status=";
      Int.to_string output.status;
      ", stdout=";
      summarize_output output.stdout;
      ", stderr=";
      summarize_output output.stderr;
      ")"
    ]

let test_event_to_json = function
  | Build event ->
      Event.to_json event
  | NoSuitesFound { package_name; suite_name } ->
      Some (
        Data.Json.Object [ ("type", Data.Json.String "NoSuitesFound"); (
            "package_name",
            match package_name with
            | Some name -> Data.Json.String name
            | None -> Data.Json.Null
          ); (
            "suite_name",
            match suite_name with
            | Some name -> Data.Json.String name
            | None -> Data.Json.Null
          ); ]
      )
  | RunningSuite { package_name; suite_name } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "RunningSuite");
        ("package", Data.Json.String package_name);
        ("suite", Data.Json.String suite_name);
      ])
  | SuiteCompleted {
    suite;
    status;
    stdout;
    stderr;
    started_at_us;
    completed_at_us;
    duration_us;
    summary
  } ->
      let test_results =
        summary.results
        |> List.map
          (fun (result: test_case_result) ->
            let status_fields =
              match result.result with
              | Passed -> [ ("status", Data.Json.String "passed") ]
              | Skipped -> [ ("status", Data.Json.String "skipped") ]
              | Timed_out { timeout_ms } -> [
                ("status", Data.Json.String "timed_out");
                ("timeout_ms", Data.Json.Int timeout_ms);
              ]
              | Failed message -> [
                ("status", Data.Json.String "failed");
                ("message", Data.Json.String message);
              ]
            in
            let size_fields =
              match result.size with
              | Small -> [ ("size", Data.Json.String "small") ]
              | Large -> [ ("size", Data.Json.String "large") ]
            in
            let reliability_fields =
              match result.reliability with
              | Stable -> [ ("reliability", Data.Json.String "stable") ]
              | Flaky { retry_attempts } -> [
                ("reliability", Data.Json.String "flaky");
                ("retry_attempts", Data.Json.Int retry_attempts);
              ]
            in
            let type_fields =
              match result.test_type with
              | Test -> [ ("type", Data.Json.String "test") ]
              | Property { examples } -> [
                ("type", Data.Json.String "property");
                ("examples", Data.Json.Int examples);
              ]
            in
            Data.Json.Object ([
              ("name", Data.Json.String result.name);
              ("index", Data.Json.Int result.index);
              ("duration_us", Data.Json.Int result.duration_us);
              ("attempts", Data.Json.Int result.attempts);
            ]
            @ status_fields
            @ size_fields
            @ reliability_fields
            @ type_fields))
      in
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "SuiteCompleted");
          ("package", Data.Json.String suite.package_name);
          ("suite", Data.Json.String suite.suite_name);
          ("status", Data.Json.Int status);
          ("stdout", Data.Json.String stdout);
          ("stderr", Data.Json.String stderr);
          (
            "started_at_us",
            match started_at_us with
            | Some value -> Data.Json.Int value
            | None -> Data.Json.Null
          );
          (
            "completed_at_us",
            match completed_at_us with
            | Some value -> Data.Json.Int value
            | None -> Data.Json.Null
          );
          (
            "duration_us",
            match duration_us with
            | Some value -> Data.Json.Int value
            | None -> Data.Json.Int summary.duration_us
          );
          ("tests", Data.Json.Array test_results);
          (
            "summary",
            Data.Json.Object [
              ("total", Data.Json.Int summary.total);
              ("passed", Data.Json.Int summary.passed);
              ("failed", Data.Json.Int summary.failed);
              ("skipped", Data.Json.Int summary.skipped);
              ("duration_us", Data.Json.Int summary.duration_us);
            ]
          );
        ]
      )
  | Summary {
    total;
    passed;
    failed;
    skipped;
    failed_tests
  } ->
      let failed_tests = failed_tests
      |> List.map
        (fun (failed_test: failed_test) ->
          Data.Json.Object [
            ("package", Data.Json.String failed_test.suite.package_name);
            ("suite", Data.Json.String failed_test.suite.suite_name);
            ("name", Data.Json.String failed_test.name);
            ("message", Data.Json.String failed_test.message);
            ("duration_us", Data.Json.Int failed_test.duration_us);
          ]) in
      Some (Data.Json.Object [
        ("type", Data.Json.String "TestSummary");
        ("total", Data.Json.Int total);
        ("passed", Data.Json.Int passed);
        ("failed", Data.Json.Int failed);
        ("skipped", Data.Json.Int skipped);
        ("failed_tests", Data.Json.Array failed_tests);
      ])

let find_suite_binary_path = fun ~(store:Riot_store.Store.t) ~(suite:suite_binary) results ->
  let find_suite_export (result: Riot_executor.Package_builder.build_result) =
    if String.equal result.package.name suite.package_name then
      match result.status with
      | Riot_executor.Package_builder.Built artifact
      | Riot_executor.Package_builder.Cached artifact ->
          List.find_opt
            (fun (entry: Riot_store.Manifest.export_entry) ->
              String.equal entry.name suite.suite_name)
            artifact.exports
      | Riot_executor.Package_builder.Skipped _
      | Riot_executor.Package_builder.Failed _ -> None
    else
      None
  in
  match List.find_map find_suite_export results with
  | None -> Error (SuiteArtifactNotFound {
    suite;
    reason = "suite '" ^ suite.suite_name ^ "' was not produced by build results"
  })
  | Some export_entry -> (
      match Riot_store.Store.export_source_path store export_entry with
      | Some path -> Ok (Path.to_string path)
      | None -> Error (SuiteArtifactNotFound {
        suite;
        reason = "suite '" ^ suite.suite_name ^ "' resolved to an invalid absolute export path"
      })
    )

let run_suite_binary_capture = fun ~workspace_root ~(suite:suite_binary) ~extra_args binary_path ->
  let extra_args = remove_json_args extra_args @ [ "--json" ] in
  let cmd = Command.make
    binary_path
    ~env:[
      ("RIOT_PACKAGE_NAME", suite.package_name);
      ("RIOT_WORKSPACE_ROOT", Path.to_string workspace_root);
    ]
    ~args:("run-tests" :: extra_args) in
  Command.output cmd

let list_suite_binary_capture = fun ~workspace_root ~(suite:suite_binary) ~extra_args binary_path ->
  let extra_args = remove_list_args extra_args @ [ "--json" ] in
  let cmd = Command.make
    binary_path
    ~env:[
      ("RIOT_PACKAGE_NAME", suite.package_name);
      ("RIOT_WORKSPACE_ROOT", Path.to_string workspace_root);
    ]
    ~args:("list-tests" :: extra_args) in
  Command.output cmd

let list_suite = fun ~(workspace:Riot_model.Workspace.t) ~suite ~extra_args binary_path ->
  match list_suite_binary_capture ~workspace_root:workspace.root ~suite ~extra_args binary_path with
  | Error (Command.SystemError reason) -> Error (SuiteExecutionError { suite; reason })
  | Ok output -> (
      match parse_listed_tests_output output.stdout with
      | Error reason -> Error (SuiteExecutionError {
        suite;
        reason = parse_failure_reason ~suite ~output reason
      })
      | Ok tests -> Ok { suite; source_path = find_suite_source_path ~workspace suite; tests }
    )

let list_tests = fun ?(on_suite = no_listed_suite) ?(on_suite_error = no_list_error) (
  request: test_request
) ->
  let suites = collect_suite_binaries
    request.workspace
    ?package_filter:request.package_filter
    ?suite_filter:request.suite_filter
    () in
  if suites = [] then
    Ok []
  else
    match
      Build_runtime.build_best_effort ~record_cache_generation:false
        {
          workspace = request.workspace;
          packages = requested_packages suites;
          targets = Build_runtime.Host;
          scope = Build_runtime.Dev;
          profile = request.profile;
        }
    with
    | Error err -> Error (BuildFailed err)
    | Ok results ->
        let store = Riot_store.Store.create_for_lane
          ~workspace:request.workspace
          ~profile:request.profile
          ~target:(Riot_model.Riot_dirs.host_target ()) in
        let rec resolve_binaries acc = function
          | [] -> (List.rev acc, [])
          | suite :: rest -> (
              match find_suite_binary_path ~store ~suite results with
              | Ok binary_path ->
                  let resolved, missing = resolve_binaries ((suite, binary_path) :: acc) rest in
                  (resolved, missing)
              | Error err ->
                  let resolved, missing = resolve_binaries acc rest in
                  (resolved, (suite, err) :: missing)
            )
        in
        let suite_binaries, missing_suites = resolve_binaries [] suites in
        List.iter (fun (suite, err) -> on_suite_error suite err) missing_suites;
        if suite_binaries = [] then
          Ok []
        else
          let concurrency = Int.max 1 (Int.min 8 Thread.available_parallelism) in
          let parent = self () in
          let rec spawn_initial active remaining =
            if active >= concurrency then
              (active, remaining)
            else
              match remaining with
              | [] -> (active, [])
              | (suite, binary_path) :: rest ->
                  let _worker =
                    spawn
                      (fun () ->
                        let result =
                          try list_suite
                            ~workspace:request.workspace
                            ~suite
                            ~extra_args:request.extra_args
                            binary_path with
                          | exn -> Error (SuiteExecutionError {
                            suite;
                            reason = Kernel.Exception.to_string exn
                          })
                        in
                        send parent (ListedTestsReady (suite, result));
                        Ok ())
                  in
                  spawn_initial (active + 1) rest
          in
          let rec collect active remaining acc =
            if active <= 0 then
              Ok (List.rev acc)
            else
              let suite, result =
                receive
                  ~selector:(fun (msg: Message.t) ->
                    match msg with
                    | ListedTestsReady payload -> `select payload
                    | _ -> `skip)
                  ()
              in
              let acc =
                match result with
                | Ok listed ->
                    on_suite listed;
                    (suite, listed) :: acc
                | Error err ->
                    on_suite_error suite err;
                    acc
              in
              match remaining with
              | [] -> collect (active - 1) [] acc
              | (next_suite, next_binary_path) :: rest ->
                  let _worker =
                    spawn
                      (fun () ->
                        let result =
                          try list_suite
                            ~workspace:request.workspace
                            ~suite:next_suite
                            ~extra_args:request.extra_args
                            next_binary_path with
                          | exn -> Error (SuiteExecutionError {
                            suite = next_suite;
                            reason = Kernel.Exception.to_string exn
                          })
                        in
                        send parent (ListedTestsReady (next_suite, result));
                        Ok ())
                  in
                  collect active rest acc
          in
          let initial_active, remaining = spawn_initial 0 suite_binaries in
          collect initial_active remaining []
          |> Result.map
            (fun collected ->
              collected
              |> List.sort (fun (left, _) (right, _) -> compare_suite_binary left right)
              |> List.map (fun (_, value) -> value))

let test = fun ?(on_event = no_event) (request: test_request) ->
  let suites = collect_suite_binaries
    request.workspace
    ?package_filter:request.package_filter
    ?suite_filter:request.suite_filter
    () in
  if suites = [] then
    (
      on_event
        (NoSuitesFound { package_name = request.package_filter; suite_name = request.suite_filter });
      Ok ()
    )
  else
    match
      Build_runtime.build ~record_cache_generation:false ~on_event:(fun event ->
        on_event (Build event))
        {
          workspace = request.workspace;
          packages = requested_packages suites;
          targets = Build_runtime.Host;
          scope = Build_runtime.Dev;
          profile = request.profile;
        }
    with
    | Error err -> Error (BuildFailed err)
    | Ok results ->
        let store = Riot_store.Store.create_for_lane
          ~workspace:request.workspace
          ~profile:request.profile
          ~target:(Riot_model.Riot_dirs.host_target ()) in
        let total = ref 0 in
        let passed = ref 0 in
        let failed = ref 0 in
        let skipped = ref 0 in
        let failed_tests = ref [] in
        let rec loop = function
          | [] ->
              on_event
                (
                  Summary {
                    total = !total;
                    passed = !passed;
                    failed = !failed;
                    skipped = !skipped;
                    failed_tests = List.rev !failed_tests;
                  }
                );
              if !failed > 0 then
                Error (SuitesFailed !failed)
              else
                Ok ()
          | suite :: rest -> (
              match find_suite_binary_path ~store ~suite results with
              | Error _ as err -> err
              | Ok binary_path ->
                  on_event (RunningSuite suite);
                  match run_suite_binary_capture
                    ~workspace_root:request.workspace.root
                    ~suite
                    ~extra_args:request.extra_args
                    binary_path with
                  | Error (Command.SystemError reason) -> Error (SuiteExecutionError {
                    suite;
                    reason
                  })
                  | Ok output -> (
                      let parsed_output =
                        match parse_test_suite_output output.stdout with
                        | Ok parsed -> Ok parsed
                        | Error reason when Int.equal output.status 0
                        && is_blank output.stdout
                        && is_blank output.stderr -> Ok ("", None, None, None, empty_suite_summary)
                        | Error reason -> Error reason
                      in
                      match parsed_output with
                      | Error reason -> Error (SuiteExecutionError {
                        suite;
                        reason = parse_failure_reason ~suite ~output reason
                      })
                      | Ok (stdout, started_at_us, completed_at_us, duration_us, summary) ->
                          total := !total + summary.total;
                          passed := !passed + summary.passed;
                          failed := !failed + summary.failed;
                          skipped := !skipped + summary.skipped;
                          failed_tests := List.rev_append
                            (
                              summary.results |> List.filter_map
                                (fun (result: test_case_result) ->
                                  match result.result with
                                  | Failed message -> Some {
                                    suite;
                                    name = result.name;
                                    message;
                                    duration_us = result.duration_us
                                  }
                                  | Timed_out { timeout_ms } -> Some {
                                    suite;
                                    name = result.name;
                                    message = "timed out after " ^ Int.to_string timeout_ms ^ "ms";
                                    duration_us = result.duration_us
                                  }
                                  | Passed
                                  | Skipped -> None)
                            )
                            !failed_tests;
                          on_event
                            (
                              SuiteCompleted {
                                suite;
                                status = output.status;
                                stdout;
                                stderr = output.stderr;
                                started_at_us;
                                completed_at_us;
                                duration_us;
                                summary;
                              }
                            );
                          loop rest
                    )
            )
        in
        loop suites
