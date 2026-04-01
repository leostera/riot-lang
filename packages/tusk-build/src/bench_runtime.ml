open Std

type suite_binary = Test_runtime.suite_binary = {
  package_name: string;
  suite_name: string;
}

type bench_request = {
  workspace: Tusk_model.Workspace.t;
  package_filter: string option;
  query: string option;
  extra_args: string list;
}

type bench_event =
  | Build of Build_runtime.build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of { suite: suite_binary; status: int; stdout: string; stderr: string }
  | Summary of { total: int; passed: int; failed: int }

type bench_error =
  | BuildFailed of Build_runtime.build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

let no_event : bench_event -> unit = fun _ -> ()

let is_benchmark_binary_name = fun name ->
  String.ends_with ~suffix:"_bench" name || String.ends_with ~suffix:"-bench" name

let compare_suite_binary = fun left right ->
  String.compare
    (left.package_name ^ ":" ^ left.suite_name)
    (right.package_name ^ ":" ^ right.suite_name)

let collect_suite_binaries = fun (workspace: Tusk_model.Workspace.t) ?package_filter () ->
  workspace.packages |> List.filter Tusk_model.Package.is_workspace_member |> List.filter
    (fun (pkg: Tusk_model.Package.t) ->
      match package_filter with
      | None -> true
      | Some package_name -> String.equal pkg.name package_name) |> List.concat_map
    (fun (pkg: Tusk_model.Package.t) ->
      List.filter_map
        (fun (bin: Tusk_model.Package.binary) ->
          if is_benchmark_binary_name bin.name then
            Some { package_name = pkg.name; suite_name = bin.name }
          else
            None)
        pkg.binaries) |> List.sort compare_suite_binary

let bench_error_message = function
  | BuildFailed err -> Build_runtime.error_message err
  | ClientError err -> Client.error_message err
  | SuiteArtifactNotFound { reason; _ } -> reason
  | SuiteExecutionError { reason; _ } -> reason
  | SuitesFailed count -> Int.to_string count ^ " benchmark suite(s) failed"

let bench_event_to_json = function
  | Build event -> Event.to_json event
  | NoSuitesFound { package_name } ->
      Some (
        Data.Json.Object [ ("type", Data.Json.String "NoBenchSuitesFound"); (
            "package_name",
            match package_name with
            | Some name -> Data.Json.String name
            | None -> Data.Json.Null
          ); ]
      )
  | RunningSuite { package_name; suite_name } -> Some (Data.Json.Object [
    ("type", Data.Json.String "RunningBenchSuite");
    ("package", Data.Json.String package_name);
    ("suite", Data.Json.String suite_name);
  ])
  | SuiteCompleted { suite; status; stdout; stderr } -> Some (Data.Json.Object [
    ("type", Data.Json.String "BenchSuiteCompleted");
    ("package", Data.Json.String suite.package_name);
    ("suite", Data.Json.String suite.suite_name);
    ("status", Data.Json.Int status);
    ("stdout", Data.Json.String stdout);
    ("stderr", Data.Json.String stderr);
  ])
  | Summary { total; passed; failed } -> Some (Data.Json.Object [
    ("type", Data.Json.String "BenchSummary");
    ("total", Data.Json.Int total);
    ("passed", Data.Json.Int passed);
    ("failed", Data.Json.Int failed);
  ])

let reconnect = fun ~workspace ->
  Client.connect_local ~workspace () |> Result.map_error (fun err -> ClientError err)

let run_suite_binary_capture = fun ~extra_args binary_path ->
  let cmd = Command.make binary_path ~args:(("run-benchmarks" :: extra_args)) in
  Command.output cmd

let bench = fun ?(on_event = no_event) (request: bench_request) ->
  let suites = collect_suite_binaries request.workspace ?package_filter:request.package_filter () in
  if suites = [] then
    (
      on_event (NoSuitesFound { package_name = request.package_filter });
      Ok ()
    )
  else
    match
      Build_runtime.build ~on_event:(fun event -> on_event (Build event))
        {
          workspace = request.workspace;
          packages = [];
          targets = Build_runtime.Host;
          scope = Build_runtime.Dev;
          profile = "debug";
        }
    with
    | Error err -> Error (BuildFailed err)
    | Ok () -> (
        match reconnect ~workspace:request.workspace with
        | Error _ as err -> err
        | Ok client ->
            let result =
              let total = ref 0 in
              let passed = ref 0 in
              let failed = ref 0 in
              let extra_args =
                match request.query with
                | None -> request.extra_args
                | Some query -> query :: request.extra_args
              in
              let rec loop = function
                | [] ->
                    on_event (Summary { total = !total; passed = !passed; failed = !failed });
                    if !failed > 0 then
                      Error (SuitesFailed !failed)
                    else
                      Ok ()
                | suite :: rest -> (
                    total := !total + 1;
                    match Client.find_artifact
                      client
                      ~package:suite.package_name
                      ~kind:"binary"
                      ~name:suite.suite_name with
                    | Error reason -> Error (SuiteArtifactNotFound { suite; reason })
                    | Ok binary_path ->
                        on_event (RunningSuite suite);
                        match run_suite_binary_capture ~extra_args binary_path with
                        | Error (Command.SystemError reason) -> Error (SuiteExecutionError {
                          suite;
                          reason
                        })
                        | Ok output ->
                            on_event
                              (SuiteCompleted {
                                suite;
                                status = output.status;
                                stdout = output.stdout;
                                stderr = output.stderr
                              });
                            if Int.equal output.status 0 then
                              passed := !passed + 1
                            else
                              failed := !failed + 1;
                              loop rest
                  )
              in
              loop suites
            in
            Client.close client;
            result
      )
