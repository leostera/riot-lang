open Std

type suite_binary = {
  package_name: string;
  suite_name: string;
}

type test_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  suite_filter: string option;
  query: string option;
  extra_args: string list;
}

type test_event =
  | Build of Build_runtime.build_event
  | NoSuitesFound of { package_name: string option; suite_name: string option }
  | RunningSuite of suite_binary
  | SuiteCompleted of { suite: suite_binary; status: int; stdout: string; stderr: string }
  | Summary of { total: int; passed: int; failed: int }

type test_error =
  | BuildFailed of Build_runtime.build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

let no_event: test_event -> unit = fun _ -> ()

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

let test_error_message = function
  | BuildFailed err -> Build_runtime.error_message err
  | ClientError err -> Client.error_message err
  | SuiteArtifactNotFound { reason; _ } -> reason
  | SuiteExecutionError { reason; _ } -> reason
  | SuitesFailed count -> Int.to_string count ^ " test suite(s) failed"

let test_event_to_json = function
  | Build event -> Event.to_json event
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
  | RunningSuite { package_name; suite_name } -> Some (Data.Json.Object [
    ("type", Data.Json.String "RunningSuite");
    ("package", Data.Json.String package_name);
    ("suite", Data.Json.String suite_name);
  ])
  | SuiteCompleted { suite; status; stdout; stderr } -> Some (Data.Json.Object [
    ("type", Data.Json.String "SuiteCompleted");
    ("package", Data.Json.String suite.package_name);
    ("suite", Data.Json.String suite.suite_name);
    ("status", Data.Json.Int status);
    ("stdout", Data.Json.String stdout);
    ("stderr", Data.Json.String stderr);
  ])
  | Summary { total; passed; failed } -> Some (Data.Json.Object [
    ("type", Data.Json.String "TestSummary");
    ("total", Data.Json.Int total);
    ("passed", Data.Json.Int passed);
    ("failed", Data.Json.Int failed);
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
  let cmd = Command.make
    binary_path
    ~env:[
      ("RIOT_PACKAGE_NAME", suite.package_name);
      ("RIOT_WORKSPACE_ROOT", Path.to_string workspace_root);
    ]
    ~args:(("run-tests" :: extra_args)) in
  Command.output cmd

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
          profile = "debug";
        }
    with
    | Error err -> Error (BuildFailed err)
    | Ok results ->
        let store = Riot_store.Store.create_for_lane
          ~workspace:request.workspace
          ~profile:"debug"
          ~target:(Riot_model.Riot_dirs.host_target ()) in
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
              match find_suite_binary_path ~store ~suite results with
              | Error _ as err -> err
              | Ok binary_path ->
                  on_event (RunningSuite suite);
                  match run_suite_binary_capture
                    ~workspace_root:request.workspace.root
                    ~suite
                    ~extra_args
                    binary_path with
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
