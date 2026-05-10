open Std

module Types = Types
module Request = Request
module Catalog = Catalog
module Execution = Execution

type run_outcome = Types.run_outcome = {
  result: Runner.run_result;
  limit_reached: bool;
}

type event = Types.event =
  | Start of {
      mode: Runner.mode;
      concurrency: int;
    }
  | FileStarted of {
      file: Path.t;
    }
  | FileProgress of {
      file: Path.t;
      progress: Fixme.Source_runner.progress_event;
    }
  | FileResult of Runner.file_result
  | Summary of {
      summary: Runner.summary;
      limit_reached: bool;
    }

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "fix"
  |> about "Lint OCaml code and optionally apply safe fixes"
  |> args
    [
      flag "list-rules"
      |> long "list-rules"
      |> help "List all available rules in the current riot-fix runtime";
      flag "list-diagnostics"
      |> long "list-diagnostics"
      |> help "List all available diagnostics in the current riot-fix runtime";
      flag "apply"
      |> long "apply"
      |> help "Apply safe fixes to files";
      flag "check"
      |> long "check"
      |> help "Check for issues without modifying files (default; kept for compatibility)";
      option "limit"
      |> long "limit"
      |> help "Stop after surfacing at most N diagnostics";
      option "explain"
      |> long "explain"
      |> help "Explain a rule id (e.g. riot:snake-case-type-names or std:no-stdlib)";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      positional "path"
      |> required false
      |> help "OCaml file or directory to scan (default: workspace packages or current directory)";
    ]

let list_rules_output = Catalog.list_rules_output

let list_diagnostics_output = Catalog.list_diagnostics_output

let run_result = Execution.run_result

let unavailable_build_package = fun
  ~workspace:_ ~package_name:_ ~profile:_ ?transform_workspace:_ () ->
  Error (Failure "No build_package callback was provided")

let no_event = fun (_: event) -> ()

let resolved_output_mode = fun ?output_mode (request: Request.t) ->
  match output_mode with
  | Some output_mode -> output_mode
  | None ->
      match Request.(request.action) with
      | Request.Run { output_mode; _ } -> output_mode
      | Request.ListRules { format }
      | Request.ListDiagnostics { format } -> Types.Report format
      | Request.ExplainRule _ -> Types.Report Reporter.Text

let run_request_direct = fun ~on_event ~output_mode (request: Request.t) ->
  match request.action with
  | Request.ListRules { format } -> Catalog.list_rules format
  | Request.ListDiagnostics { format } -> Catalog.list_diagnostics format
  | Request.ExplainRule { rule_id } -> Catalog.explain_rule rule_id
  | Request.Run {
      mode;
      limit;
      target;
      output_mode = _;
      use_generated_runner = _;
    } ->
      Execution.run_with_coordinator
        ~on_event
        ~output_mode
        ~mode
        ~scope:request.scope
        ~limit
        ~roots:[ target ]
        ()

let run_matches = fun ~build_package ?(on_event = Types.no_event) ?output_mode matches ->
  match Request.from_matches matches with
  | Error _ as err -> err
  | Ok request ->
      let output_mode = resolved_output_mode ?output_mode request in
      match request.action with
      | Request.ListRules { format } -> Catalog.list_rules format
      | Request.ListDiagnostics { format } -> Catalog.list_diagnostics format
      | Request.ExplainRule { rule_id } -> Catalog.explain_rule rule_id
      | Request.Run {
          mode;
          limit;
          target;
          use_generated_runner;
          output_mode = _;
        } ->
          match (request.scope, use_generated_runner) with
          | (Some scope, true) ->
              let report_output =
                match output_mode with
                | Types.Silent -> false
                | Types.Report _ -> true
              in
              Execution.run_generated_runner
                ~cwd:request.cwd
                ~build_package
                ~report_output
                ~mode
                ~limit
                ~target
                ~output_mode
                scope
          | _ -> run_request_direct ~on_event ~output_mode request

let run = fun ?(build_package = unavailable_build_package) matches ->
  run_matches
    ~build_package
    matches

let run_args = fun ?cwd ?(on_event = Types.no_event) ?(report_output = true) ~build_package args ->
  Common.with_cwd
    ?cwd
    (fun () ->
      match ArgParser.get_matches command ("fix" :: args) with
      | Error err ->
          ArgParser.print_error err;
          ArgParser.print_help command;
          Error (Failure "Argument parsing failed")
      | Ok matches ->
          let output_mode =
            if report_output then
              None
            else
              Some Types.Silent
          in
          run_matches ~build_package ~on_event ?output_mode matches)

let run_check_paths = fun
  ?cwd ?(on_event = Types.no_event) ?(report_output = false) ~build_package paths ->
  let args = "--check" :: List.map paths ~fn:Path.to_string in
  run_args ?cwd ~on_event ~report_output ~build_package args

let main = fun ?(build_package = unavailable_build_package) ~args () ->
  match ArgParser.get_matches command args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help command;
      Error (Failure "Argument parsing failed")
  | Ok matches -> run ~build_package matches
