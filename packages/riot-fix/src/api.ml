open Std

type build_package = Cli.Types.build_package

type fix_output_mode = Cli.Types.output_mode =
  | Silent
  | Report of Reporter.format

type fix_action = Cli.Request.action =
  | ListRules of {
      format: Reporter.format;
    }
  | ListDiagnostics of {
      format: Reporter.format;
    }
  | ExplainRule of {
      rule_id: Rule_id.t;
    }
  | Run of {
      mode: Runner.mode;
      limit: int option;
      target: Path.t;
      output_mode: fix_output_mode;
      use_generated_runner: bool;
    }

type fix_request = Cli.Request.t = {
  cwd: Path.t;
  scope: Fix_config.scope option;
  action: fix_action;
}

type fix_response =
  | Completed
  | ListedRules of {
      format: Reporter.format;
      output: string;
    }
  | ListedDiagnostics of {
      format: Reporter.format;
      output: string;
    }
  | ExplainedRule of {
      rule_id: Rule_id.t;
      output: string;
    }

let unavailable_build_package = fun
  ~workspace:_ ~package_name:_ ~profile:_ ?transform_workspace:_ () ->
  Error (Failure "No build_package callback was provided")

let check_request = Cli.Request.check_request

let fix_request_of_matches = Cli.Request.from_matches

let output_mode_of_request = fun request ->
  match request.action with
  | Run { output_mode; _ } -> output_mode
  | ListRules { format }
  | ListDiagnostics { format } -> Report format
  | ExplainRule _ -> Report Reporter.Text

let explain_rule_output = fun rule_id ->
  match Explanations.explain rule_id with
  | Some entry -> Ok (Explanations.format entry)
  | None -> Error (Failure ("Unknown riot-fix rule id: " ^ Rule_id.to_string rule_id))

let response_output response =
  match response with
  | Completed -> None
  | ListedRules { output; _ }
  | ListedDiagnostics { output; _ }
  | ExplainedRule { output; _ } -> Some output

let no_event = fun (_: Event.t) -> ()

let fix = fun
  ?(build_package = unavailable_build_package) ?(on_event = no_event) ?output_mode request ->
  let output_mode =
    match output_mode with
    | Some output_mode -> output_mode
    | None -> output_mode_of_request request
  in
  match request.action with
  | ListRules { format } ->
      Ok (ListedRules { format; output = Cli.Catalog.list_rules_output ~format })
  | ListDiagnostics { format } ->
      Ok (ListedDiagnostics { format; output = Cli.Catalog.list_diagnostics_output ~format })
  | ExplainRule { rule_id } ->
      explain_rule_output rule_id
      |> Result.map ~fn:(fun output -> ExplainedRule { rule_id; output })
  | Run {
      mode;
      limit;
      target;
      use_generated_runner;
      output_mode = _;
    } ->
      (
        match (request.scope, use_generated_runner) with
        | (Some scope, true) ->
            let report_output =
              match output_mode with
              | Silent -> false
              | Report _ -> true
            in
            Cli.Execution.run_generated_runner
              ~cwd:request.cwd
              ~build_package
              ~report_output
              ~mode
              ~limit
              ~target
              ~output_mode
              scope
        | _ ->
            Cli.Execution.run_with_coordinator
              ~on_event
              ~output_mode
              ~mode
              ~scope:request.scope
              ~limit
              ~roots:[ target ]
              ()
      )
      |> Result.map ~fn:(fun () -> Completed)
