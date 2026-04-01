open Std

type build_package = Cli.Types.build_package

type fix_output_mode = Cli.Types.output_mode =
  Silent
  | Report of Reporter.format

type fix_action = Cli.Request.action =
  | List_rules of { format: Reporter.format }
  | List_diagnostics of { format: Reporter.format }
  | Explain_rule of { rule_id: string }
  | Run of {
      mode: Runner.mode;
      limit: int option;
      target: Path.t;
      forwarded_args: string list;
      output_mode: fix_output_mode;
      use_generated_runner: bool
    }

type fix_request = Cli.Request.t = {
  cwd: Path.t;
  scope: Fix_config.scope option;
  action: fix_action;
}

type fix_response =
  | Completed
  | Listed_rules of { format: Reporter.format; output: string }
  | Listed_diagnostics of { format: Reporter.format; output: string }
  | Explained_rule of { rule_id: string; output: string }

let unavailable_build_package = fun ~workspace_root:_ ~package_name:_ ->
  Error (Failure "No build_package callback was provided")

let check_request = Cli.Request.check_request

let fix_request_of_matches = Cli.Request.of_matches

let output_mode_of_request = fun request ->
  match request.action with
  | Run { output_mode; _ } -> output_mode
  | List_rules { format }
  | List_diagnostics { format } -> Report format
  | Explain_rule _ -> Report Reporter.Text

let explain_rule_output = fun rule_id ->
  match Explanations.explain rule_id with
  | Some entry -> Ok (Explanations.format entry)
  | None -> Error (Failure ("Unknown tusk-fix rule id: " ^ rule_id))

let response_output = function
  | Completed -> None
  | Listed_rules { output; _ }
  | Listed_diagnostics { output; _ }
  | Explained_rule { output; _ } -> Some output

let no_event = fun (_: Event.t) -> ()

let fix = fun ?(build_package = unavailable_build_package) ?(on_event = no_event) ?output_mode request ->
  let output_mode =
    match output_mode with
    | Some output_mode -> output_mode
    | None -> output_mode_of_request request
  in
  match request.action with
  | List_rules { format } -> Ok (Listed_rules {
    format;
    output = Cli.Catalog.list_rules_output ~format
  })
  | List_diagnostics { format } -> Ok (Listed_diagnostics {
    format;
    output = Cli.Catalog.list_diagnostics_output ~format
  })
  | Explain_rule { rule_id } -> explain_rule_output rule_id
  |> Result.map (fun output -> Explained_rule { rule_id; output })
  | Run {
    mode;
    limit;
    target;
    forwarded_args;
    use_generated_runner;
    _
  } ->
      (
        match request.scope, use_generated_runner with
        | Some scope, true ->
            let report_output =
              match output_mode with
              | Silent -> false
              | Report _ -> true
            in
            Cli.Execution.run_generated_runner
              ~cwd:request.cwd
              ~build_package
              ~report_output
              ~args:forwarded_args
              scope
        | _ -> Cli.Execution.run_with_coordinator
          ~on_event
          ~output_mode
          ~mode
          ~scope:request.scope
          ~limit
          ~roots:[ target ]
          ()
      ) |> Result.map (fun () -> Completed)
