open Std

type action =
  | List_rules of { format: Reporter.format }
  | List_diagnostics of { format: Reporter.format }
  | Explain_rule of { rule_id: string }
  | Run of {
      mode: Runner.mode;
      limit: int option;
      target: Path.t;
      forwarded_args: string list;
      output_mode: Types.output_mode;
      use_generated_runner: bool
    }

type t = {
  cwd: Path.t;
  scope: Fix_config.scope option;
  action: action;
}

let parse_limit = fun matches ->
  match ArgParser.get_int matches "limit" with
  | Some n when n > 0 -> Ok (Some n)
  | Some _ -> Error (Failure "--limit must be greater than 0")
  | None -> Ok None

let reporter_format = fun matches ->
  if ArgParser.get_flag matches "json" then
    Reporter.Json
  else
    Reporter.Text

let output_mode = fun format -> Types.Report format

let generated_runner_disabled = fun () ->
  match Env.var String ~name:"TUSK_FIX_DISABLE_GENERATED_RUNNER" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let use_generated_runner = fun scope ->
  if generated_runner_disabled () then
    false
  else
    match scope with
    | Some scope when List.length (Fix_config.providers (Some scope)) > 0 -> true
    | _ -> false

let of_matches = fun matches ->
  let cwd = Common.current_dir () in
  let scope = Fix_config.load_scope ~cwd in
  let apply = ArgParser.get_flag matches "apply" in
  let check = ArgParser.get_flag matches "check" in
  let format = reporter_format matches in
  match parse_limit matches with
  | Error _ as err -> err
  | Ok limit ->
      if apply && check then
        Error (Failure "cannot use both --apply and --check")
      else
        let action =
          match ArgParser.get_flag matches "list-rules", ArgParser.get_flag matches "list-diagnostics", ArgParser.get_one
            matches
            "explain" with
          | true, _, _ ->
              List_rules { format }
          | false, true, _ ->
              List_diagnostics { format }
          | false, false, Some rule_id ->
              Explain_rule { rule_id }
          | false, false, None ->
              let mode =
                if apply then
                  Runner.Apply
                else
                  Runner.Check
              in
              Run {
                mode;
                limit;
                target = Common.resolve_target matches;
                forwarded_args = Common.args_of_matches matches;
                output_mode = output_mode format;
                use_generated_runner = use_generated_runner scope;
              }
        in
        Ok { cwd; scope; action }
