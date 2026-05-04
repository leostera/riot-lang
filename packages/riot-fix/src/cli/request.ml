open Std

type action =
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
      output_mode: Types.output_mode;
      use_generated_runner: bool;
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

let use_generated_runner = fun scope ->
  match scope with
  | Some scope when not (List.is_empty (Fix_config.providers (Some scope))) -> true
  | _ -> false

let check_request = fun ~cwd ~target ->
  let scope = Fix_config.load_scope ~cwd in
  {
    cwd;
    scope;
    action =
      Run {
        mode = Runner.Check;
        limit = None;
        target;
        output_mode = Types.Silent;
        use_generated_runner = use_generated_runner scope;
      };
  }

let from_matches = fun matches ->
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
          match (
            ArgParser.get_flag matches "list-rules",
            ArgParser.get_flag matches "list-diagnostics",
            ArgParser.get_one matches "explain"
          ) with
          | (true, _, _) -> ListRules { format }
          | (false, true, _) -> ListDiagnostics { format }
          | (false, false, Some rule_id) -> ExplainRule { rule_id = Rule_id.from_string rule_id }
          | (false, false, None) ->
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
                output_mode = output_mode format;
                use_generated_runner = use_generated_runner scope;
              }
        in
        Ok { cwd; scope; action }
