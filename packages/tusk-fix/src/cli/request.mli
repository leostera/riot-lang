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
val check_request: cwd:Path.t -> target:Path.t -> t
val of_matches: ArgParser.matches -> (t, exn) result
