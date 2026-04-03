open Std

type action =
  | ListRules of { format: Reporter.format }
  | ListDiagnostics of { format: Reporter.format }
  | ExplainRule of { rule_id: string }
  | Run of {
      mode: Runner.mode;
      limit: int option;
      target: Path.t;
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
