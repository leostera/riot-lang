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
val unavailable_build_package: build_package

val check_request: cwd:Path.t -> target:Path.t -> fix_request

val fix_request_of_matches: ArgParser.matches -> (fix_request, exn) result

val output_mode_of_request: fix_request -> fix_output_mode

val fix:
  ?build_package:build_package ->
  ?on_event:(Event.t -> unit) ->
  ?output_mode:fix_output_mode ->
  fix_request ->
  (fix_response, exn) result

val response_output: fix_response -> string option
