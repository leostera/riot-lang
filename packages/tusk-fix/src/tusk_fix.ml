open Std

(** Tusk-Fix - OCaml Linter and Code Fixer *)
module Diagnostic = Diagnostic
module Fix = Fix
module Pipeline = Pipeline
module Provider = Provider
module Provider_registry = Provider_registry
module Reporter = Reporter
module Rule = Rule
module Runner = Runner
module Event = Event
module Cli = Cli
module Rules = Rules
module Traversal = Traversal
module Source_runner = Fixme.Source_runner
module Rule_test = Fixme.Rule_test
module Rule_query = Rule_query
module File_scanner = File_scanner
module Messages = Messages
module Worker = Worker
module Coordinator = Coordinator
module Config = Fix_config
module Explanation = Explanation
module Explanations = Explanations
module Fixme_runner = Fixme_runner

type build_package = Api.build_package

type fix_output_mode = Api.fix_output_mode =
  | Silent
  | Report of Reporter.format

type fix_action = Api.fix_action =
  | ListRules of { format: Reporter.format }
  | ListDiagnostics of { format: Reporter.format }
  | ExplainRule of { rule_id: string }
  | Run of {
      mode: Runner.mode;
      limit: int option;
      target: Path.t;
      forwarded_args: string list;
      output_mode: fix_output_mode;
      use_generated_runner: bool
    }

type fix_request = Api.fix_request = {
  cwd: Path.t;
  scope: Fix_config.scope option;
  action: fix_action;
}

type fix_response = Api.fix_response =
  | Completed
  | ListedRules of { format: Reporter.format; output: string }
  | ListedDiagnostics of { format: Reporter.format; output: string }
  | ExplainedRule of { rule_id: string; output: string }

let check_request = Api.check_request

let fix_request_of_matches = Api.fix_request_of_matches

let output_mode_of_request = Api.output_mode_of_request

let fix = Api.fix

let response_output = Api.response_output
