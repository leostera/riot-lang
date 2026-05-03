open Std

(**
   OCaml linting and safe-fix pipeline.

   A pipeline-based linter and code fixer for OCaml, built on top of the syn parser.
*)

(** Typed rule identifiers shared across lint surfaces. *)
module Rule_id: module type of Rule_id

(** Structured diagnostic information for lint errors. *)
module Diagnostic: module type of Diagnostic

(** Types for code fixes and transformations. *)
module Fix: module type of Fix

(** Linting pipeline orchestration. *)
module Pipeline: module type of Pipeline

(** Package-provided riot-fix rule surface. *)
module Provider: module type of Provider

(** Runtime registry for package-provided rules and explanations. *)
module Provider_registry: module type of Provider_registry

(** Diagnostic output formatting. *)
module Reporter: module type of Reporter

(** Lint rule abstraction. *)
module Rule: module type of Rule

(** Synchronous lint/apply runner for files and directories. *)
module Runner: module type of Runner

(** Structured `riot fix` event payloads and JSON serialization. *)
module Event: module type of Event

(** CLI surface shared by the standalone binary and `riot fix`. *)
module Cli: module type of Cli

(** Built-in lint rules. *)
module Rules: module type of Rules

(** Ast traversal helpers. *)
module Traversal: module type of Traversal

(** Pure rule execution and safe-fix application on source strings. *)
module Source_runner: module type of Fixme.Source_runner

(** Test helper for running rules, applying fixes, and rerunning on updated source. *)
module Rule_test: module type of Fixme.Rule_test

(** Rule-oriented Ast query helpers. *)
module Rule_query: module type of Rule_query

(** File system scanner for finding OCaml source files. *)
module File_scanner: module type of File_scanner

(** Shared message types for coordinator and worker communication. *)
module Messages: module type of Messages

(** Worker actor for linting individual files. *)
module Worker: module type of Worker

(** Coordinator actor for managing lint workers. *)
module Coordinator: module type of Coordinator

(** Workspace and package-local configuration resolution for `riot fix`. *)
module Config: module type of Fix_config

(** Shared explanation entry type used by built-in and provider rules. *)
module Explanation: module type of Explanation

(** Explanation lookup for loaded built-in and provider rules. *)
module Explanations: module type of Explanations

(** Build-time fixme runner planning for package-provided rules. *)
module Fixme_runner: module type of Fixme_runner

type build_package = Api.build_package
type fix_output_mode = Api.fix_output_mode =
  | Silent
  | Report of Reporter.format
type fix_action = Api.fix_action =
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
type fix_request = Api.fix_request = {
  cwd: Path.t;
  scope: Fix_config.scope option;
  action: fix_action;
}
type fix_response = Api.fix_response =
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
