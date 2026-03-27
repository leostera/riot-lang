open Std

(** Tusk-Fix - OCaml Linter and Code Fixer
    
    A pipeline-based linter and code fixer for OCaml, built on top of the syn parser.
*)

module Diagnostic : module type of Diagnostic
(** Structured diagnostic information for lint errors *)

module Fix : module type of Fix
(** Types for code fixes and transformations *)

module Pipeline : module type of Pipeline
(** Linting pipeline orchestration *)

module Provider : module type of Provider
(** Package-provided tusk-fix rule surface *)

module Provider_registry : module type of Provider_registry
(** Runtime registry for package-provided rules and explanations *)

module Reporter : module type of Reporter
(** Diagnostic output formatting *)

module Rule : module type of Rule
(** Lint rule abstraction *)

module Runner : module type of Runner
(** Synchronous lint/apply runner for files and directories *)

module Cli : module type of Cli
(** CLI surface shared by the standalone binary and `tusk fix` *)

module Rules : module type of Rules
(** Built-in lint rules *)

module Traversal : module type of Traversal
(** CST traversal helpers *)

module Source_runner : module type of Fixme.Source_runner
(** Pure rule execution and safe-fix application on source strings *)

module Rule_test : module type of Fixme.Rule_test
(** Test helper for running rules, applying fixes, and rerunning on updated source *)

module Rule_query : module type of Rule_query
(** Rule-oriented CST query helpers built on top of `Syn.Visit` *)

module File_scanner : module type of File_scanner
(** File system scanner for finding OCaml source files *)

module Messages : module type of Messages
(** Shared message types for coordinator and worker communication *)

module Worker : module type of Worker
(** Worker actor for linting individual files *)

module Coordinator : module type of Coordinator
(** Coordinator actor for managing lint workers *)

module Config : module type of Fix_config
(** Workspace and package-local configuration resolution for `tusk fix` *)

module Explanation : module type of Explanation
(** Shared explanation entry type used by built-in and provider rules *)

module Explanations : module type of Explanations
(** Explanation lookup for loaded built-in and provider rules *)

module Fixme_runner : module type of Fixme_runner
(** Build-time fixme runner planning for package-provided rules *)
