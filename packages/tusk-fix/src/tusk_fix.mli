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

module File_scanner : module type of File_scanner
(** File system scanner for finding OCaml source files *)

module Messages : module type of Messages
(** Shared message types for coordinator and worker communication *)

module Worker : module type of Worker
(** Worker actor for linting individual files *)

module Coordinator : module type of Coordinator
(** Coordinator actor for managing lint workers *)
