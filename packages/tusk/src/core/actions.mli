(** Build actions - Concrete build steps and blueprint generation

    This module defines the build actions that can be executed in a sandbox and
    provides blueprint generation for packages. *)

open Model
open Ocaml

(** Build action types *)
type action =
  (* File compilation actions *)
  | CompileInterface of {
      source : string;
      output : string;
      includes : string list;
      flags : Ocamlc.compiler_flag list;
    }  (** Compile .mli file *)
  | CompileImplementation of {
      source : string;
      output : string;
      includes : string list;
      flags : Ocamlc.compiler_flag list;
    }  (** Compile .ml file *)
  | GenerateInterface of {
      source : string;
      output : string;
      includes : string list;
      flags : Ocamlc.compiler_flag list;
    }  (** Generate .mli from .ml using ocamlc -i *)
  | CompileC of { source : string; output : string }  (** Compile C file *)
  (* Linking actions *)
  | CreateLibrary of {
      output : string;
      objects : string list;
      includes : string list;
    }  (** Create .cma library *)
  | CreateExecutable of {
      output : string;
      objects : string list;
      libraries : string list;
      includes : string list;
    }  (** Create executable *)
  (* File operations *)
  | CopyDir of { source : string; destination : string }  (** Copy directory recursively *)
  | CopyFile of { source : string; destination : string }  (** Copy file *)
  | WriteFile of { destination : string; content : string }
      (** Write content to file *)
  (* Output declaration *)
  | DeclareOutputs of { outputs : string list }
      (** Declare output files that should be copied to target *)

(** Result of executing an action *)
type action_result = Success | Failed of string | Skipped of string

(** {1 Action Execution} *)

val execute_action : action -> Toolchains.toolchain -> action_result * string
(** Execute a single build action. Returns (result, output_message). *)

(** {1 Hashing} *)

val action_to_string : action -> string
(** Convert action to canonical string representation for hashing *)

val hash : action list -> Std.Crypto.hash
(** Hash a list of actions *)

val hash_action :
  (module Std.Crypto.Hasher.Intf with type state = 'state) ->
  'state ->
  action ->
  unit
(** Hash a single action into a hasher *)

val hash_actions :
  (module Std.Crypto.Hasher.Intf with type state = 'state) ->
  'state ->
  action list ->
  unit
(** Hash a list of actions into a hasher *)

(** {1 Display} *)

val string_of_action : action -> string
(** Pretty-print an action *)
