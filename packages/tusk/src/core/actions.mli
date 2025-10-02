(** Build actions - Concrete build steps and blueprint generation

    This module defines the build actions that can be executed in a sandbox and
    provides blueprint generation for packages. *)

open Std
open Model
open Ocaml

(** Build action types *)
type action =
  (* File compilation actions *)
  | CompileInterface of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Ocamlc.compiler_flag list;
    }  (** Compile .mli file *)
  | CompileImplementation of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Ocamlc.compiler_flag list;
    }  (** Compile .ml file *)
  | GenerateInterface of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Ocamlc.compiler_flag list;
    }  (** Generate .mli from .ml using ocamlc -i *)
  | CompileC of { source : Path.t; output : Path.t }  (** Compile C file *)
  (* Linking actions *)
  | CreateLibrary of {
      output : Path.t;
      objects : Path.t list;
      includes : Path.t list;
    }  (** Create .cma library *)
  | CreateExecutable of {
      output : Path.t;
      objects : Path.t list;
      libraries : Path.t list;
      includes : Path.t list;
    }  (** Create executable *)
  (* File operations *)
  | CopyDir of { source : Path.t; destination : Path.t }
      (** Copy directory recursively *)
  | CopyFile of { source : Path.t; destination : Path.t }  (** Copy file *)
  | WriteFile of { destination : Path.t; content : string }
      (** Write content to file *)
  (* Output declaration *)
  | DeclareOutputs of { outputs : Path.t list }
      (** Declare output files that should be copied to target *)

(** Result of executing an action *)
type action_result = Success | Failed of string | Skipped of string

(** {1 Action Execution} *)

val execute_action :
  action -> Toolchains.toolchain -> Std.Path.t -> action_result * string
(** Execute a single build action in the specified working directory. Returns (result, output_message). *)

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
