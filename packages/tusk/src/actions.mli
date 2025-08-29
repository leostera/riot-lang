(** Build actions - Concrete build steps and blueprint generation

    This module defines the build actions that can be executed in a sandbox and
    provides blueprint generation for packages. *)

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

val hash : action list -> Hasher.hash
(** Hash a list of actions *)

(** {1 Display} *)

val string_of_action : action -> string
(** Pretty-print an action *)
