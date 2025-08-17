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
    }  (** Compile .mli file *)
  | CompileImplementation of {
      source : string;
      output : string;
      includes : string list;
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

type dep_info = {
  name : string;  (** Package name *)
  relative_path : string;  (** Relative path from workspace root *)
  dependencies : string list;  (** Names of this dependency's dependencies *)
}
(** Dependency information *)

type blueprint = {
  package_name : string;  (** Name of the package being built *)
  package_path : string;  (** Absolute path to package directory *)
  dependencies : dep_info list;  (** Direct dependencies of this package *)
  actions : action list;  (** Ordered list of build actions to execute *)
  toolchain : Toolchains.toolchain;
      (** OCaml toolchain to use for compilation *)
  hash : Hasher.hash option;  (** Content-based hash of all inputs *)
}
(** Build blueprint - Complete build plan for a package *)

(** {1 Blueprint Generation} *)

val generate_blueprint :
  Workspace.workspace ->
  Build_node.t ->
  dep_info list ->
  dep_info list ->
  Toolchains.toolchain ->
  hash:Hasher.hash ->
  unit ->
  blueprint
(** Generate build blueprint for a package.

    [generate_blueprint root pkg_name pkg_path pkg_relative_path dependencies
     all_packages toolchain ~hash ()]

    Creates a complete build plan including:
    - Dependency resolution
    - Source file discovery and ordering (via ocamldep)
    - Action generation for compilation and linking
    - Output declarations

    @param root Workspace root directory
    @param pkg_name Package name
    @param pkg_path Absolute path to package
    @param pkg_relative_path Relative path from workspace root
    @param dependencies Direct dependencies as dep_info list
    @param all_packages All packages in workspace (for transitive deps)
    @param toolchain OCaml toolchain to use
    @param hash Content hash for caching *)

(** {1 Action Execution} *)

val execute_action : action -> Toolchains.toolchain -> action_result * string
(** Execute a single build action. Returns (result, output_message). *)

val execute_blueprint : Workspace.workspace -> blueprint -> bool * string
(** Execute all actions in a blueprint. Returns (success, message) where success
    is true if all actions succeeded. *)

(** {1 Hashing} *)

val action_to_string : action -> string
(** Convert action to canonical string representation for hashing *)

val compute_blueprint_hash : blueprint -> Hasher.hash
(** Compute content-based hash for a blueprint. Takes into account package
    metadata, dependencies, source files, and actions. *)

(** {1 Display} *)

val string_of_action : action -> string
(** Pretty-print an action *)

val print_blueprint : blueprint -> unit
(** Print a blueprint to stdout *)
