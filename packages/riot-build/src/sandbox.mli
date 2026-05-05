open Std
open Riot_model
open Riot_planner

(** A sandbox directory for building packages *)
type t

type dependency_copy_stats = {
  dependency_count: int;
  object_count: int;
}

type dependency_copy_error =
  | DependencyArtifactDirUnavailable of {
      package: Package_name.t;
      artifact_dir: Path.t;
      message: string;
    }
  | DependencyObjectCopyFailed of {
      package: Package_name.t;
      src: Path.t;
      dst: Path.t;
      message: string;
    }

type prepare_stats = {
  input_count: int;
  dependency_count: int;
  dependency_object_count: int;
}

type prepare_error =
  | InputCopyFailed of { message: string }
  | DependencyCopyFailed of dependency_copy_error

val dependency_copy_error_to_string: dependency_copy_error -> string

val prepare_error_to_string: prepare_error -> string

(** Create a sandbox directory for a package build. *)
val create:
  workspace:Workspace.t ->
  (** Hash-derived seed for isolating sandboxes of the same package. *)
  ?id_seed:Crypto.hash ->
  (** Build session id mixed into seeded sandbox ids to avoid stale reuse across invocations. *)
  ?session_id:Session_id.t ->
  ?profile:string ->
  ?target:Target.t ->
  unit ->
  package_name:Package_name.t ->
  t

(** Copy package source inputs into a sandbox. *)
val copy_inputs: sandbox:t -> package:Package.t -> inputs:Path.t list -> int

(** Copy dependency object files required by linker actions into a sandbox. *)
val copy_dependency_object_files:
  store:Riot_store.Store.t ->
  sandbox:t ->
  package:Package.t ->
  depset:Dependency.t list ->
  (dependency_copy_stats, dependency_copy_error) result

(**
   Prepare an existing sandbox by copying package inputs and dependency object
   files required by the current execution model.
*)
val prepare:
  sandbox:t ->
  package:Package.t ->
  inputs:Path.t list ->
  depset:Dependency.t list ->
  store:Riot_store.Store.t ->
  (prepare_stats, prepare_error) result

(** Get the directory path of the sandbox *)
val get_dir: t -> Path.t

(** Remove sandbox files from disk. *)
val cleanup: t -> unit

(**
   Execute a function with a prepared sandbox.

   Creates a sandbox, copies inputs and object files, executes the function,
   and returns the result.

   @param workspace The workspace
   @param package The package being built
   @param inputs List of input file paths (relative to package)
   @param depset List of dependencies
   @param store The artifact store
   @param expected_outputs List of expected output paths (unused currently)
   @param f Function to execute with the sandbox
*)
val with_sandbox:
  workspace:Workspace.t ->
  (** Hash-derived seed for isolating sandboxes of the same package. *)
  ?id_seed:Crypto.hash ->
  (** Build session id mixed into seeded sandbox ids to avoid stale reuse across invocations. *)
  ?session_id:Session_id.t ->
  ?profile:string ->
  ?target:Target.t ->
  package:Package.t ->
  inputs:Path.t list ->
  depset:Dependency.t list ->
  store:Riot_store.Store.t ->
  expected_outputs:Path.t list ->
  (t -> 'a) ->
  'a
