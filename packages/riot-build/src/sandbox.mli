open Std
open Riot_model
open Riot_planner

(** A sandbox directory for building packages *)
type t

type dependency_prepare_stats = {
  dependency_count: int;
  object_count: int;
}

type dependency_prepare_error =
  | DependencyArtifactUnavailable of {
      package: Package_name.t;
      artifact_dir: Path.t;
      message: string;
    }
  | DependencyObjectMaterializeFailed of {
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

type materialize_stats = {
  copy_count: int;
  link_count: int;
  reference_count: int;
}

type materialize_error =
  | SandboxFileMaterializeFailed of {
      mode: Riot_planner.Sandbox_file.mode;
      src: Path.t;
      dst: Path.t;
      message: string;
    }

type prepare_error =
  | InputCopyFailed of { message: string }
  | DependencyPreparationFailed of dependency_prepare_error
  | SandboxMaterializationFailed of materialize_error

val dependency_prepare_error_to_string: dependency_prepare_error -> string

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

(** Materialize planner-selected sandbox files. *)
val materialize_files:
  sandbox:t ->
  files:Riot_planner.Sandbox_file.t list ->
  (materialize_stats, materialize_error) result

(** Materialize dependency native objects needed by linker actions. *)
val materialize_dependency_objects:
  store:Riot_store.Store.t ->
  sandbox:t ->
  package:Package.t ->
  depset:Dependency.t list ->
  (dependency_prepare_stats, dependency_prepare_error) result

(**
   Prepare an existing sandbox by copying package inputs and validating
   dependency artifacts. Dependency include/library outputs are referenced from
   the store, while native object basenames are linked into the sandbox for
   `ocamlopt`/linker compatibility.
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
